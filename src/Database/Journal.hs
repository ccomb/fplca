{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.Journal
Description : The record of what was edited, and how to apply it again

An uploaded database is what its author uploaded. Editing it never rewrites
those files: the edits are appended to a journal beside them, and loading the
database means reading the sources (or the matrix cache) and then replaying
that journal over them.

The alternative — rewriting the sources in their own format after every edit —
only works for a format that records process identity in the files themselves.
EcoSpold 1 does not: its flow identifiers are derived from the position of a
dataset in the file, and any writer that renumbers them moves every identity
the next time the file is read. Leaving the sources alone sidesteps the whole
question, because a file that is never rewritten parses to the same
identifiers forever.

What a line records is a verb, not a result: the activities as their author
described them, or the process ids removed. Replaying them re-derives
everything else, so the journal stays small and readable, and the difference
between a published database and the one in use is exactly its journal.

The file is @journal.jsonl@ in the database's upload directory, one JSON
object per line:

> {"v":1,"at":"2026-08-03T09:12:41Z","op":"create","activities":[…],"written":["8f3c…_a1b2…"]}
> {"v":1,"at":"2026-08-03T09:14:02Z","op":"replace","target":"8f3c…_a1b2…","activity":{…}}
> {"v":1,"at":"2026-08-03T09:15:30Z","op":"delete","targets":["3e7a…_c4d5…"]}

Two things guard the replay.

The first is @written@ (and @target@, which serves the same purpose for a
replace): the process ids the edit produced when it was made. Identity is
minted from what the author wrote, so replaying re-mints it; if the two ever
disagree, the engine's minting has changed under a journal written by an older
one, and the replay stops there instead of silently landing the activity under
a different identity.

The second is the version on every line. A line this engine cannot read is
refused, never skipped.

The last line is the exception, and only when it is the last: a line is
written and flushed before its edit is acknowledged, so a torn final line
belongs to an edit no caller was ever told had happened. It is dropped with a
warning. A line that fails to parse anywhere else refuses the whole journal.
-}
module Database.Journal (
    -- * What a journal records
    JournalEvent (..),
    JournalOp (..),

    -- * The file
    journalPath,
    appendEvent,
    readJournal,

    -- * Applying it
    replayJournal,
) where

import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    Value,
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode), withFile)

import Database.Author (
    AuthorContext (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    FlowRef (..),
    ResolvedInsert (..),
    validateAuthored,
 )
import Database.Rebuild (
    deleteActivitiesWith,
    insertActivities,
    renderKey,
    replaceActivities,
    resolveProcess,
 )
import Progress (ProgressLevel (..), reportProgress)
import Types (BioDirection (..), Compartment (..), Database)

-- ---------------------------------------------------------------------------
-- What a journal records
-- ---------------------------------------------------------------------------

{- | One line of the journal: what was done, and when. The timestamp is
provenance for whoever reads the file; the replay never looks at it.
-}
data JournalEvent = JournalEvent
    { jeAt :: Text
    , jeOp :: JournalOp
    }
    deriving (Eq, Show)

{- | The three ways a database changes. Each carries the identity the edit
produced, so a replay can check that it still produces it.
-}
data JournalOp
    = -- | Activities added, and the process ids they minted.
      Created [AuthoredActivity] [Text]
    | {- | The process rewritten, and the description written over it. The
      target is the identity the new description has to keep.
      -}
      Replaced Text AuthoredActivity
    | -- | The process ids removed.
      Deleted [Text]
    deriving (Eq, Show)

-- | The version this engine writes, and the only one it reads.
journalVersion :: Int
journalVersion = 1

-- | A database's journal, given the upload directory that database lives in.
journalPath :: FilePath -> FilePath
journalPath home = home </> "journal.jsonl"

-- ---------------------------------------------------------------------------
-- The file
-- ---------------------------------------------------------------------------

{- | Record an edit. Appends one line and closes the file before returning, so
an edit is on disk by the time its caller answers.

Stamps the line with the current time, which is why this takes the operation
rather than a whole event: when it happened is the journal's business, not its
caller's.
-}
appendEvent :: FilePath -> JournalOp -> IO (Either Text ())
appendEvent home op = do
    now <- getCurrentTime
    let event = JournalEvent{jeAt = T.pack (iso8601Show now), jeOp = op}
    written <- try $ do
        createDirectoryIfMissing True home
        withFile (journalPath home) AppendMode $ \handle ->
            BL.hPut handle (encode event <> "\n")
    pure $ case written of
        Right () -> Right ()
        Left (err :: SomeException) ->
            Left $
                "could not record the edit in "
                    <> T.pack (journalPath home)
                    <> ": "
                    <> T.pack (show err)

{- | Read a database's journal. A database with no journal has made no edits,
which is not an error.

A torn last line is dropped with a warning (see the module header); anything
else unreadable refuses the whole file, naming the line.
-}
readJournal :: FilePath -> IO (Either Text [JournalEvent])
readJournal home = do
    let path = journalPath home
    exists <- doesFileExist path
    if not exists
        then pure (Right [])
        else
            try (BS.readFile path) >>= \case
                Left (err :: SomeException) ->
                    pure $ Left $ "could not read " <> T.pack path <> ": " <> T.pack (show err)
                Right bytes -> decodeLines path bytes

{- | What a line turned out to be.

The distinction is what keeps the last-line exception honest. A line that is
not JSON at all is a write that was cut short. A line that is complete JSON
says something definite, and if this engine cannot read what it says — a
version it does not know, an operation it has no verb for — that is a refusal
wherever the line sits, including at the end. Otherwise a newer engine's
entries, which are exactly the ones at the end of the file, would be dropped
as debris.
-}
data LineProblem
    = Torn String
    | Unreadable String

decodeLines :: FilePath -> BS.ByteString -> IO (Either Text [JournalEvent])
decodeLines path bytes =
    case problems of
        [] -> pure (Right events)
        [(i, Torn err)] | i == lastLine -> do
            reportProgress Warning $
                "The last line of "
                    <> path
                    <> " is incomplete and was dropped ("
                    <> err
                    <> "). A line is written before its edit is acknowledged, so no\
                       \ edit anyone was told about is lost."
            pure (Right events)
        ((i, problem) : _) -> pure (Left (situate i problem))
  where
    numbered =
        [ (i, line)
        | (i, line) <- zip [1 :: Int ..] (BS.lines bytes)
        , not (BS.all isSpace line)
        ]
    results = [(i, readLine line) | (i, line) <- numbered]
    events = [event | (_, Right event) <- results]
    problems = [(i, problem) | (i, Left problem) <- results]
    lastLine = length numbered
    situate i problem =
        T.pack path <> " line " <> T.pack (show i) <> " " <> case problem of
            Torn err -> "is not complete JSON: " <> T.pack err
            Unreadable err -> "is not an entry this engine reads: " <> T.pack err

readLine :: BS.ByteString -> Either LineProblem JournalEvent
readLine line = case eitherDecodeStrict line of
    Left err -> Left (Torn err)
    Right value -> first Unreadable (parseEither parseJSON value)

-- ---------------------------------------------------------------------------
-- Applying it
-- ---------------------------------------------------------------------------

{- | Replay a journal over the database it belongs to, one line at a time, each
line seeing the result of the ones before it.

The context carries the database to start from ('acDb'), the dependencies
suppliers are resolved against, and the units amounts are judged in — the same
context authoring uses, because a replay validates exactly what authoring
validated. That is deliberate: a supplier that has since vanished from a
dependency fails here rather than landing as a dangling link.

Any failure refuses the whole database, naming the line and the reason. A
half-applied journal would be a database that silently disagrees with its own
record.

Warnings raised during validation are dropped: they were reported to the
author when the edit was made, and repeating them at every load says nothing
new.
-}
replayJournal :: AuthorContext -> [JournalEvent] -> Either Text Database
replayJournal ctx = foldM step (acDb ctx) . zip [1 :: Int ..]
  where
    step db (i, event) =
        first (situate i (jeOp event)) (applyOp ctx{acDb = db} (jeOp event))
    situate i op msg =
        "journal event " <> T.pack (show i) <> " (" <> opName op <> "): " <> msg

applyOp :: AuthorContext -> JournalOp -> Either Text Database
applyOp ctx = \case
    Created activities written -> do
        resolved <- resolve activities
        let minted = map (renderKey . riKey) resolved
        if minted == written
            then insertActivities (acUnitConfig ctx) resolved (acDb ctx)
            else Left (drift written minted)
    Replaced target activity -> do
        resolved <- resolve [activity]
        case map (renderKey . riKey) resolved of
            [minted]
                | minted == target -> replaceActivities (acUnitConfig ctx) resolved (acDb ctx)
            others -> Left (drift [target] others)
    Deleted targets -> do
        pids <- traverse (resolveProcess (acDb ctx)) targets
        deleteActivitiesWith (acUnitConfig ctx) pids (acDb ctx)
  where
    resolve = bimap (T.intercalate "; ") fst . validateAuthored ctx

{- | The one failure a journal exists to make impossible to miss: the same
description no longer minting the identity it was recorded under. Everything
that points at a process id — a script, a saved score, another database's
links — would follow the old one.
-}
drift :: [Text] -> [Text] -> Text
drift recorded minted =
    "recorded as "
        <> render recorded
        <> " but the same description now mints "
        <> render minted
        <> ". Identity comes from the name, location, product and unit, so this\
           \ journal cannot be replayed by an engine that mints differently."
  where
    render [] = "nothing"
    render keys = T.intercalate ", " keys

opName :: JournalOp -> Text
opName = \case
    Created _ _ -> "create"
    Replaced _ _ -> "replace"
    Deleted _ -> "delete"

-- ---------------------------------------------------------------------------
-- Codec
--
-- Hand-written, and the journal's own: the wire types describing the same
-- edits ("API.Types") are free to change shape with the API, while what is
-- already on disk has to keep reading. The version on each line is what makes
-- that separation enforceable.
-- ---------------------------------------------------------------------------

instance ToJSON JournalEvent where
    toJSON event = object $ ["v" .= journalVersion, "at" .= jeAt event] <> opFields (jeOp event)

instance FromJSON JournalEvent where
    parseJSON = withObject "journal entry" $ \o -> do
        version <- o .: "v"
        if version /= journalVersion
            then
                fail $
                    "journal format version "
                        <> show (version :: Int)
                        <> ", but this engine reads version "
                        <> show journalVersion
            else JournalEvent <$> o .: "at" <*> parseOp o

opFields :: JournalOp -> [Pair]
opFields = \case
    Created activities written ->
        [ "op" .= ("create" :: Text)
        , "activities" .= map activityJSON activities
        , "written" .= written
        ]
    Replaced target activity ->
        ["op" .= ("replace" :: Text), "target" .= target, "activity" .= activityJSON activity]
    Deleted targets ->
        ["op" .= ("delete" :: Text), "targets" .= targets]

parseOp :: Object -> Parser JournalOp
parseOp o =
    o .: "op" >>= \case
        ("create" :: Text) ->
            Created <$> (o .: "activities" >>= traverse parseActivity) <*> o .: "written"
        "replace" ->
            Replaced <$> o .: "target" <*> (o .: "activity" >>= parseActivity)
        "delete" ->
            Deleted <$> o .: "targets"
        other -> fail ("unknown journal operation: " <> T.unpack other)

activityJSON :: AuthoredActivity -> Value
activityJSON a =
    object
        [ "name" .= aaName a
        , "location" .= aaLocation a
        , "description" .= aaDescription a
        , "product"
            .= object
                [ "name" .= aaProductName a
                , "amount" .= aaProductAmount a
                , "unit" .= aaProductUnit a
                ]
        , "exchanges" .= map exchangeJSON (aaExchanges a)
        ]

parseActivity :: Value -> Parser AuthoredActivity
parseActivity = withObject "authored activity" $ \o -> do
    prod <- o .: "product"
    name <- o .: "name"
    location <- o .: "location"
    description <- o .: "description"
    productName <- prod .: "name"
    productAmount <- prod .: "amount"
    productUnit <- prod .: "unit"
    exchanges <- o .: "exchanges" >>= traverse parseExchange
    pure
        AuthoredActivity
            { aaName = name
            , aaLocation = location
            , aaDescription = description
            , aaProductName = productName
            , aaProductAmount = productAmount
            , aaProductUnit = productUnit
            , aaExchanges = exchanges
            }

exchangeJSON :: AuthoredExchange -> Value
exchangeJSON = \case
    AuthoredTechInput provider amount unit comment ->
        object $ ["kind" .= ("input" :: Text), "provider" .= provider, "amount" .= amount] <> stated unit comment
    AuthoredWasteOutput provider amount unit comment ->
        object $ ["kind" .= ("waste" :: Text), "provider" .= provider, "amount" .= amount] <> stated unit comment
    AuthoredBio flow direction amount unit comment ->
        object $
            [ "kind" .= ("biosphere" :: Text)
            , "flow" .= flowJSON flow
            , "direction" .= directionText direction
            , "amount" .= amount
            ]
                <> stated unit comment
  where
    stated unit comment = optional "unit" unit <> optional "comment" comment
    optional key = maybe [] (\value -> [key .= (value :: Text)])

parseExchange :: Value -> Parser AuthoredExchange
parseExchange = withObject "exchange" $ \o ->
    o .: "kind" >>= \case
        ("input" :: Text) ->
            AuthoredTechInput <$> o .: "provider" <*> o .: "amount" <*> o .:? "unit" <*> o .:? "comment"
        "waste" ->
            AuthoredWasteOutput <$> o .: "provider" <*> o .: "amount" <*> o .:? "unit" <*> o .:? "comment"
        "biosphere" ->
            AuthoredBio
                <$> (o .: "flow" >>= parseFlow)
                <*> (o .: "direction" >>= parseDirection)
                <*> o .: "amount"
                <*> o .:? "unit"
                <*> o .:? "comment"
        other -> fail ("unknown exchange kind: " <> T.unpack other)

{- | A biosphere line names a flow the vocabulary already has, or introduces
one. The unit of an introduced flow is part of its identity, which is why it
lives inside the flow rather than beside it: the amount's own unit is a
separate field, and the two are not required to be the same thing.
-}
flowJSON :: FlowRef -> Value
flowJSON = \case
    ExistingFlow flowId -> object ["id" .= UUID.toText flowId]
    NewBioFlow name compartment unit ->
        object $
            ["name" .= name, "compartment" .= compartmentName compartment, "unit" .= unit]
                <> maybe [] (\sub -> ["sub_compartment" .= sub]) (compartmentSub compartment)

parseFlow :: Value -> Parser FlowRef
parseFlow = withObject "biosphere flow" $ \o -> do
    mIdentifier <- o .:? "id"
    mName <- o .:? "name"
    case (mIdentifier, mName) of
        (Just identifier, Nothing) ->
            maybe (fail ("not a flow identifier: " <> T.unpack identifier)) (pure . ExistingFlow) $
                UUID.fromText identifier
        (Nothing, Just name) -> do
            compartment <- o .: "compartment"
            sub <- o .:? "sub_compartment"
            unit <- o .: "unit"
            pure $
                NewBioFlow
                    name
                    Compartment{compartmentName = compartment, compartmentSub = sub}
                    unit
        (Just _, Just _) ->
            fail "a biosphere flow names either an existing flow or a new one, not both"
        (Nothing, Nothing) ->
            fail "a biosphere flow needs either an identifier or a name, compartment and unit"

directionText :: BioDirection -> Text
directionText = \case
    Resource -> "resource"
    Emission -> "emission"

parseDirection :: Text -> Parser BioDirection
parseDirection raw = case T.toLower (T.strip raw) of
    "resource" -> pure Resource
    "emission" -> pure Emission
    other -> fail ("unknown biosphere direction: " <> T.unpack other <> " (expected resource|emission)")
