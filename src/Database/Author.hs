{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Database.Author
Description : Turning authored activity descriptions into database rows

Importing a database is tolerant: a supplier link that resolves to nothing is
warned about and dropped ('Database.MatrixBuild.findProducer'), because the
alternative — refusing a 20 000-dataset file over one bad row — helps nobody.
Authoring is the opposite situation. The author is present, the batch is small,
and every defect is fixable on the spot, so this module refuses instead of
repairing: an unresolvable supplier, an impossible unit conversion, a
non-finite amount and an empty name are all 'Left'. Nothing is guessed, and
nothing is silently dropped.

Identity is deterministic. An authored activity is addressed by
@(activityUUID, productUUID)@ like every other process, and both halves are
UUID5-minted from what the author wrote (see 'authoredNamespace'), never from
a counter or a clock. Authoring the same description twice therefore yields
the same key — which is what makes "write this activity again with one number
changed" a replace rather than a silent duplicate.

The product UUID keys on name *and* unit. Two activities that both produce
@milk@ in @kg@ share one product flow, exactly as a database with several
producers of the same product does; @milk@ in @kg@ and @milk@ in @l@ are two
flows, because they are. That makes "one flow UUID carrying two different
units" unrepresentable rather than merely unlikely.

Scope: one reference product per authored activity. Coproducts and allocation
are a later phase, and the types here do not pretend to support them — an
'AuthoredActivity' has exactly one product field group, not a list.
-}
module Database.Author (
    -- * What an author writes
    AuthoredActivity (..),
    AuthoredExchange (..),
    FlowRef (..),
    AuthorContext (..),

    -- * What the database can take
    ResolvedInsert (..),
    validateAuthored,

    -- * Deterministic identity
    authoredNamespace,
    authoredActivityUUID,
    authoredProductUUID,
    authoredBioFlowUUID,
) where

import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V

import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    ProcessId,
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    UnitDB,
    declaredLocationSource,
    exchangeIsInput,
    exchangeIsReference,
    findProcessIdByActivityUUID,
    getUnitNameForExchange,
    parseUUIDPair,
 )
import UnitConversion (UnitConfig, convertUnit, normalizeUnit)

-- ---------------------------------------------------------------------------
-- Deterministic identity
-- ---------------------------------------------------------------------------

{- | UUID5 namespace for everything an author creates, alongside the
per-format namespaces each parser owns ('SimaPro.Parser.simaproNamespace',
@ecospold1Namespace@). A separate namespace is what keeps an authored
@wheat, at farm/FR@ from colliding with an imported one of the same name: the
two are different datasets and must stay different rows.
-}
authoredNamespace :: UUID
authoredNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "volca:authored")

{- | Fields are joined with NUL before hashing: an author may write any
printable character in a name, so a printable separator would let two
different descriptions mint one key ("wheat @farm" in "FR" versus "wheat"
in "farm@FR").
-}
mintAuthored :: [Text] -> UUID
mintAuthored = UUID5.generateNamed authoredNamespace . BS.unpack . TE.encodeUtf8 . T.intercalate "\0"

-- | Activity half of the process key: what the activity does, and where.
authoredActivityUUID :: Text -> Text -> UUID
authoredActivityUUID name location = mintAuthored ["activity", name, location]

{- | Product half of the process key. Keyed on unit as well as name — see the
module header on why @milk@ in @kg@ and @milk@ in @l@ are two flows.
-}
authoredProductUUID :: Text -> Text -> UUID
authoredProductUUID productName unit = mintAuthored ["product", productName, unit]

{- | Identity of a biosphere flow an author introduces. Same three coordinates
the SimaPro parser mints on (name, compartment, unit), so re-declaring the
same flow in a second activity reuses the first one instead of forking it.
-}
authoredBioFlowUUID :: Text -> Compartment -> Text -> UUID
authoredBioFlowUUID name comp unit =
    mintAuthored ["flow", name, compartmentName comp, fromMaybe "" (compartmentSub comp), unit]

-- ---------------------------------------------------------------------------
-- What an author writes
-- ---------------------------------------------------------------------------

{- | The flow a biosphere exchange names: one already in the vocabulary, or one
the author is introducing.

There is deliberately no technosphere counterpart. A technosphere input always
names a *supplier* — a product that something in scope produces — so the flow
is whatever that supplier's process key already says it is. The only new
technosphere flow authoring can create is the authored activity's own product,
and that one is minted from the activity itself, never stated as an input.
-}
data FlowRef
    = ExistingFlow UUID
    | -- | name, compartment, unit
      NewBioFlow Text Compartment Text

{- | One line of an authored activity's inventory.

@ati*@ / @aw*@ carry a @process_id@ in the same currency the API and the UI
speak (@activityUUID_productUUID@, or a bare activity UUID when the activity
has a single product), never a matrix index — those renumber on every edit.

'AuthoredWasteOutput' is a waste this activity generates and hands to a
treatment process: the provider is the treatment activity, exactly as a
technosphere input's provider is the producer. The two resolve identically;
only the direction differs.
-}
data AuthoredExchange
    = AuthoredTechInput
        { atiProvider :: Text
        , atiAmount :: Double
        , atiUnit :: Maybe Text
        , atiComment :: Maybe Text
        }
    | AuthoredBio
        { abFlow :: FlowRef
        , abDirection :: BioDirection
        , abAmount :: Double
        , abUnit :: Maybe Text
        , abComment :: Maybe Text
        }
    | AuthoredWasteOutput
        { awProvider :: Text
        , awAmount :: Double
        , awUnit :: Maybe Text
        , awComment :: Maybe Text
        }

-- | A complete activity as an author states it: what it is, and what it exchanges.
data AuthoredActivity = AuthoredActivity
    { aaName :: Text
    , aaLocation :: Text
    , aaDescription :: [Text]
    , aaProductName :: Text
    , aaProductAmount :: Double
    , aaProductUnit :: Text
    , aaExchanges :: [AuthoredExchange]
    }

{- | Everything resolution needs to read: the database being edited, the
databases it may draw suppliers from, and the unit table conversions are
judged against.
-}
data AuthorContext = AuthorContext
    { acDb :: Database
    , acDeps :: [Database]
    , acUnitConfig :: UnitConfig
    }

{- | An authored activity that the database can accept, with the vocabulary it
brings along. The caller inserts the flows and the activity together —
'Database.Edit.insertActivities' does — so the activity never lands referring
to a flow nothing declares.
-}
data ResolvedInsert = ResolvedInsert
    { riKey :: (UUID, UUID)
    , riActivity :: Activity
    , riNewTechFlows :: [TechnosphereFlow]
    , riNewBioFlows :: [BiosphereFlow]
    }

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

{- | Resolve a batch of authored activities, or report everything wrong with it.

Errors accumulate across the whole batch and across every exchange of every
activity: an author fixing a ten-line inventory sees ten complaints once, not
one complaint ten times. Each message names the activity it belongs to.

Warnings ('snd' of the success case) never block. Today the only one is a
biosphere flow new to the database, which by construction no characterization
factor matches by UUID — it may still be reached by name, so it is a caution
and not a refusal.

Whether the key may already exist is not decided here: this function does not
know if the caller means to insert or to replace.
'Database.Edit.insertActivities' and 'Database.Edit.replaceActivities' own
that check, each in the direction it means. What *is* checked here is that the
batch does not mint one key twice, since no intent makes that coherent.
-}
validateAuthored :: AuthorContext -> [AuthoredActivity] -> Either [Text] ([ResolvedInsert], [Text])
validateAuthored ctx activities =
    case partitionEithers (map (validateOne ctx) activities) of
        ([], oks) ->
            let inserts = map fst oks
             in case duplicateKeys (map riKey inserts) of
                    [] -> Right (inserts, concatMap snd oks)
                    dups -> Left (map duplicateMessage dups)
        (errs, _) -> Left (concat errs)
  where
    duplicateMessage (a, p) =
        "Two activities in this batch mint the same identity ("
            <> UUID.toText a
            <> "_"
            <> UUID.toText p
            <> "): same name, location, product and unit."

duplicateKeys :: [(UUID, UUID)] -> [(UUID, UUID)]
duplicateKeys keys = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(k, 1) | k <- keys]))

validateOne :: AuthorContext -> AuthoredActivity -> Either [Text] (ResolvedInsert, [Text])
validateOne ctx a =
    case (productChecks, partitionEithers (map resolveExchange (aaExchanges a)), mProductUnit) of
        ([], ([], resolved), Just (unitRef, unitLabel)) ->
            let key = (authoredActivityUUID (aaName a) (aaLocation a), authoredProductUUID (aaProductName a) unitLabel)
                productExchange =
                    TechnosphereExchange
                        { techFlowId = snd key
                        , techAmount = aaProductAmount a
                        , techUnitId = unitRef
                        , techRole = ReferenceProduct
                        , -- Self-link, as every loaded database records its
                          -- reference products.
                          techActivityLinkId = fst key
                        , techProcessLinkId = Nothing
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        }
                productFlow =
                    TechnosphereFlow
                        { tfId = snd key
                        , tfName = aaProductName a
                        , tfUnitId = unitRef
                        , tfSynonyms = M.empty
                        , tfCAS = Nothing
                        , tfSubstanceId = Nothing
                        }
             in Right
                    ( ResolvedInsert
                        { riKey = key
                        , riActivity = buildActivity a unitLabel (productExchange : map reExchange resolved)
                        , riNewTechFlows = [productFlow | not (M.member (snd key) (dbTechFlows (acDb ctx)))]
                        , riNewBioFlows = mapMaybe reNewBioFlow resolved
                        }
                    , map here (concatMap reWarnings resolved)
                    )
        (perActivityErrs, (exchangeErrs, _), _) -> Left (map here (perActivityErrs <> concat exchangeErrs))
  where
    here msg = aaName a <> " {" <> aaLocation a <> "}: " <> msg
    resolveExchange ex = mapLeft (map ((describeExchange ex <> ": ") <>)) (resolveOne ctx ex)
    mProductUnit = lookupUnit ctx (aaProductUnit a)
    productChecks =
        concat
            [ ["the activity name is empty" | T.null (T.strip (aaName a))]
            , ["the product name is empty" | T.null (T.strip (aaProductName a))]
            , [ "the product amount is " <> T.pack (show (aaProductAmount a)) <> "; it must be a finite non-zero number"
              | not (isUsableAmount (aaProductAmount a))
              ]
            , [ "unknown unit \"" <> aaProductUnit a <> "\" for the product"
              | Nothing <- [mProductUnit]
              ]
            ]

buildActivity :: AuthoredActivity -> Text -> [Exchange] -> Activity
buildActivity a unitLabel exchangeList =
    Activity
        { activityName = aaName a
        , activityDescription = aaDescription a
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = aaLocation a
        , activityLocationSource = declaredLocationSource (aaLocation a)
        , activityUnit = unitLabel
        , exchanges = exchangeList
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

{- | How a complaint names the line it is about. What an author can act on is
which supplier or which flow, not a position in a list — an API caller may not
even have sent the exchanges as one list.
-}
describeExchange :: AuthoredExchange -> Text
describeExchange ex = case ex of
    AuthoredTechInput{atiProvider = provider} -> "input from \"" <> provider <> "\""
    AuthoredWasteOutput{awProvider = provider} -> "waste output to \"" <> provider <> "\""
    AuthoredBio{abFlow = ExistingFlow flowId} -> "biosphere flow " <> UUID.toText flowId
    AuthoredBio{abFlow = NewBioFlow name _ _} -> "biosphere flow \"" <> name <> "\""

{- | An amount that can carry information: finite, and not zero. A zero
exchange is not a measurement of nothing, it is a line that should not have
been written — and it would divide into a zero normalization factor.
-}
isUsableAmount :: Double -> Bool
isUsableAmount x = not (isNaN x) && not (isInfinite x) && x /= 0

-- ---------------------------------------------------------------------------
-- Exchange resolution
-- ---------------------------------------------------------------------------

-- | One resolved inventory line, plus whatever it drags into the vocabulary.
data ResolvedExchange = ResolvedExchange
    { reExchange :: Exchange
    , reNewBioFlow :: Maybe BiosphereFlow
    , reWarnings :: [Text]
    }

resolveOne :: AuthorContext -> AuthoredExchange -> Either [Text] ResolvedExchange
resolveOne ctx ex = case ex of
    AuthoredTechInput provider amount mUnit comment ->
        resolveLinked ctx provider amount mUnit $ \sup unitRef ->
            TechnosphereExchange
                { techFlowId = snd (supKey sup)
                , techAmount = amount
                , techUnitId = unitRef
                , techRole = Input
                , techActivityLinkId = fst (supKey sup)
                , techProcessLinkId = Nothing
                , techLocation = ""
                , techComment = comment
                , techPedigree = Nothing
                }
    AuthoredWasteOutput provider amount mUnit comment ->
        resolveLinked ctx provider amount mUnit $ \sup unitRef ->
            WasteExchange
                { waFlowId = snd (supKey sup)
                , waAmount = amount
                , waUnitId = unitRef
                , waIsInput = False
                , waActivityLinkId = fst (supKey sup)
                , waProcessLinkId = Nothing
                , waLocation = ""
                , waComment = comment
                , waPedigree = Nothing
                }
    AuthoredBio flowRef direction amount mUnit comment ->
        resolveBio ctx flowRef direction amount mUnit comment

{- | A technosphere input and a waste output differ only in the constructor
they build: both name a provider by process id, both take their flow from that
provider's process key, and both convert into that provider's reference unit.
-}
resolveLinked ::
    AuthorContext ->
    Text ->
    Double ->
    Maybe Text ->
    (Supplier -> UUID -> Exchange) ->
    Either [Text] ResolvedExchange
resolveLinked ctx provider amount mUnit build =
    case (amountCheck, resolveSupplier ctx provider) of
        ([], Right sup) ->
            let stated = fromMaybe (defaultUnit sup) mUnit
             in case lookupUnit ctx stated of
                    Nothing -> Left ["unknown unit \"" <> stated <> "\""]
                    Just (unitRef, unitLabel) ->
                        case unitError ctx sup unitLabel of
                            Just err -> Left [err]
                            Nothing -> Right (ResolvedExchange (build sup unitRef) Nothing [])
        (errs, Right _) -> Left errs
        (errs, Left err) -> Left (errs <> [err])
  where
    amountCheck =
        [ "the amount is " <> T.pack (show amount) <> "; it must be a finite non-zero number"
        | not (isUsableAmount amount)
        ]
    defaultUnit sup =
        case (supProducedUnit sup, supAnyRefUnit sup) of
            ("", "") -> ""
            ("", other) -> other
            (produced, _) -> produced

{- | Judge the unit an amount is stated in against what the matrix will do
with it. A provider with a produced reference output gets the conversion
check; a provider whose only reference is an input (a treatment process) gets
an exact-match rule instead, because 'Database.MatrixBuild.techTriple' never
converts into a reference input — a mismatched unit would land as a wrong raw
number, not as a conversion.
-}
unitError :: AuthorContext -> Supplier -> Text -> Maybe Text
unitError ctx sup stated
    | T.null (supProducedUnit sup)
    , not (T.null (supAnyRefUnit sup))
    , normalizeUnit stated /= normalizeUnit (supAnyRefUnit sup) =
        Just $
            "the provider's reference is stated in \""
                <> supAnyRefUnit sup
                <> "\" but the exchange states \""
                <> stated
                <> "\"; amounts to a provider with no produced output are not converted, so restate it in \""
                <> supAnyRefUnit sup
                <> "\""
    | otherwise = conversionError ctx stated (supProducedUnit sup)

{- | The unit a value must be stated in before it can enter the technosphere
matrix. Mirrors 'Database.MatrixBuild.techTriple' exactly — same emptiness
guards, same conversion table — so a batch that validates here cannot fail the
rebuild afterwards for a reason authoring could have named first.
-}
conversionError :: AuthorContext -> Text -> Text -> Maybe Text
conversionError ctx stated supplierUnit
    | not needsConversion = Nothing
    | otherwise = case convertUnit (acUnitConfig ctx) stated supplierUnit 1 of
        Just _ -> Nothing
        Nothing ->
            Just $
                "cannot convert \""
                    <> stated
                    <> "\" into the supplier's \""
                    <> supplierUnit
                    <> "\""
  where
    needsConversion =
        normalizeUnit stated /= normalizeUnit supplierUnit
            && not (T.null stated)
            && not (T.null supplierUnit)

resolveBio ::
    AuthorContext ->
    FlowRef ->
    BioDirection ->
    Double ->
    Maybe Text ->
    Maybe Text ->
    Either [Text] ResolvedExchange
resolveBio ctx flowRef direction amount mUnit comment
    | not (isUsableAmount amount) =
        Left ["the amount is " <> T.pack (show amount) <> "; it must be a finite non-zero number"]
    | otherwise = case flowRef of
        ExistingFlow flowId -> case findBioFlow ctx flowId of
            Nothing ->
                Left
                    [ "no biosphere flow "
                        <> UUID.toText flowId
                        <> " in this database or its dependencies"
                    ]
            Just (flow, ownerUnits, local) ->
                let flowUnit = unitNameOf ownerUnits (bfUnitId flow)
                 in case mUnit of
                        Just stated | normalizeUnit stated /= normalizeUnit flowUnit -> Left [mismatch flow stated flowUnit]
                        _ -> emitKnown flowId flow flowUnit local
        NewBioFlow name comp unit -> case lookupUnit ctx unit of
            Nothing -> Left ["unknown unit \"" <> unit <> "\" for flow \"" <> name <> "\""]
            Just (unitRef, unitLabel) ->
                let flowId = authoredBioFlowUUID name comp unitLabel
                 in case findBioFlow ctx flowId of
                        Just (flow, ownerUnits, local) -> emitKnown flowId flow (unitNameOf ownerUnits (bfUnitId flow)) local
                        Nothing ->
                            emit
                                flowId
                                unitRef
                                (Just (newBioFlow flowId name comp unitRef))
                                [ "biosphere flow \""
                                    <> name
                                    <> "\" is new to this database; no characterization factor matches it by identity yet"
                                ]
  where
    -- A flow found in a dependency is copied into the edited database with
    -- its unit remapped: characterization resolves flows through the edited
    -- database's own vocabulary ('dbBioFlows'), so an exchange must never
    -- reference a flow only a dependency declares.
    emitKnown flowId flow flowUnit local
        | local = emit flowId (bfUnitId flow) Nothing []
        | otherwise = case lookupUnit ctx flowUnit of
            Nothing ->
                Left
                    [ "biosphere flow \""
                        <> bfName flow
                        <> "\" from a dependency is stated in \""
                        <> flowUnit
                        <> "\", a unit this database does not have"
                    ]
            Just (unitRef, _) -> emit flowId unitRef (Just flow{bfUnitId = unitRef}) []
    emit flowId unitRef mNew warnings =
        Right
            ( ResolvedExchange
                BiosphereExchange
                    { bioFlowId = flowId
                    , bioAmount = amount
                    , bioUnitId = unitRef
                    , bioDirection = direction
                    , bioLocation = ""
                    , bioComment = comment
                    , bioPedigree = Nothing
                    }
                mNew
                warnings
            )
    -- The biosphere matrix carries amounts through unconverted, so a unit the
    -- flow does not use would land as a wrong number rather than as a
    -- conversion. Refuse and name both units.
    mismatch flow stated flowUnit =
        "biosphere flow \""
            <> bfName flow
            <> "\" is recorded in \""
            <> flowUnit
            <> "\" but the exchange states \""
            <> stated
            <> "\"; biosphere amounts are not converted, so restate it in \""
            <> flowUnit
            <> "\""

newBioFlow :: UUID -> Text -> Compartment -> UUID -> BiosphereFlow
newBioFlow flowId name comp unitRef =
    BiosphereFlow
        { bfId = flowId
        , bfName = name
        , bfUnitId = unitRef
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just comp
        }

-- ---------------------------------------------------------------------------
-- Lookups across the edited database and its dependencies
-- ---------------------------------------------------------------------------

{- | A resolved provider. Only its @(activityUUID, productUUID)@ key goes into
the exchange, never a 'ProcessId': process ids renumber on every rebuild, so
an embedded one would silently point at whichever row inherits the number.
'Database.MatrixBuild.findProducer' resolves the pair, and a provider living
in a dependency resolves the same way through cross-database relinking.
-}
data Supplier = Supplier
    { supKey :: (UUID, UUID)
    , supProducedUnit :: Text
    {- ^ unit of the produced reference output, @""@ for a treatment process
    that has only a reference input. Mirrors
    'Database.MatrixBuild.buildSupplierRefUnits', which is what the matrix
    converts into.
    -}
    , supAnyRefUnit :: Text
    {- ^ unit of the first reference exchange either way, used only to default
    an omitted unit — a treatment provider has no produced unit to borrow.
    -}
    }

resolveSupplier :: AuthorContext -> Text -> Either Text Supplier
resolveSupplier ctx provider =
    case mapMaybe inDatabase (acDb ctx : acDeps ctx) of
        [] -> Left ("unknown provider \"" <> provider <> "\"")
        (sup : _) -> Right sup
  where
    inDatabase db = do
        pid <- resolveProcess db provider
        key <- dbProcessIdTable db V.!? fromIntegral pid
        act <- dbActivities db V.!? fromIntegral pid
        pure
            Supplier
                { supKey = key
                , supProducedUnit = refUnitOf db act (\e -> exchangeIsReference e && not (exchangeIsInput e))
                , supAnyRefUnit = refUnitOf db act exchangeIsReference
                }

{- | Resolve a process id string the same two ways the rest of the engine
does: the canonical @activityUUID_productUUID@ pair, or a bare activity UUID
when that activity has a single product.
-}
resolveProcess :: Database -> Text -> Maybe ProcessId
resolveProcess db queryText = case parseUUIDPair queryText of
    Just (a, p) -> M.lookup (a, p) (dbProcessIdLookup db)
    Nothing -> UUID.fromText queryText >>= findProcessIdByActivityUUID db

refUnitOf :: Database -> Activity -> (Exchange -> Bool) -> Text
refUnitOf db act keep = case filter keep (exchanges act) of
    (ex : _) -> getUnitNameForExchange (dbUnits db) ex
    [] -> ""

{- | Find a biosphere flow, with the unit table that can name its unit and
whether it lives in the edited database itself.
-}
findBioFlow :: AuthorContext -> UUID -> Maybe (BiosphereFlow, UnitDB, Bool)
findBioFlow ctx flowId =
    case mapMaybe look (zip (True : repeat False) (acDb ctx : acDeps ctx)) of
        [] -> Nothing
        (found : _) -> Just found
  where
    look (local, db) = (,dbUnits db,local) <$> M.lookup flowId (dbBioFlows db)

{- | Resolve a unit the author names to the identifier an exchange carries,
plus the canonical name of that unit. Names and symbols both resolve, so
@kilogram@ and @kg@ reach the same row; a name wins over a symbol when a
database happens to use one string for both.
-}
lookupUnit :: AuthorContext -> Text -> Maybe (UUID, Text)
lookupUnit ctx stated = M.lookup (normalizeUnit stated) (unitIndex (dbUnits (acDb ctx)))

unitIndex :: UnitDB -> M.Map Text (UUID, Text)
unitIndex units =
    M.fromList
        [ (normalizeUnit key, (unitId u, unitName u))
        | u <- M.elems units
        , key <- [unitSymbol u, unitName u]
        , not (T.null (T.strip key))
        ]

unitNameOf :: UnitDB -> UUID -> Text
unitNameOf units uid = maybe "" unitName (M.lookup uid units)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
