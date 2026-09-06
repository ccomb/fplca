{-# LANGUAGE OverloadedStrings #-}

{- | Reaching a source block by the identifier its file gave it.

Someone with a SimaPro export open beside the engine has the block's
@Process identifier@ in front of them and nothing else that is certain: the
@Process name@ is truncated to 80 characters and reused across unrelated
blocks. Typing the identifier has to land on that block, all of it, and on
nothing that merely resembles it.
-}
module NativeIdSearchSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.Types (ActivitySummary (..))
import Database (buildDatabaseWithMatrices)
import Database.Loader (defaultLoadOptions, loadDatabaseWithLocationAliases)
import Service (ActivityFilterCore (..), SearchFilter (..), activityMatches, mkActivitySummary)
import Types (AllocationKey (..), BuildInputs (..), Database)
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "finding a source block by its identifier" $ do
    it "answers the whole block the identifier names, and only it" $
        withDatabase $ \db ->
            productsFound db "P-1" `shouldBe` ["Cheese", "Whey"]

    it "does not answer the block whose identifier only resembles it" $
        withDatabase $ \db ->
            productsFound db "P-2" `shouldBe` ["Slag", "Steel"]

    -- An identifier is read off a page and pasted, so it arrives with whatever
    -- the copy picked up, and SimaPro writes its own in upper case.
    it "reads an identifier pasted with its surrounding spaces, in any case" $
        withDatabase $ \db ->
            productsFound db "  p-1 " `shouldBe` ["Cheese", "Whey"]

    {- A fragment names no block, so the query is a query again: these two
    blocks share their name, so searching that name finds all four rows.
    Answering a fragment with the blocks whose identifier starts that way would
    answer half a database to someone who named one dataset, since the codes of
    one database share their prefix.
    -}
    it "falls back to the ordinary search for a query that names no block" $
        withDatabase $ \db ->
            productsFound db "One name for two blocks"
                `shouldBe` ["Cheese", "Slag", "Steel", "Whey"]

{- | The products a query brings back, sorted, through the funnel a listing and
its tab counter both use.
-}
productsFound :: Database -> Text -> [Text]
productsFound db query =
    sort [prsProductName (mkActivitySummary db pid act) | (pid, act) <- activityMatches db (SearchFilter (nameOnly query) False)]
  where
    nameOnly :: Text -> ActivityFilterCore
    nameOnly q =
        ActivityFilterCore
            { afcName = Just q
            , afcLocation = Nothing
            , afcProduct = Nothing
            , afcClassifications = []
            , afcLimit = Nothing
            , afcOffset = Nothing
            , afcSort = Nothing
            , afcOrder = Nothing
            }

-- | The two blocks, loaded and built the way a served database is.
withDatabase :: (Database -> IO ()) -> IO ()
withDatabase k = withSystemTempDirectory "native-id-search" $ \dir -> do
    let path = dir </> "source.csv"
    BS.writeFile path twoBlocksSharingAName
    loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) path
    simpleDb <- either (fail . T.unpack) pure loaded
    built <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) simpleDb
    either (fail . T.unpack) k built

{- | Two blocks written under one process name, each with two coproducts. Their
@Process identifier@ lines are all that tell them apart, and the two differ by
one character, which is what a fuzzy matcher would confuse them on.
-}
twoBlocksSharingAName :: BS.ByteString
twoBlocksSharingAName =
    simaProFile
        ( block "P-1" ["Cheese;kg;1;60;not defined;material;", "Whey;kg;3;40;not defined;material;"]
            ++ block "P-2" ["Steel;kg;1;70;not defined;material;", "Slag;kg;2;30;not defined;material;"]
        )

-- | One SimaPro process block under a fixed name, identified by @identifier@.
block :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
block identifier products =
    [ "Process"
    , ""
    , "Category type"
    , "material"
    , ""
    , "Process identifier"
    , identifier
    , ""
    , "Process name"
    , "One name for two blocks"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Geography"
    , "GLO"
    , ""
    , "Products"
    ]
        ++ products
        ++ ["", "End", ""]

simaProFile :: [BS.ByteString] -> BS.ByteString
simaProFile body =
    BS.intercalate
        "\r\n"
        ( [ "{SimaPro 9.6.0.1}"
          , "{CSV separator: semicolon}"
          , "{Decimal separator: .}"
          , ""
          ]
            ++ body
        )
