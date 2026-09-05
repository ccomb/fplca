{-# LANGUAGE OverloadedStrings #-}

{- | The block name an activity summary carries ('Types.sourceBlockOf').

A listing that wants to show the coproducts of one block together cannot group
on the process id, which names the product. The block name renders
'activityGroupKey', the key a block's coproducts are indexed under, and it
travels with the count of products the block holds so a page can say when it
is showing only part of one.
-}
module SourceBlockSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import Data.List (nub, sort)
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.Types (ActivitySummary (..))
import qualified Data.Text as T
import Database (buildDatabaseWithMatrices)
import Database.Loader (defaultLoadOptions, loadDatabaseWithLocationAliases)
import Service (mkActivitySummary)
import Types (AllocationKey (..), BuildInputs (..), Database (..))
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "the source block an activity summary names" $ do
    it "separates two blocks that share a process name" $
        withDatabase twoBlocksSharingAName $ \summaries -> do
            -- Four processes, two blocks of two products.
            map prsProductName summaries
                `shouldMatchList` ["Cheese", "Whey", "Steel", "Slag"]
            length (nub (map prsBlock summaries)) `shouldBe` 2
            sort (map prsBlockProducts summaries) `shouldBe` [2, 2, 2, 2]

    it "gives the coproducts of one block the same name" $
        withDatabase twoBlocksSharingAName $ \summaries -> do
            let blockOf name = [prsBlock s | s <- summaries, prsProductName s == name]
            blockOf "Cheese" `shouldBe` blockOf "Whey"
            blockOf "Cheese" `shouldNotBe` blockOf "Steel"

    it "names a block of one product too, so every row has one" $
        withDatabase oneBlockOneProduct $ \summaries -> do
            map prsBlockProducts summaries `shouldBe` [1]
            map (T.null . prsBlock) summaries `shouldBe` [False]

-- | Every process of a SimaPro file, summarised the way a search result is.
withDatabase :: BS.ByteString -> ([ActivitySummary] -> IO ()) -> IO ()
withDatabase csv k = withSystemTempDirectory "source-block" $ \dir -> do
    let path = dir </> "source.csv"
    BS.writeFile path csv
    loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) path
    simpleDb <- either (fail . T.unpack) pure loaded
    built <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) simpleDb
    db <- either (fail . T.unpack) pure built
    k [mkActivitySummary db pid act | (pid, act) <- zip [0 ..] (V.toList (dbActivities db))]

{- | Two blocks written under one process name, each with two coproducts. The
producer tells them apart by their @Process identifier@ line and nothing else,
which is what a block name has to survive.
-}
twoBlocksSharingAName :: BS.ByteString
twoBlocksSharingAName =
    simaProFile
        ( block "P-1" ["Cheese;kg;1;60;not defined;material;", "Whey;kg;3;40;not defined;material;"]
            ++ block "P-2" ["Steel;kg;1;70;not defined;material;", "Slag;kg;2;30;not defined;material;"]
        )

-- | One block, one product: it is still a block, and it holds one product.
oneBlockOneProduct :: BS.ByteString
oneBlockOneProduct = simaProFile (block "P-1" ["Cheese;kg;1;100;not defined;material;"])

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
