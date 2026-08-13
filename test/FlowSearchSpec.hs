{-# LANGUAGE OverloadedStrings #-}

{- | What a flow search matches, and how it orders what it found.

Agribalyse 3.2 carries seven @Deltamethrin@ flows differing only by
compartment. A result that drops the sub-compartment, or an order that
interleaves the media, shows them as duplicate rows.
-}
module FlowSearchSpec (spec) where

import API.Types (FlowSearchResult (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database (flowMatchesQuery)
import Service (FlowFilter (..), flowSearchResults)
import Test.Hspec
import Types (BiosphereFlow (..), Compartment (..), FlowKind (..), UUID)

spec :: Spec
spec = do
    matchSpec
    orderSpec

matchSpec :: Spec
matchSpec = describe "Database.flowMatchesQuery" $ do
    it "finds a name whose words the query didn't punctuate" $
        flowMatchesQuery "water fossil" waterFossil `shouldBe` True

    it "finds it whatever order the words come in" $
        flowMatchesQuery "fossil water" waterFossil `shouldBe` True

    it "still finds it when the query does punctuate" $
        flowMatchesQuery "water, fossil" waterFossil `shouldBe` True

    it "requires every word, not just one" $
        flowMatchesQuery "water fossil" (namedFlow "Water, lake" []) `shouldBe` False

    it "keeps matching inside a word, which is how chemicals are searched" $
        flowMatchesQuery "chlor" (namedFlow "Trichloroethane" []) `shouldBe` True

    it "reads the name and the synonyms as one text" $
        flowMatchesQuery "laughing dinitrogen" laughingGas `shouldBe` True

    it "matches nothing when the query holds no word at all" $
        flowMatchesQuery ", " waterFossil `shouldBe` False

waterFossil :: FlowKind
waterFossil = namedFlow "Water, fossil" []

laughingGas :: FlowKind
laughingGas = namedFlow "Dinitrogen monoxide" ["laughing gas", "nitrous oxide"]

namedFlow :: Text -> [Text] -> FlowKind
namedFlow name syns = BioKind (biosphere 0 name syns)

orderSpec :: Spec
orderSpec = describe "Service.flowSearchResults" $ do
    it "carries the sub-compartment that tells two same-medium flows apart" $
        map (\r -> (fsrCategory r, fsrCompartment r)) (search sortByName deltamethrins)
            `shouldContain` [("soil", Just "agricultural"), ("soil", Just "forestry")]

    it "groups homonyms by compartment instead of leaving them in database order" $
        -- Sorting on the name alone is stable, so seven identical names would
        -- keep the input (UUID) order: soil, water, air, soil, soil, water, air.
        map compartmentOf (search sortByName deltamethrins)
            `shouldBe` [ ("air", Just "low. pop.")
                       , ("air", Just "low. pop., long-term")
                       , ("soil", Nothing)
                       , ("soil", Just "agricultural")
                       , ("soil", Just "forestry")
                       , ("water", Just "groundwater")
                       , ("water", Just "river")
                       ]

    it "reverses the whole order on desc, not just the requested column" $
        map compartmentOf (search sortByName{ffOrder = Just "desc"} deltamethrins)
            `shouldBe` reverse (map compartmentOf (search sortByName deltamethrins))

    it "orders by medium then sub-compartment when sorting on the category" $
        map compartmentOf (search sortByName{ffSort = Just "category"} deltamethrins)
            `shouldBe` map compartmentOf (search sortByName deltamethrins)

compartmentOf :: FlowSearchResult -> (Text, Maybe Text)
compartmentOf r = (fsrCategory r, fsrCompartment r)

-- | No unit database: every flow reports the same unit, so unit never breaks a tie.
search :: FlowFilter -> [FlowKind] -> [FlowSearchResult]
search = flowSearchResults M.empty

sortByName :: FlowFilter
sortByName =
    FlowFilter
        { ffQuery = "Deltamethrin"
        , ffLang = Nothing
        , ffLimit = Nothing
        , ffOffset = Nothing
        , ffSort = Nothing
        , ffOrder = Nothing
        }

{- | The seven Agribalyse 3.2 flows, in the UUID order the database yields
them — deliberately not the order they should come out in.
-}
deltamethrins :: [FlowKind]
deltamethrins =
    [ bioFlow 1 "soil" Nothing
    , bioFlow 2 "water" (Just "groundwater")
    , bioFlow 3 "air" (Just "low. pop.")
    , bioFlow 4 "soil" (Just "agricultural")
    , bioFlow 5 "soil" (Just "forestry")
    , bioFlow 6 "water" (Just "river")
    , bioFlow 7 "air" (Just "low. pop., long-term")
    ]

bioFlow :: Int -> Text -> Maybe Text -> FlowKind
bioFlow n medium sub =
    BioKind (biosphere n "Deltamethrin" []){bfCompartment = Just (Compartment medium sub)}

biosphere :: Int -> Text -> [Text] -> BiosphereFlow
biosphere n name syns =
    BiosphereFlow
        { bfId = mkUUID n
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.singleton "en" (S.fromList syns)
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0
