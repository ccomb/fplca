{-# LANGUAGE OverloadedStrings #-}

module SynonymExtractSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Method.FlowResolver (ILCDFlowInfo (..))
import SynonymDB.Extract (extractFromILCDFlows)
import Test.Hspec

spec :: Spec
spec = describe "extractFromILCDFlows" $ do
    it "emits baseName↔synonym pairs from <common:synonyms>" $ do
        let flows = M.fromList [(uuidA, mkFlow "Carbon dioxide" Nothing ["CO2", "carbonic anhydride"])]
        extractFromILCDFlows flows
            `shouldMatchList` [("Carbon dioxide", "CO2"), ("Carbon dioxide", "carbonic anhydride")]

    it "does not chain same-CAS flows into synonym pairs (CAS is matched by the cascade)" $ do
        -- Two distinct substances sharing one CAS, neither carrying <synonyms>.
        -- The old CAS grouping would emit ("Substance A","Substance B"); now nothing.
        let flows =
                M.fromList
                    [ (uuidA, mkFlow "Substance A" (Just "100-00-0") [])
                    , (uuidB, mkFlow "Substance B" (Just "100-00-0") [])
                    ]
        extractFromILCDFlows flows `shouldBe` []

mkFlow :: Text -> Maybe Text -> [Text] -> ILCDFlowInfo
mkFlow name cas syns =
    ILCDFlowInfo
        { ilcdBaseName = name
        , ilcdCompartment = Nothing
        , ilcdCAS = cas
        , ilcdSynonyms = syns
        , ilcdFlowType = "Elementary flow"
        , ilcdFlowPropertyRef = Nothing
        }

uuidA, uuidB :: UUID
uuidA = mkUUID "11111111-1111-1111-1111-111111111111"
uuidB = mkUUID "22222222-2222-2222-2222-222222222222"

mkUUID :: String -> UUID
mkUUID s = case UUID.fromString s of
    Just u -> u
    Nothing -> error ("bad test UUID: " <> s)
