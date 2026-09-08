{-# LANGUAGE OverloadedStrings #-}

module CASBackfillSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import SubstanceRegistry (CASNumber (..), NormName (..))
import SynonymDB (normalizeName)
import Types (
    BiosphereFlow (..),
    Compartment (..),
    Medium (..),
    fillBioFlowCAS,
 )

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

mkFlow :: Integer -> Text -> Maybe Text -> BiosphereFlow
mkFlow i name cas =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = cas
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment Water Nothing)
        }

-- Key the binding through the same normalizer 'fillBioFlowCAS' applies, so the
-- test pins the fill behaviour, not normalizeName's internals.
bindings :: M.Map NormName CASNumber
bindings = M.fromList [(NormName (normalizeName "2,4-D"), CASNumber "94-75-7")]

casOf :: UUID -> M.Map UUID BiosphereFlow -> Maybe Text
casOf u m = M.lookup u m >>= bfCAS

spec :: Spec
spec = describe "fillBioFlowCAS" $ do
    it "fills an empty bfCAS from a name→CAS binding" $ do
        let out = fillBioFlowCAS bindings (M.singleton (mkUUID 1) (mkFlow 1 "2,4-D" Nothing))
        casOf (mkUUID 1) out `shouldBe` Just "94-75-7"

    it "never overwrites a CAS the flow already carries" $ do
        let out = fillBioFlowCAS bindings (M.singleton (mkUUID 1) (mkFlow 1 "2,4-D" (Just "99-99-9")))
        casOf (mkUUID 1) out `shouldBe` Just "99-99-9"

    it "treats an empty-string CAS as a hole to fill" $ do
        let out = fillBioFlowCAS bindings (M.singleton (mkUUID 1) (mkFlow 1 "2,4-D" (Just "")))
        casOf (mkUUID 1) out `shouldBe` Just "94-75-7"

    it "leaves a flow whose name has no binding untouched" $ do
        let out = fillBioFlowCAS bindings (M.singleton (mkUUID 2) (mkFlow 2 "water" Nothing))
        casOf (mkUUID 2) out `shouldBe` Nothing
