{- | Cut-off allocation strategy shared by the EcoSpold1 and EcoSpold2 parsers.

Post-processing applied once per parsed activity (outside the SAX fold):
reduce a multi-output dataset to a single reference product so the downstream
engine sees a single-output process.
-}
module EcoSpold.Cutoff (
    applyCutoffStrategy,
    hasReferenceProduct,
    removeZeroAmountCoproducts,
    assignSingleProductAsReference,
    isProductionExchange,
) where

import qualified Data.Text as T
import Types

{- | Apply cut-off strategy
1. Remove zero-amount production exchanges (co-products)
2. Assign single non-zero product as reference product
3. Ensure single-output process structure
4. VALIDATION: Fail if no reference product can be established
-}
applyCutoffStrategy :: Activity -> Either String Activity
applyCutoffStrategy activity =
    let filteredExchanges = removeZeroAmountCoproducts (exchanges activity)
        updatedActivity = activity{exchanges = filteredExchanges}
        finalActivity =
            if hasReferenceProduct updatedActivity
                then updatedActivity
                else assignSingleProductAsReference updatedActivity
     in if hasReferenceProduct finalActivity
            then Right finalActivity
            else Left $ "Activity has no reference product: " ++ T.unpack (activityName activity)

-- | Check if activity has any reference product
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity = any exchangeIsReference (exchanges activity)

-- | Remove production exchanges with zero amounts
removeZeroAmountCoproducts :: [Exchange] -> [Exchange]
removeZeroAmountCoproducts = filter keepExchange
  where
    keepExchange TechnosphereExchange{techRole = ReferenceProduct} = True
    keepExchange TechnosphereExchange{techRole = ReferenceInput} = True
    keepExchange TechnosphereExchange{techRole = Input} = True
    keepExchange TechnosphereExchange{techRole = Coproduct, techAmount = amount} = amount /= 0.0
    keepExchange BiosphereExchange{} = True
    keepExchange WasteExchange{} = True

-- | Assign single product as reference product
assignSingleProductAsReference :: Activity -> Activity
assignSingleProductAsReference activity =
    let productionExchanges = [ex | ex <- exchanges activity, isProductionExchange ex]
        nonZeroProduction = [ex | ex <- productionExchanges, exchangeAmount ex /= 0.0]
     in case nonZeroProduction of
            [singleProduct] ->
                -- Update the single product to be reference product
                let updatedExchanges = map (updateReferenceProduct singleProduct) (exchanges activity)
                 in activity{exchanges = updatedExchanges}
            [] -> activity -- No production exchanges, leave as-is
            _ -> activity -- Multiple production exchanges, leave as-is (shouldn't happen after cutoff)

-- | Check if exchange is production exchange (technosphere output)
isProductionExchange :: Exchange -> Bool
isProductionExchange TechnosphereExchange{techRole = ReferenceProduct} = True
isProductionExchange TechnosphereExchange{techRole = Coproduct} = True
isProductionExchange TechnosphereExchange{techRole = Input} = False
isProductionExchange TechnosphereExchange{techRole = ReferenceInput} = False
isProductionExchange BiosphereExchange{} = False
isProductionExchange WasteExchange{} = False

-- | Update reference product flag for the specified exchange
updateReferenceProduct :: Exchange -> Exchange -> Exchange
updateReferenceProduct target current
    | exchangeFlowId target == exchangeFlowId current = markAsReference current
    | otherwise = unmarkAsReference current

-- | Promote a production exchange to reference product
markAsReference :: Exchange -> Exchange
markAsReference ex@TechnosphereExchange{} = ex{techRole = ReferenceProduct}
markAsReference ex@BiosphereExchange{} = ex
markAsReference ex@WasteExchange{} = ex

-- | Demote a reference role back to non-reference (preserving input/output direction)
unmarkAsReference :: Exchange -> Exchange
unmarkAsReference ex@TechnosphereExchange{techRole = role} = ex{techRole = demote role}
  where
    demote ReferenceProduct = Coproduct
    demote ReferenceInput = Input
    demote Coproduct = Coproduct
    demote Input = Input
unmarkAsReference ex@BiosphereExchange{} = ex
unmarkAsReference ex@WasteExchange{} = ex
