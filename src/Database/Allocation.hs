{-# LANGUAGE OverloadedStrings #-}

{- | From an activity as its source wrote it to the single-output processes
the matrix can hold.

An activity may produce several products. A matrix column carries one
product's share of the inventory, so every activity passes through here
between the parser and the index:

* 'allocate' splits an activity whose product outputs all carry a declared
  share into one process per product, the shared exchanges scaled by that
  share. It never refuses: what it cannot split it hands back unchanged.
* 'asAllocated' is the gate. An activity still carrying a 'Coproduct', or
  not carrying exactly one reference exchange, has no column the matrix could
  fill honestly. The matrix builder, the compute path and the quality report
  all read this one verdict, so a refused activity loads, is inspectable, and
  is refused with the same words everywhere.

The split reproduces what the SimaPro parser used to do per block, with one
rule the parser never had to state: a product row's share is applied as
declared, whether or not the block's shares sum to 100. A single product at
51 % or at 0 % is what SimaPro itself reads from such a block, and what the
SimaPro writer emits for every process split here.
-}
module Database.Allocation (
    Allocating (..),
    AllocationRefusal (..),
    AllocatedActivity,
    allocatedActivity,
    allocate,
    allocateAll,
    asAllocated,
    describeRefusal,
    propertyKeyShares,
    scaleExchange,
    PropertyRefusal (..),
    propertyName,
    productProperty,
    propertyShares,
    massShares,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import UnitConversion (UnitConfig, convertUnit)

-- | Why an activity has no honest column in the matrix.
data AllocationRefusal
    = -- | Product outputs of the activity, and how many of them carry no declared share.
      UnallocatedOutputs
        { refusedOutputs :: !Int
        , refusedWithoutShare :: !Int
        }
    | -- | How many reference exchanges the activity carries, where exactly one is needed.
      NoSingleReference !Int
    deriving (Eq, Show)

{- | An activity 'asAllocated' accepted. The constructor is not exported: the
only way to hold one is to have passed the gate.
-}
newtype AllocatedActivity = AllocatedActivity Activity

allocatedActivity :: AllocatedActivity -> Activity
allocatedActivity (AllocatedActivity act) = act

{- | What deciding the shares of one block needs besides the block: the key,
and the two unit tables reading a key needs -- the conversion table a property
is weighed through, and the names this database gives its units.
-}
data Allocating = Allocating
    { alKey :: !AllocationKey
    , alUnitConfig :: !UnitConfig
    , alUnitDB :: !UnitDB
    }

{- | Split an activity into one process per product output when every product
carries a declared share; otherwise return it as it is, normalised.

Each process keeps its own product as the reference, in the position the
source gave it, followed by the shared exchanges (inputs, avoided products,
biosphere, waste) scaled by @share / 100@. Its unit is the product's, and its
classification is the activity's with whatever the product row states on top.
-}
allocate :: Allocating -> Activity -> NonEmpty Activity
allocate Allocating{alKey = key, alUnitConfig = unitCfg, alUnitDB = unitDB} act =
    fromMaybe (pure normalised) $ do
        shares <- sharesUnder key
        NE.nonEmpty (zipWith process products shares)
  where
    normalised :: Activity
    normalised = normalise act

    products, shared :: [Exchange]
    (products, shared) = partition exchangeIsProductOutput (exchanges normalised)

    sharesUnder :: AllocationKey -> Maybe [Double]
    sharesUnder Declared = map dsPercent <$> traverse exchangeDeclaredShare products
    sharesUnder (ByProperty prop) = propertyKeyShares prop unitCfg unitDB products

    process :: Exchange -> Double -> Activity
    process productEx share =
        normalised
            { exchanges = applied share (asReference productEx) : map (scaleExchange (share / 100)) shared
            , activityUnit = getUnitNameForExchange unitDB productEx
            , activityClassification = M.union (exchangeClassification productEx) (activityClassification normalised)
            }

    {- The share a split process records is the one that was applied to it.
    Under the declared key that is already what the row says, formula included.
    Under a property key it is not: the row's percentage and its formula both
    describe the key the source chose, and a database whose inventory was
    divided one way while its rows still claim another exports divided by the
    wrong factor. -}
    applied :: Double -> Exchange -> Exchange
    applied share ex = case key of
        Declared -> ex
        ByProperty _ -> withShare share ex

    withShare :: Double -> Exchange -> Exchange
    withShare share ex = case ex of
        TechnosphereExchange{} -> ex{techShare = Just (DeclaredShare{dsPercent = share, dsFormula = Nothing})}
        BiosphereExchange{} -> ex
        WasteExchange{} -> ex

{- | The shares a property key gives the products of one block, keeping a
declared zero at zero.

A product whose source declares exactly 0 % carries none of the load, and the
key divides what is left among the others. That zero is a modelling decision --
an author who cut a residue out of the block on purpose -- and recomputing it
into a share would undo the decision without saying so. Every other declared
share is replaced, which is the whole point of naming another key.

'Nothing' where the property cannot decide, because 'allocate' never refuses:
the block is handed back whole and the gate says why it has no column.
-}
propertyKeyShares :: AllocationProperty -> UnitConfig -> UnitDB -> [Exchange] -> Maybe [Double]
propertyKeyShares prop cfg unitDB products = do
    block <- NE.nonEmpty products
    shares <- either (const Nothing) Just (propertyShares prop unitDB cfg block)
    let kept = NE.toList (NE.zipWith zeroDeclared block shares)
        total = sum kept
    guard (total > 0)
    pure (map (* (100 / total)) kept)
  where
    zeroDeclared :: Exchange -> Double -> Double
    zeroDeclared ex share = if declaresZero ex then 0 else share

    declaresZero :: Exchange -> Bool
    declaresZero ex = (dsPercent <$> exchangeDeclaredShare ex) == Just 0

-- | 'allocate' over a parsed list, in order.
allocateAll :: Allocating -> [Activity] -> [Activity]
allocateAll alloc = concatMap (NE.toList . allocate alloc)

{- | The gate: the activity has no 'Coproduct' left and exactly one reference
exchange.
-}
asAllocated :: Activity -> Either AllocationRefusal AllocatedActivity
asAllocated act
    | any isCoproduct products =
        Left (UnallocatedOutputs (length products) (length (filter (isNothing . exchangeDeclaredShare) products)))
    | references /= 1 = Left (NoSingleReference references)
    | otherwise = Right (AllocatedActivity act)
  where
    products :: [Exchange]
    products = filter exchangeIsProductOutput (exchanges act)

    references :: Int
    references = length (filter exchangeIsReference (exchanges act))

-- | What is missing, and what would repair it.
describeRefusal :: AllocationRefusal -> Text
describeRefusal refusal = case refusal of
    UnallocatedOutputs outputs withoutShare ->
        T.pack (show outputs)
            <> " product outputs, "
            <> T.pack (show withoutShare)
            <> " without a declared share: state a share on every product row, or load the dataset already allocated"
    NoSingleReference 0 -> "no reference exchange: one product output must be the reference"
    NoSingleReference k -> T.pack (show k) <> " reference exchanges where exactly one is needed"

-- | The word a message uses for a property.
propertyName :: AllocationProperty -> Text
propertyName prop = case prop of
    DryMass -> "dry mass"
    WetMass -> "wet mass"

-- | Why a physical property cannot serve as the key of a block.
data PropertyRefusal
    = -- | A product of the block states nothing of the property, and nothing stands in for it.
      NotStated
    | -- | The unit a product is stated in, which is not a mass.
      NotAMass !Text
    | -- | The amount a product states, which no share can be read from.
      NonPositiveMass !Double
    deriving (Eq, Show)

{- | The quantity one product line carries of a physical property.

The source's own statement wins, in the unit the source writes it in. Where
the source states none, a line already written in a mass unit is its own wet
mass: that is the same fact read from the amount column instead of from a
property table, and it is what makes a wet-mass key computable on the formats
that have no property table at all.

A dry mass is never read from an amount. Nothing in a wet kilogram says how
much of it is water, so a block whose products declare none is refused rather
than divided on a number that measures something else.
-}
productProperty :: AllocationProperty -> UnitDB -> Exchange -> Maybe StatedAmount
productProperty prop unitDB ex = case prop of
    DryMass -> perUnit (epDryMass (exchangeProperties ex))
    WetMass -> perUnit (epWetMass (exchangeProperties ex)) <|> ownAmount
  where
    -- A property is stated per unit of the line, so the line's own amount is
    -- what turns it into a quantity.
    perUnit :: Maybe StatedAmount -> Maybe StatedAmount
    perUnit stated = (\s -> s{saAmount = saAmount s * exchangeAmount ex}) <$> stated

    ownAmount :: Maybe StatedAmount
    ownAmount = Just StatedAmount{saUnit = getUnitNameForExchange unitDB ex, saAmount = exchangeAmount ex}

{- | The share each product of one block carries under a property key, as
percentages in the order given.

Every quantity is converted to kilograms first, for the reason 'massShares'
gives, and the same refusals apply on top of the property not being stated at
all. Nothing here knows about the shares the source declares: this is what the
property says, and combining the two is the caller's business.
-}
propertyShares ::
    AllocationProperty ->
    UnitDB ->
    UnitConfig ->
    NonEmpty Exchange ->
    Either PropertyRefusal (NonEmpty Double)
propertyShares prop unitDB cfg products =
    massShares cfg =<< maybe (Left NotStated) Right (traverse (productProperty prop unitDB) products)

{- | The share each product of one source block would carry if the key were
its mass, as percentages in the order given.

This is not an 'AllocationKey'. Nothing is split and nothing is scored: it
answers what the mass would say beside what the source declared, and the
reader draws the comparison. An impact per kilo is the quotient of the two
fractions, so the whole comparison follows from these numbers alone.

Every amount is converted to kilograms first, because summing amounts as
written would hand the half-kilo of a 1 kg / 500 g pair 99.8 % of the load.
A unit that is not a mass, or an amount at or below zero, refuses the whole
block rather than dropping one product to a silent zero.
-}
massShares :: UnitConfig -> NonEmpty StatedAmount -> Either PropertyRefusal (NonEmpty Double)
massShares cfg products = share <$> traverse mass products
  where
    mass :: StatedAmount -> Either PropertyRefusal Double
    mass stated
        | saAmount stated <= 0 = Left (NonPositiveMass (saAmount stated))
        | otherwise =
            maybe (Left (NotAMass (saUnit stated))) Right $
                convertUnit cfg (saUnit stated) "kg" (saAmount stated)

    -- Every mass is strictly positive, so their total is too.
    share :: NonEmpty Double -> NonEmpty Double
    share masses = (* (100 / sum masses)) <$> masses

{- | The two rules the EcoSpold parsers used to apply to every dataset: a
zero-amount coproduct the source states no share for is not an output, and
an activity with no reference but one non-zero product output is that
product's process.
-}
normalise :: Activity -> Activity
normalise act = promote act{exchanges = filter keep (exchanges act)}
  where
    keep :: Exchange -> Bool
    keep ex = case ex of
        TechnosphereExchange{techRole = Coproduct, techAmount = amount, techShare = Nothing} -> amount /= 0
        TechnosphereExchange{} -> True
        BiosphereExchange{} -> True
        WasteExchange{} -> True

    promote :: Activity -> Activity
    promote a
        | any exchangeIsReference (exchanges a) = a
        | otherwise = case filter ((/= 0) . exchangeAmount) (filter exchangeIsProductOutput (exchanges a)) of
            [single] -> a{exchanges = map (promoteTo single) (exchanges a)}
            _ -> a

    promoteTo :: Exchange -> Exchange -> Exchange
    promoteTo single ex
        | exchangeFlowId ex == exchangeFlowId single && exchangeIsProductOutput ex = asReference ex
        | otherwise = ex

-- | Scale an exchange amount by a factor.
scaleExchange :: Double -> Exchange -> Exchange
scaleExchange factor ex = case ex of
    TechnosphereExchange{} -> ex{techAmount = techAmount ex * factor}
    BiosphereExchange{} -> ex{bioAmount = bioAmount ex * factor}
    WasteExchange{} -> ex{waAmount = waAmount ex * factor}

isCoproduct :: Exchange -> Bool
isCoproduct ex = case ex of
    TechnosphereExchange{techRole = role} -> role == Coproduct
    BiosphereExchange{} -> False
    WasteExchange{} -> False

asReference :: Exchange -> Exchange
asReference ex = case ex of
    TechnosphereExchange{} -> ex{techRole = ReferenceProduct}
    BiosphereExchange{} -> ex
    WasteExchange{} -> ex
