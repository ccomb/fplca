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
    AllocationKey (..),
    AllocationRefusal (..),
    AllocatedActivity,
    allocatedActivity,
    allocate,
    allocateAll,
    asAllocated,
    describeRefusal,
    scaleExchange,
    MassKeyRefusal (..),
    massShares,
) where

import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import UnitConversion (UnitConfig, convertUnit)

-- | How the shares of a multi-output activity are decided.
data AllocationKey
    = -- | The source states each product's share, a number or an evaluated formula.
      Declared
    deriving (Eq, Show)

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

{- | Split an activity into one process per product output when every product
carries a declared share; otherwise return it as it is, normalised.

Each process keeps its own product as the reference, in the position the
source gave it, followed by the shared exchanges (inputs, avoided products,
biosphere, waste) scaled by @share / 100@. Its unit is the product's, and its
classification is the activity's with whatever the product row states on top.
-}
allocate :: AllocationKey -> UnitDB -> Activity -> NonEmpty Activity
allocate Declared unitDB act =
    fromMaybe (pure normalised) $ do
        shares <- traverse exchangeDeclaredShare products
        NE.nonEmpty (zipWith process products shares)
  where
    normalised :: Activity
    normalised = normalise act

    products, shared :: [Exchange]
    (products, shared) = partition exchangeIsProductOutput (exchanges normalised)

    process :: Exchange -> DeclaredShare -> Activity
    process productEx share =
        normalised
            { exchanges = asReference productEx : map (scaleExchange (dsPercent share / 100)) shared
            , activityUnit = getUnitNameForExchange unitDB productEx
            , activityClassification = M.union (exchangeClassification productEx) (activityClassification normalised)
            }

-- | 'allocate' over a parsed list, in order.
allocateAll :: AllocationKey -> UnitDB -> [Activity] -> [Activity]
allocateAll key unitDB = concatMap (NE.toList . allocate key unitDB)

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

-- | Why the mass of a block's products cannot serve as a key.
data MassKeyRefusal
    = -- | The unit a product is stated in, which is not a mass.
      NotAMass !Text
    | -- | The amount a product states, which no share can be read from.
      NonPositiveMass !Double
    deriving (Eq, Show)

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
massShares :: UnitConfig -> NonEmpty StatedAmount -> Either MassKeyRefusal (NonEmpty Double)
massShares cfg products = share <$> traverse mass products
  where
    mass :: StatedAmount -> Either MassKeyRefusal Double
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

{- | Scale an exchange amount by a factor.

A technosphere line's stated properties describe the line as a whole, so they
are scaled with it: half a line of 2 kg weighs 1 kg, and a property left
behind at 2 kg would contradict the amount printed beside it.
-}
scaleExchange :: Double -> Exchange -> Exchange
scaleExchange factor ex = case ex of
    TechnosphereExchange{} ->
        ex
            { techAmount = techAmount ex * factor
            , techProperties = scaleProperties factor (techProperties ex)
            }
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
