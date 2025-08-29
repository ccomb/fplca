{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix (
    computeInventoryMatrix,
    buildDemandVectorFromIndex,
) where

import ACV.Types
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Debug.Trace (trace)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | Simple vector operations
type Vector = U.Vector Double

-- | Immediate trace with stderr flush
traceFlush :: String -> a -> a
traceFlush msg x = unsafePerformIO $ do
    hPutStrLn stderr msg
    hFlush stderr
    return x

-- Vector operations
norm2 :: Vector -> Double
norm2 v = sqrt $ U.sum $ U.map (^ 2) v

dot :: Vector -> Vector -> Double
dot v1 v2 = U.sum $ U.zipWith (*) v1 v2

scale :: Double -> Vector -> Vector
scale s v = U.map (* s) v

vectorAdd :: Vector -> Vector -> Vector
vectorAdd = U.zipWith (+)

vectorSub :: Vector -> Vector -> Vector
vectorSub = U.zipWith (-)

fromList :: [Double] -> Vector
fromList = U.fromList

toList :: Vector -> [Double]
toList = U.toList

zeroVector :: Int -> Vector
zeroVector n = U.replicate n 0.0

-- | Compressed Row Storage (CRS) format
data CRSMatrix = CRSMatrix
    { crsValues :: !(U.Vector Double)
    , crsColIndices :: !(U.Vector Int)
    , crsRowPointers :: !(U.Vector Int)
    , crsNumRows :: !Int
    , crsNumCols :: !Int
    }
    deriving (Show)

-- | Convert sparse triplets to CRS format
buildCRSMatrix :: [(Int, Int, Double)] -> Int -> Int -> CRSMatrix
buildCRSMatrix triplets nRows nCols =
    let sortedTriplets = sortOn (\(i, j, _) -> (i, j)) triplets
        (rows, cols, vals) = unzip3 sortedTriplets

        rowPtrs = U.create $ do
            ptrs <- MU.new (nRows + 1)
            forM_ [0 .. nRows] $ \i -> MU.write ptrs i 0
            forM_ rows $ \row -> do
                count <- MU.read ptrs (row + 1)
                MU.write ptrs (row + 1) (count + 1)
            forM_ [1 .. nRows] $ \i -> do
                prev <- MU.read ptrs (i - 1)
                current <- MU.read ptrs i
                MU.write ptrs i (prev + current)
            return ptrs
     in CRSMatrix
            { crsValues = U.fromList vals
            , crsColIndices = U.fromList cols
            , crsRowPointers = rowPtrs
            , crsNumRows = nRows
            , crsNumCols = nCols
            }

-- | Sparse matrix-vector multiplication
sparseMatVec :: CRSMatrix -> Vector -> Vector
sparseMatVec crs x =
    let n = crsNumRows crs
        result = U.create $ do
            vec <- MU.new n
            forM_ [0 .. n - 1] $ \i -> MU.write vec i 0.0
            forM_ [0 .. n - 1] $ \i -> do
                let start = crsRowPointers crs U.! i
                let end = crsRowPointers crs U.! (i + 1)
                rowSum <-
                    foldM
                        ( \acc k -> do
                            let val = crsValues crs U.! k
                            let col = crsColIndices crs U.! k
                            let xVal = x U.! col
                            return (acc + val * xVal)
                        )
                        0.0
                        [start .. end - 1]
                MU.write vec i rowSum
            return vec
     in result

-- | Extract diagonal elements
extractDiagonal :: CRSMatrix -> Vector
extractDiagonal crs =
    U.generate (crsNumRows crs) $ \i ->
        let start = crsRowPointers crs U.! i
            end = crsRowPointers crs U.! (i + 1)
            findDiag k
                | k >= end = 1.0
                | crsColIndices crs U.! k == i = crsValues crs U.! k
                | otherwise = findDiag (k + 1)
         in findDiag start

-- | Simple BiCGSTAB solver (no preconditioning)
solveBiCGSTAB :: CRSMatrix -> Vector -> Vector -> Double -> Int -> Either String Vector
solveBiCGSTAB a b x0 tol maxIter =
    let n = crsNumRows a
        b_norm = norm2 b
        rel_tol = tol * max 1.0 b_norm

        iterateBICGSTAB iter x r rho_prev p
            | iter >= maxIter = Left "Max iterations reached"
            | norm2 r <= rel_tol = Right x
            | otherwise =
                let v = sparseMatVec a p
                    rhat_dot_v = dot r0_hat v
                 in if abs rhat_dot_v < 1e-14
                        then Left "Breakdown in alpha computation"
                        else
                            let alpha = rho_prev / rhat_dot_v
                                s = vectorSub r (scale alpha v)
                             in if norm2 s <= rel_tol
                                    then Right (vectorAdd x (scale alpha p))
                                    else
                                        let t = sparseMatVec a s
                                            omega = dot t s / dot t t
                                            x_new = vectorAdd x (vectorAdd (scale alpha p) (scale omega s))
                                            r_new = vectorSub s (scale omega t)
                                            rho_new = dot r0_hat r_new
                                         in if abs rho_new < 1e-14
                                                then Left "Breakdown in rho computation"
                                                else
                                                    let beta = (rho_new / rho_prev) * (alpha / omega)
                                                        p_new = vectorAdd r_new (scale beta (vectorSub p (scale omega v)))
                                                     in iterateBICGSTAB (iter + 1) x_new r_new rho_new p_new

        r0 = vectorSub b (sparseMatVec a x0)
        r0_hat = r0
        rho0 = dot r0_hat r0
     in if norm2 r0 <= rel_tol
            then Right x0
            else
                if abs rho0 < 1e-14
                    then Left "Initial breakdown"
                    else iterateBICGSTAB 1 x0 r0 rho0 r0

-- | Simple iterative solver as fallback
solveSimpleIterative :: CRSMatrix -> Vector -> Vector
solveSimpleIterative a b =
    let n = crsNumRows a
        diag = extractDiagonal a
        invDiag = U.map (\d -> if abs d > 1e-14 then 1.0 / d else 1.0) diag

        iterateJacobi iter x
            | iter >= 100 = x
            | otherwise =
                let r = vectorSub b (sparseMatVec a x)
                    dx = U.zipWith (*) invDiag r
                    x_new = vectorAdd x dx
                 in if norm2 r < 1e-4
                        then x_new
                        else iterateJacobi (iter + 1) x_new
     in iterateJacobi 0 (zeroVector n)

-- | Solve sparse linear system
solveSparseLinearSystem :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystem techTriples n demandVec =
    let _ = traceFlush ("Building " ++ show n ++ "x" ++ show n ++ " system matrix") ()

        -- Build (I - A) system
        identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        negTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        stabilization = [(i, i, 1e-12) | i <- [0 .. n - 1]]
        allTriples = identityTriples ++ negTechTriples ++ stabilization

        systemCRS = buildCRSMatrix allTriples n n

        _ = traceFlush ("Starting solver with " ++ show (length allTriples) ++ " non-zero elements") ()

        -- Choose solver based on size
        result =
            if n <= 5000
                then case solveBiCGSTAB systemCRS demandVec (zeroVector n) 1e-6 500 of
                    Right sol -> sol
                    Left err ->
                        traceFlush ("BiCGSTAB failed: " ++ err ++ ", using iterative") $
                            solveSimpleIterative systemCRS demandVec
                else
                    solveSimpleIterative systemCRS demandVec -- For large systems, use simple iterative
     in result

-- | Matrix-based inventory calculation
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID =
    traceFlush ("Starting matrix computation for UUID: " ++ show rootUUID) $
        let activityCount = dbActivityCount db
            bioFlowCount = dbBiosphereCount db

            techTriples = dbTechnosphereTriples db
            bioTriples = dbBiosphereTriples db
            activityIndex = dbActivityIndex db
            bioFlowUUIDs = dbBiosphereFlows db

            _ = traceFlush ("Tech matrix: " ++ show (length techTriples) ++ " elements") ()
            _ = traceFlush ("Bio matrix: " ++ show (length bioTriples) ++ " elements") ()

            demandVec = buildDemandVectorFromIndex activityIndex rootUUID
            supplyVec = solveSparseLinearSystem techTriples activityCount demandVec
            inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

            result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
         in result

-- | Apply sparse matrix multiplication
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector -> Vector
applySparseMatrix sparseTriples nRows inputVec =
    let crs = buildCRSMatrix sparseTriples nRows nRows
     in sparseMatVec crs inputVec

-- | Build demand vector from index
buildDemandVectorFromIndex :: M.Map UUID Int -> UUID -> Vector
buildDemandVectorFromIndex activityIndex rootUUID =
    let n = M.size activityIndex
        rootIndex = fromMaybe 0 (M.lookup rootUUID activityIndex)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]
