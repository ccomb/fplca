{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix (
    computeInventoryMatrix,
    buildDemandVector,
    buildDemandVectorFromIndex,
) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.List (elemIndex, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Debug.Trace (trace)
import System.Random (mkStdGen, randomRs)

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

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

-- | Compressed Row Storage (CRS) format for efficient sparse operations with unboxed vectors
data CRSMatrix = CRSMatrix
    { crsValues :: !(U.Vector Double) -- Non-zero values (unboxed)
    , crsColIndices :: !(U.Vector Int) -- Column indices for each value (unboxed)
    , crsRowPointers :: !(U.Vector Int) -- Start index of each row in values array (unboxed)
    , crsNumRows :: !Int -- Number of rows
    , crsNumCols :: !Int -- Number of columns
    }
    deriving (Show)

-- | Convert sparse triplets to CRS format with optimized unboxed construction
buildCRSMatrix :: [(Int, Int, Double)] -> Int -> Int -> CRSMatrix
buildCRSMatrix triplets nRows nCols =
    let
        -- Sort triplets by row first, then by column for optimal access pattern
        sortedTriplets = sortOn (\(i, j, _) -> (i, j)) triplets
        nnz = length sortedTriplets

        -- Extract components efficiently
        (rows, cols, vals) = unzip3 sortedTriplets

        -- Build row pointers with single-pass algorithm using unboxed vectors
        rowPtrs = U.create $ do
            -- Pre-allocate exactly the size we need
            ptrs <- MU.new (nRows + 1)

            -- Initialize all pointers to 0
            forM_ [0 .. nRows] $ \i -> MU.write ptrs i 0

            -- Count elements per row in single pass
            forM_ rows $ \row -> do
                count <- MU.read ptrs (row + 1)
                MU.write ptrs (row + 1) (count + 1)

            -- Convert counts to cumulative pointers
            forM_ [1 .. nRows] $ \i -> do
                prevCount <- MU.read ptrs (i - 1)
                currentCount <- MU.read ptrs i
                MU.write ptrs i (prevCount + currentCount)

            return ptrs
     in
        CRSMatrix
            { crsValues = U.fromList vals
            , crsColIndices = U.fromList cols
            , crsRowPointers = rowPtrs
            , crsNumRows = nRows
            , crsNumCols = nCols
            }

-- | Optimized sparse matrix-vector multiplication: y = A * x
sparseMatVec :: CRSMatrix -> Vector -> Vector
sparseMatVec crs x =
    let n = crsNumRows crs
        resultUnboxed = U.create $ do
            result <- MU.new n
            -- Initialize result to zero
            forM_ [0 .. n - 1] $ \i -> MU.write result i 0.0

            -- Compute matrix-vector product row by row
            forM_ [0 .. n - 1] $ \i -> do
                let rowStart = crsRowPointers crs U.! i
                let rowEnd = crsRowPointers crs U.! (i + 1)

                -- Compute dot product for row i
                let computeRowSum acc k
                        | k >= rowEnd = acc
                        | otherwise =
                            let val = crsValues crs U.! k
                                colIdx = crsColIndices crs U.! k
                                xVal = x U.! colIdx
                             in computeRowSum (acc + val * xVal) (k + 1)

                let rowSum = computeRowSum 0.0 rowStart
                MU.write result i rowSum

            return result
     in resultUnboxed

-- | Extract diagonal elements for preconditioning
extractDiagonal :: CRSMatrix -> Vector
extractDiagonal crs =
    let n = crsNumRows crs
        diagUnboxed = U.generate n getDiagElement
     in diagUnboxed
  where
    getDiagElement i =
        let rowStart = crsRowPointers crs U.! i
            rowEnd = crsRowPointers crs U.! (i + 1)
            findDiag k
                | k >= rowEnd = 1.0 -- Default to 1.0 if no diagonal found
                | (crsColIndices crs U.! k) == i = crsValues crs U.! k
                | otherwise = findDiag (k + 1)
         in findDiag rowStart

{- | Clean BiCGSTAB implementation following Wikipedia algorithm exactly
Solves Ax = b for sparse matrices using stabilized biconjugate gradient method
-}
solveBiCGSTAB :: CRSMatrix -> Vector -> Vector -> Double -> Int -> Either String Vector
solveBiCGSTAB a b x0 tol maxIter =
    let n = crsNumRows a
        b_norm = norm2 b
        rel_tol = tol * max 1.0 b_norm

        -- Wikipedia Algorithm Implementation:
        -- Step 1: r0 = b - A*x0
        r0 = vectorSub b (sparseMatVec a x0)
        r0_norm = norm2 r0

        _ = trace ("BiCGSTAB: Starting for " ++ show n ++ "x" ++ show n ++ " system, target=" ++ show rel_tol) ()

        -- Step 2: Choose r̂0 = r0 (standard choice)
        r_hat_0 = r0

        -- Step 3: ρ0 = (r̂0, r0)
        rho_0 = dot r_hat_0 r0

        -- Step 4: p0 = r0
        p_0 = r0

        -- BiCGSTAB iteration
        bicgstabIter !iter !x !r !rho_prev !p
            | iter >= maxIter = Left $ "BiCGSTAB: Failed to converge in " ++ show maxIter ++ " iterations"
            | norm2 r <= rel_tol =
                let _ = trace ("BiCGSTAB: Converged in " ++ show iter ++ " iterations") () in Right x
            | otherwise =
                let
                    -- Step 1: v = A * p(i-1)
                    v = sparseMatVec a p

                    -- Step 2: α = ρ(i-1) / (r̂0, v)
                    r_hat_dot_v = dot r_hat_0 v
                 in
                    if abs r_hat_dot_v < 1e-14
                        then Left "BiCGSTAB: Breakdown in alpha computation"
                        else
                            let
                                alpha = rho_prev / r_hat_dot_v

                                -- Step 3: h = x(i-1) + α * p(i-1)
                                h = vectorAdd x (scale alpha p)

                                -- Step 4: s = r(i-1) - α * v
                                s = vectorSub r (scale alpha v)
                                s_norm = norm2 s
                             in
                                -- Step 5: Check intermediate convergence
                                if s_norm <= rel_tol
                                    then Right h
                                    else
                                        let
                                            -- Step 6: t = A * s
                                            t = sparseMatVec a s

                                            -- Step 7: ω = (t, s) / (t, t)
                                            t_dot_t = dot t t
                                         in
                                            if abs t_dot_t < 1e-14
                                                then Left "BiCGSTAB: Breakdown in omega computation"
                                                else
                                                    let
                                                        omega = dot t s / t_dot_t

                                                        -- Step 8: x(i) = h + ω * s
                                                        x_new = vectorAdd h (scale omega s)

                                                        -- Step 9: r(i) = s - ω * t
                                                        r_new = vectorSub s (scale omega t)

                                                        -- Step 11: ρ(i) = (r̂0, r(i))
                                                        rho_new = dot r_hat_0 r_new
                                                     in
                                                        if abs rho_new < 1e-14
                                                            then Left "BiCGSTAB: Breakdown in rho computation"
                                                            else
                                                                let
                                                                    -- Step 12: β = (ρ(i) / ρ(i-1)) * (α / ω)
                                                                    beta = (rho_new / rho_prev) * (alpha / omega)

                                                                    -- Step 13: p(i) = r(i) + β * (p(i-1) - ω * v)
                                                                    p_new = vectorAdd r_new (scale beta (vectorSub p (scale omega v)))
                                                                 in
                                                                    bicgstabIter (iter + 1) x_new r_new rho_new p_new
     in if r0_norm <= rel_tol
            then Right x0
            else
                if abs rho_0 < 1e-14
                    then Left "BiCGSTAB: Initial breakdown"
                    else bicgstabIter 1 x0 r0 rho_0 p_0

-- helper: zero vector
zeroVector :: Int -> Vector
zeroVector n = U.replicate n 0.0

-- already present:
-- extractDiagonal :: CRSMatrix -> Vector

invDiag :: CRSMatrix -> Vector
invDiag a = U.map (\d -> if abs d < 1e-30 then 1.0 else 1.0 / d) (extractDiagonal a)

-- apply left preconditioner: z = M^{-1} r, with M = diag(A)
applyJacobi :: Vector -> Vector -> Vector
applyJacobi invD v = U.zipWith (*) invD v

{- | Preconditioned BiCGSTAB(l) with Jacobi (left) and residual replacement.
     l >= 1 (l=1 ≡ BiCGSTAB). Based on Sleijpen & Fokkema (1993).
-}
solveBiCGSTABL :: CRSMatrix -> Vector -> Vector -> Double -> Int -> Int -> Either String Vector
solveBiCGSTABL a b x0 tol maxIter lIn =
    let n = crsNumRows a
        l = max 1 lIn
        bNorm = norm2 b
        relTol = tol * max 1.0 bNorm

        -- Jacobi left preconditioner
        invD = invDiag a
        mInv = applyJacobi invD

        -- A~ v = M^{-1} (A v)
        amat v = mInv (sparseMatVec a v)

        -- true residual (unpreconditioned)
        trueRes x = vectorSub b (sparseMatVec a x)

        -- initial residual (preconditioned)
        r0_true = trueRes x0
        r0 = mInv r0_true
        rhat = r0

        alpha0 = 1.0
        omega0 = 1.0
        rho0 = 1.0
        v0 = zeroVector n
        p0 = zeroVector n

        -- one MR(1) step on preconditioned operator
        mrStep :: Vector -> Vector -> (Vector, Vector, Double, Bool)
        mrStep xAcc sPrev =
            let t = amat sPrev -- t = A~ s
                tDotT = dot t t
             in if abs tDotT < 1e-30
                    then (xAcc, sPrev, 0, True)
                    else
                        let omega = (dot t sPrev) / tDotT
                            xAcc' = vectorAdd xAcc (scale omega sPrev)
                            r' = vectorSub sPrev (scale omega t)
                            stop = norm2 (trueRes xAcc') <= relTol
                         in (xAcc', r', omega, stop)

        mrLoop :: Int -> Vector -> Vector -> (Vector, Vector, Double) -- (x', r', lastOmega)
        mrLoop i xAcc sPrev
            | i <= 0 = (xAcc, sPrev, 1.0)
            | otherwise =
                let (xAcc1, r1, omega1, stop1) = mrStep xAcc sPrev
                 in if stop1 || i == 1
                        then (xAcc1, r1, omega1)
                        else mrLoop (i - 1) xAcc1 r1

        -- residual replacement: recompute true residual, re-precondition, reset shadow & search dir
        residualReplace :: Vector -> (Vector, Vector, Double, Double, Vector, Vector)
        residualReplace x =
            let r_true = trueRes x
                r_pre = mInv r_true
             in (r_pre, r_pre, 1.0, 1.0, zeroVector n, zeroVector n) -- r, rhat, rhoPrev, omegaPrev, p, v
        go :: Int -> Int -> Vector -> Vector -> Vector -> Double -> Double -> Double -> Vector -> Either String Vector
        go !k !sinceRR x r p rhoPrev alphaPrev omegaPrev vPrev
            | k > maxIter = Left $ "BiCGSTAB(" ++ show l ++ "): no convergence in " ++ show maxIter ++ " iterations"
            | norm2 (trueRes x) <= relTol = Right x
            | otherwise =
                let rho = dot rhat r
                 in if abs rho < 1e-30
                        then
                            -- hard breakdown: do residual replacement and continue
                            let (r1, rhat1, rhoP, omegaP, p1, v1) = residualReplace x
                             in go (k + 1) 0 x r1 p1 rhoP 1.0 omegaP v1
                        else
                            let beta = (rho / rhoPrev) * (alphaPrev / omegaPrev)
                                p' = vectorAdd r (scale beta (vectorSub p (scale omegaPrev vPrev)))
                                v = amat p'
                                rhatDotV = dot rhat v
                             in if abs rhatDotV < 1e-30
                                    then
                                        -- soft breakdown (your error): swap shadow & restart via residual replacement
                                        let (r1, rhat1, rhoP, omegaP, p1, v1) = residualReplace x
                                         in go (k + 1) 0 x r1 p1 rhoP 1.0 omegaP v1
                                    else
                                        let alpha = rho / rhatDotV
                                            xh = vectorAdd x (scale alpha p')
                                            s0 = vectorSub r (scale alpha v)
                                         in if norm2 (trueRes xh) <= relTol || norm2 s0 <= relTol
                                                then Right xh
                                                else
                                                    let (xAfter, rNew, omegaLast) = mrLoop l xh s0
                                                        sinceRR' = sinceRR + 1
                                                        -- periodic residual replacement to prevent drift (every 50 outer steps)
                                                        (xAfter2, rUse, rhatUse, rhoUse, omegaUse, pUse, vUse, sinceRR2)
                                                            | sinceRR' >= 10 =
                                                                let (r2, rhat2, rhoP2, omegaP2, p2, v2) = residualReplace xAfter
                                                                 in (xAfter, r2, rhat2, rhoP2, omegaP2, p2, v2, 0)
                                                            | otherwise = (xAfter, rNew, rhat, rho, omegaLast, p', v, sinceRR')
                                                     in if abs omegaUse < 1e-30
                                                            then
                                                                let (r3, rhat3, rhoP3, omegaP3, p3, v3) = residualReplace xAfter2
                                                                 in go (k + 1) 0 xAfter2 r3 p3 rhoP3 1.0 omegaP3 v3
                                                            else
                                                                go (k + 1) sinceRR2 xAfter2 rUse pUse rhoUse alpha omegaUse vUse
     in if norm2 r0_true <= relTol
            then Right x0
            else go 1 0 x0 r0 p0 rho0 alpha0 omega0 v0

-- | Solve linear system (I - A) * x = b using BiCGSTAB
solveSparseLinearSystem :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystem techTriples n demandVec =
    let _ = trace ("SPARSE SOLVER: Building " ++ show n ++ "x" ++ show n ++ " CRS system matrix") ()

        -- Build (I - A) system from sparse triplets
        -- Add identity matrix: I[i,i] = 1.0
        identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        -- Subtract technosphere: (I - A)[i,j] = I[i,j] - A[i,j]
        negTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        allTriples = identityTriples ++ negTechTriples

        _ = trace ("SPARSE SOLVER: Converting to CRS matrix format...") ()
        systemCRS = buildCRSMatrix allTriples n n

        _ = trace ("SPARSE SOLVER: Starting BiCGSTAB solve...") ()
        tolerance = 1e-8
        maxIterations = min 2000 (3 * n)
        initialGuess = fromList $ replicate n 0.0
        lDegree = 4 -- try 2 or 4
     in case solveBiCGSTABL systemCRS demandVec initialGuess tolerance maxIterations lDegree of
            Right solution ->
                let _ = trace ("SPARSE SOLVER: BiCGSTAB completed successfully") ()
                 in solution
            Left errorMsg ->
                let _ = trace ("SPARSE SOLVER ERROR: " ++ errorMsg) ()
                 in error $ "BiCGSTAB solver failed: " ++ errorMsg

-- | Efficient sparse matrix multiplication: result = A * vector
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector -> Vector
applySparseMatrix sparseTriples nRows inputVec =
    let crs = buildCRSMatrix sparseTriples nRows nRows
     in sparseMatVec crs inputVec

{- | Matrix-based inventory calculation using sparse approach
Uses pre-built sparse triplets, then solves: (I - A)^-1 * f
Where: I = identity, A = technosphere matrix, f = demand vector
-}
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID =
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db

        _ = trace ("=== MATRIX INVENTORY CALCULATION START ===") ()
        _ = trace ("Sparse matrix calculation for " ++ show activityCount ++ " activities, " ++ show bioFlowCount ++ " biosphere flows") ()

        -- Use pre-built sparse coordinate lists from database
        _ = trace ("Extracting pre-built sparse matrices from database...") ()
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowUUIDs = dbBiosphereFlows db

        _ = trace ("Tech matrix has " ++ show (length techTriples) ++ " non-zero elements") ()
        _ = trace ("Bio matrix has " ++ show (length bioTriples) ++ " non-zero elements") ()

        -- Build demand vector for root activity
        _ = trace ("Building demand vector for root activity...") ()
        demandVec = buildDemandVectorFromIndex activityIndex rootUUID

        -- Solve sparse LCA equation: (I - A) * supply = demand
        _ = trace ("Starting sparse linear system solving...") ()
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        _ = trace ("Applying biosphere matrix multiplication...") ()
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

        _ = trace ("Converting result to Map format...") ()
        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
        _ = trace ("=== MATRIX INVENTORY CALCULATION COMPLETE ===") ()
     in result

{- | Build final demand vector f using pre-built activity index
f[i] = external demand for product from activity i
-}
buildDemandVectorFromIndex :: M.Map UUID Int -> UUID -> Vector
buildDemandVectorFromIndex activityIndex rootUUID =
    let n = M.size activityIndex
        rootIndex = fromMaybe 0 (M.lookup rootUUID activityIndex)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]

{- | Build final demand vector f (legacy function)
f[i] = external demand for product from activity i
-}
buildDemandVector :: ActivityDB -> [UUID] -> UUID -> Vector
buildDemandVector activities actUUIDs rootUUID =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0 ..]
        rootIndex = fromMaybe 0 (M.lookup rootUUID indexMap)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]
