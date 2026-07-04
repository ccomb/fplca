-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2024-present Christophe Combelles

module Numerical.MUMPS.Types (
    MUMPSSolver (..),
) where

import Foreign.Ptr (Ptr)
import Numerical.MUMPS.FFI (MumpsSolverC)

-- | Handle to a MUMPS solver instance. Owns the C-level memory.
data MUMPSSolver = MUMPSSolver
    { solverPtr :: !(Ptr MumpsSolverC)
    , solverSize :: !Int
    -- ^ Matrix dimension n
    }

instance Show MUMPSSolver where
    show s = "MUMPSSolver{n=" ++ show (solverSize s) ++ "}"
