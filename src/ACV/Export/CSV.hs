module ACV.Export.CSV (exportInventoryAsCSV) where

import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import qualified Data.Map as M
import qualified Data.Text as T
import ACV.Matrix (Inventory)
import ACV.Types

-- | Exporte l'inventaire en CSV : colonnes flow_id, amount
exportInventoryAsCSV :: FilePath -> Inventory -> IO ()
exportInventoryAsCSV path inv = withFile path WriteMode $ \h -> do
  hPutStrLn h "flow_id,amount"
  mapM_ (\(fid, amt) -> hPutStrLn h (T.unpack fid ++ "," ++ show amt)) (M.toList inv)
