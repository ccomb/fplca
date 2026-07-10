{- | Deterministic in-memory zip packaging.

Entry modification times are pinned to epoch 0 so the archive bytes are
reproducible across runs. Paths must be unique: every caller derives them from a
UUID, and a duplicate would already be a writer bug.

Building 'zEntries' directly rather than folding 'addEntryToArchive' is what
keeps this linear. That function deletes any entry sharing the new path before
consing, so it filters the whole entry list on each insert: a fold over @n@
entries costs O(n²) 'FilePath' comparisons and stacks @n@ lazy filters before
anything is forced. On a 53 508-file ILCD package that is enough to exhaust
memory and blow the HTTP timeout.
-}
module Zip (zipFiles) where

import Codec.Archive.Zip (Archive (..), emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{- | Pack @(path, bytes)@ entries into a zip archive, in the order given.

Linear in the number of entries. Paths use forward slashes on every OS, as the
zip format mandates; callers are responsible for that and for uniqueness.
-}
zipFiles :: [(FilePath, BS.ByteString)] -> BL.ByteString
zipFiles files =
    fromArchive emptyArchive{zEntries = [toEntry path 0 (BL.fromStrict bytes) | (path, bytes) <- files]}
