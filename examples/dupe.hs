{-# LANGUAGE QuasiQuotes, TupleSections #-}

{- dupe.hs: find duplicate files

This program finds duplicate files in the directory tree rooted at the
current working directory.

It demonstrates the goal of the shqq library: to combine the easy
command execution of traditional shell scripting with the strengths of
Haskell, such as concurrency and non-trivial data structures.

Depends on: shqq-0.1, spawn-0.3, split-0.1

Run it with:
  $ ghc -O -threaded -rtsopts dupe.hs
  $ ./dupe +RTS -N

-}

import Control.Concurrent.Spawn
import Data.List.Split
import System.Posix.Files
import System.ShQQ

import qualified Data.Map as M

-- Find potentially duplicate files based on some key, either size
-- or a checksum.
dupes :: (Ord k) => [(FilePath,k)] -> [[FilePath]]
dupes = filter (not . null . drop 1) . M.elems
      . foldr (\(v,k) -> M.insertWith (++) k [v]) M.empty

-- We want to collect data in parallel, but the OS will complain if we
-- have too many open files.  We limit each pass to have at most 256
-- tests in progress at once.
inParallel :: (a -> IO b) -> [a] -> IO [b]
inParallel f xs = do p <- pool 256; parMapIO (p . f) xs

main :: IO ()
main = do
    files <- endBy "\0" `fmap` [sh| find -type f -print0 |]

    -- Get the size of every file, as a first pass.
    let getSize f = ((f,) . fileSize) `fmap` getFileStatus f
    sizeDupes <- dupes `fmap` inParallel getSize files

    -- Checksum the files which have duplicated sizes.
    let getSha f = ((f,) . head . words) `fmap` [sh| sha1sum $f |]
    shaDupes  <- dupes `fmap` inParallel getSha (concat sizeDupes)

    -- Print duplicated file names, one per line, with a blank line
    -- after each group of duplicates.
    mapM_ (mapM_ putStrLn . (++[""])) shaDupes
