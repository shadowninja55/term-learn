module TermLearn.Util where

import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import System.Random (randomRIO)

shuffleIO :: V.Vector a -> IO (V.Vector a)
shuffleIO v = do
  let n = V.length v
  mv <- V.thaw v
  for_ [0 .. n - 2] \i -> do
    j <- randomRIO (i, n - 1)
    MV.swap mv i j
  V.unsafeFreeze mv
