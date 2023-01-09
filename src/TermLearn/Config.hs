module TermLearn.Config (parseConfig) where

import Data.Vector qualified as V
import TermLearn.Types

parseConfig :: String -> Maybe Terms
parseConfig = fmap V.fromList . pairs . filter (not . null) . lines
 where
  pairs [] = Just []
  pairs [_] = Nothing
  pairs (x : y : xs) = ((x, y) :) <$> pairs xs
