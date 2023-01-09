module Main where

import TermLearn

main :: IO ()
main = do
  Just terms <- parseConfig <$> readFile "terms.txt"
  runApp terms
