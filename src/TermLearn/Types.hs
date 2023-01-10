{-# LANGUAGE TemplateHaskell #-}
module TermLearn.Types where

import Control.Lens (makePrisms)
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Vector (Vector)

data Env
  = Select Int
  | Flashcards Terms Int Bool
  | Match (Vector String) (Vector String) (Maybe Int, Maybe Int) (Set String) UTCTime (Maybe UTCTime)
  | Test

type Terms = Vector (String, String)

makePrisms ''Env
