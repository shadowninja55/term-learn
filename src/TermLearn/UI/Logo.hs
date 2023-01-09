{-# LANGUAGE TemplateHaskell #-}
module TermLearn.UI.Logo where

import Data.FileEmbed (embedStringFile)

logo :: String
logo = $(embedStringFile "logo.txt")

logoHeight :: Int
logoHeight = length $ lines logo

logoWidth :: Int
logoWidth = length . head $ lines logo
