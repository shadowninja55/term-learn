{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module TermLearn.UI.Select (drawSelect, onSelectEvent) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Time (getCurrentTime)
import Data.Vector qualified as V
import Graphics.Vty qualified as VT
import System.CPUTime (getCPUTime)
import TermLearn.Types
import TermLearn.UI.Logo
import TermLearn.Util (shuffleIO)

modes :: [String]
modes = ["flashcards", "match", "test"]

drawSelect :: Env -> [Widget ()]
drawSelect (Select mode) = pure . vCenter . hCenter $ vBox
  [ hCenter $ withBorderStyle unicodeRounded . border . padBottom (Pad 1) . padLeftRight 2 $ vBox
    [ padBottom (Pad 2) $ str logo
    , vBox . zipWith style [0..] $ ("  " <>) <$> modes
    ]
    , hCenter $ vBox 
      [ str "j     - next mode"
      , str "k     - previous mode"
      , str "space - select mode"
      , str "q     - quit"
      ]
  ]
 where
  style n
    | n == mode = withAttr (attrName "selected") . str . ('>' :) . tail
    | otherwise = str

onSelectEvent :: (?terms :: Terms) => Int -> BrickEvent () () -> EventM () Env ()
onSelectEvent mode = \case
  VtyEvent (VT.EvKey (VT.KChar 'k') []) -> _Select %= max 0 . pred
  VtyEvent (VT.EvKey (VT.KChar 'j') []) -> _Select %= min (length modes - 1) . succ
  VtyEvent (VT.EvKey (VT.KChar ' ') []) -> put =<< case mode of
    0 -> pure $ Flashcards ?terms 0 False
    1 -> liftIO do
      five <- shuffleIO ?terms
      terms <- shuffleIO $ fst <$> five
      definitions <- shuffleIO $ snd <$> five
      start <- getCurrentTime
      pure $ Match terms definitions (Nothing, Nothing) S.empty start Nothing
    2 -> pure Test
    _ -> error $ "impossible mode selected (" <> show mode <> ")"
  VtyEvent (VT.EvKey (VT.KChar 'q') []) -> halt
  _ -> continueWithoutRedraw
