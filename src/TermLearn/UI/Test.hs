{-# LANGUAGE QuasiQuotes #-}
module TermLearn.UI.Test where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center
import Control.Lens
import Data.Char
import Data.Set qualified as S
import Data.Vector qualified as V
import Graphics.Vty qualified as VT
import PyF (fmt)
import TermLearn.Types
import TermLearn.UI.Logo

drawTest :: (?terms :: Terms) => Env -> [Widget ()]
drawTest (Test terms typed correct incorrect attr) = pure . vCenter . hCenter . padBottom (Pad 2) . hLimit (logoWidth + 6) $ vBox 
  [ hCenter . withBorderStyle unicodeRounded . border . padRight Max . vLimit (logoHeight + 6) $ vBox body
  , hCenter $ vBox help
  ]
 where
  total = V.length ?terms
  body = case terms of
    [] -> pure . hCenter . vCenter . str $ [fmt|{S.size correct - S.size incorrect} / {total} correct|]
    ((term, _) : _) -> 
      [ padLeft (Pad 1) $ str term
      , fill ' '
      , withAttr (attrName "selected") (str " > ") <+> withAttr attr (str $ reverse typed)
      ]
  help = case terms of 
    [] -> [str "q - quit"]
    _ -> 
      [ str "enter - submit answer"
      , str "tab   - reveal answer"
      ]

onTestEvent :: Env -> BrickEvent () () -> EventM () Env ()
onTestEvent = \case
  Test ((term, definition) : _) typed correct incorrect attr -> \case
    VtyEvent (VT.EvKey (VT.KChar c) []) | isPrint c -> do
      _Test . _2 %= (c :)
      _Test . _5 .= attrName ""
    VtyEvent (VT.EvKey VT.KBS []) -> do
      _Test . _2 %= drop 1
      _Test . _5 .= attrName ""
    VtyEvent (VT.EvKey (VT.KChar '\t') []) -> do
      _Test . _2 .= reverse definition
      _Test . _4 %= S.insert term
      _Test . _5 .= attrName "revealed"
    VtyEvent (VT.EvKey VT.KEnter []) -> if reverse typed == definition
      then do
        _Test . _1 %= tail
        _Test . _2 .= ""
        _Test . _3 %= S.insert term
        _Test . _5 .= attrName ""
      else do
        _Test . _4 %= S.insert term
        _Test . _5 .= attrName "incorrect"
    _ -> continueWithoutRedraw
  Test [] typed correct incorrect attr -> \case
    VtyEvent (VT.EvKey (VT.KChar 'q') []) -> id .= Select 0
    _ -> continueWithoutRedraw
