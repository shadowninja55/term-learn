{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module TermLearn.UI.Match where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Map qualified as M
import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as S
import Data.Vector qualified as V
import Graphics.Vty qualified as VT
import PyF (fmt)
import System.CPUTime (getCPUTime)
import TermLearn.Types
import TermLearn.UI.Logo
import TermLearn.Util (shuffleIO)
 
drawMatch :: Env -> [Widget ()]
drawMatch (Match terms definitions selected correct start end) = pure . vCenter . hCenter . padBottom (Pad 4) $ vBox 
  [ hCenter . str $ maybe " " (\n -> show $ fromIntegral (n - start) / 1e12) end
  , hCenter . withBorderStyle unicodeRounded . border . padLeftRight 1 $ vBox rows
  ]
 where
  rows = left <> [str " "] <> right
  left = zipWith render ['1'..'5'] $ V.toList terms
  right = zipWith render (['6'..'9'] <> "0") $ V.toList definitions
  check s = if s `S.member` correct then '✓' else ' '
  highlight n = case selected of
    (Just x, Nothing) -> if digitToInt n == x then withAttr (attrName "selected") else id
    _ -> id
  render n s = highlight n $ str [fmt|[{n}] {s}{replicate (logoWidth - length s - 3) ' '}{check s}|]

onMatchEvent :: (?terms :: Terms) => (V.Vector String, V.Vector String, (Maybe Int, Maybe Int), S.Set String, Integer, Maybe Integer) -> BrickEvent () () -> EventM () Env ()
onMatchEvent (terms, definitions, selected, correct, start, end) = \case
  VtyEvent (VT.EvKey (VT.KChar 'q') []) -> id .= Select 0
  VtyEvent (VT.EvKey (VT.KChar c) []) | isDigit c -> let n = digitToInt c in do
    selected' <- case selected of
      (Nothing, Nothing) -> pure (Just n, Nothing)
      (Just x, Nothing) -> do
        let 
         term = terms V.! (x - 1)
         definition = definitions V.! if n == 0 then 4 else n - 6
        when (definition == fromJust (lookup term $ V.toList ?terms)) $
          _Match . _4 %= S.union (S.fromList [term, definition])
        pure (Just x, Just n)
      (Just _, Just _) -> pure (Just n, Nothing)
    correct' <- use $ _Match . _4
    if S.size correct' < 10
      then _Match . _3 .= selected'
      else when (isNothing end) do
        end' <- liftIO getCPUTime
        _Match . _6 ?= end'
  _ -> continueWithoutRedraw
