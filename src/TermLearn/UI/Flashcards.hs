{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module TermLearn.UI.Flashcards (drawFlashcards, onFlashcardsEvent) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center
import Control.Lens
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Tuple (swap)
import Data.Vector qualified as V
import Graphics.Vty qualified as VT
import PyF (fmt)
import TermLearn.Types
import TermLearn.UI.Logo (logoHeight, logoWidth)
import TermLearn.Util (shuffleIO)

drawFlashcards :: Env -> [Widget ()]
drawFlashcards (Flashcards cards card flipped) = pure . vCenter . hCenter . padTop (Pad 1) . hLimit (logoWidth + 6) $ vBox 
  [ hCenter $ str [fmt|[ {card + 1} / {V.length cards} ]|]
  , withBorderStyle unicodeRounded . border . vLimit (logoHeight + 6) . padBottom Max . padLeftRight 1 . strWrap . front flipped $ cards V.! card
  , hCenter $ vBox 
    [ str "j     - next card"
    , str "k     - previous card"
    , str "space - flip card"
    , str "s     - shuffle cards"
    , str "f     - flip all cards"
    , str "q     - quit"
    ]
  ]
 where
  front = bool fst snd

onFlashcardsEvent :: (?terms :: Terms) => Env -> BrickEvent () () -> EventM () Env ()
onFlashcardsEvent (Flashcards cards card flipped) = \case
  VtyEvent (VT.EvKey (VT.KChar 'k') []) -> do
    _Flashcards . _2 %= max 0 . pred
    _Flashcards . _3 .= False
  VtyEvent (VT.EvKey (VT.KChar 'j') []) -> do
    _Flashcards . _2 %= min (V.length cards - 1) . succ
    _Flashcards . _3 .= False
  VtyEvent (VT.EvKey (VT.KChar 'f') []) -> _Flashcards . _1 . each %= swap
  VtyEvent (VT.EvKey (VT.KChar 's') []) -> do
    shuffled <- liftIO $ shuffleIO cards
    _Flashcards . _1 .= shuffled
    _Flashcards . _2 .= 0
    _Flashcards . _3 .= False
  VtyEvent (VT.EvKey (VT.KChar ' ') []) -> _Flashcards . _3 %= not
  VtyEvent (VT.EvKey (VT.KChar 'q') []) -> id .= Select 0
  _ -> continueWithoutRedraw
