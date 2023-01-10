{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module TermLearn.UI (runApp) where

import Brick
import Data.Functor (void)
import Graphics.Vty qualified as VT
import TermLearn.Types
import TermLearn.UI.Flashcards
import TermLearn.UI.Match
import TermLearn.UI.Select
import TermLearn.UI.Test

draw :: (?terms :: Terms) => Env -> [Widget ()]
draw env = case env of
  Select {}  -> drawSelect env
  Flashcards {} -> drawFlashcards env
  Match {} -> drawMatch env
  Test {} -> drawTest env

onEvent :: (?terms :: Terms) => BrickEvent () () -> EventM () Env ()
onEvent event = do
  env <- get
  case env of
    Select {} -> onSelectEvent env event
    Flashcards {} -> onFlashcardsEvent env event
    Match {} -> onMatchEvent env event
    Test {} -> onTestEvent env event

runApp :: Terms -> IO ()
runApp terms = void . defaultMain app $ Select 0
 where
  app = let ?terms = terms in App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = onEvent
    , appStartEvent = pure ()
    , appAttrMap = const $ attrMap VT.defAttr 
      [ (attrName "selected", fg VT.brightGreen)
      , (attrName "revealed", fg VT.brightYellow)
      , (attrName "incorrect", fg VT.brightRed)
      ]
    }
