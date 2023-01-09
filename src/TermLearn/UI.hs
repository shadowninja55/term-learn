{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module TermLearn.UI (runApp) where

import Brick
import Control.Monad (join)
import Data.Functor (void)
import Graphics.Vty qualified as VT
import TermLearn.Types
import TermLearn.UI.Flashcards
import TermLearn.UI.Select
import TermLearn.UI.Match

draw :: Env -> [Widget ()]
draw env = case env of
  Select {}  -> drawSelect env
  Flashcards {} -> drawFlashcards env
  Match {} -> drawMatch env
  _ -> pure $ str "nyi"

onEvent :: (?terms :: Terms) => BrickEvent () () -> EventM () Env ()
onEvent event = join $ gets \case
  Select mode -> onSelectEvent mode event
  Flashcards cards card flipped -> onFlashcardsEvent (cards, card, flipped) event
  Match terms definitions selected correct start end -> onMatchEvent (terms, definitions, selected, correct, start, end) event
  _ -> pure ()

runApp :: Terms -> IO ()
runApp terms = void . defaultMain app $ Select 0
 where
  app = App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = let ?terms = terms in onEvent
    , appStartEvent = pure ()
    , appAttrMap = const $ attrMap VT.defAttr 
      [ (attrName "selected", fg VT.brightGreen)
      ]
    }
