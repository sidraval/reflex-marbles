{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Reflex.Dom
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import qualified Data.ByteString as BS

main :: IO ()
main = do
  mainWidget $ do
    elAttr "link" (Map.fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "main.css")]) blank
    gateExample
    return ()

gateExample :: forall t m. (MonadWidget t m) => m ()
gateExample = do
  el "div" $ do
    rec (inputContainer, marbleD) <- elClass' "div" "marbles-container" $ do
        holdDyn 0 =<< inputMarble inputContainer
    (_, (checkE, checkB)) <- elClass' "div" "gate-container" $ do
      checkE' <- _checkbox_change <$> checkbox False def
      checkB' <- hold False checkE'
      return (checkE', checkB')
    elClass' "div" "marbles-container" $ do
      widgetHold blank $ fmap (\t -> case t of True -> outputMarble marbleD $ (gate checkB $ updated marbleD); False -> blank) checkE
    return ()
  return ()

intToAdjustedString :: Int -> Text
intToAdjustedString = T.pack . show . (+ (-25))

inputMarble :: forall t m. (MonadWidget t m) => Element EventResult GhcjsDomSpace t -> m (Event t Int)
inputMarble container = do
  rec (div, _) <- elDynAttr' "div" attrsD  $ blank
      mousedownE <- hold False $ leftmost [True <$ domEvent Mousedown div, False <$ domEvent Mouseup container]
      let moveE = gate mousedownE $ fmap fst $ leftmost [domEvent Mousemove div, domEvent Mousemove container]
          attrsE = fmap coordToMarbleAttrs moveE
      attrsD <- holdDyn (Map.singleton "class" "marble") attrsE
  return moveE

outputMarble :: forall t m. (MonadWidget t m) => Dynamic t Int -> Event t Int -> m ()
outputMarble initialD moveE = do
  rec _ <- elDynAttr' "div" attrsD $ blank
      let attrsE = fmap coordToMarbleAttrs moveE
      initialL <- sample . current $ initialD
      attrsD <- holdDyn (coordToMarbleAttrs initialL) attrsE
  return ()


coordToMarbleAttrs :: Int -> Map Text Text
coordToMarbleAttrs x = Map.fromList [("style", "left:" <> (T.pack . show . (+ (-25)) $ x) <> "px;"), ("class", "marble")]
