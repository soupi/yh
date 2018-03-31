{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module TextBox where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Tuple
import Data.Word
import Data.Maybe
import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings
import Play.Engine.Input
import Control.Lens
import Control.DeepSeq
import Control.Monad.Except
import qualified Linear
import qualified Control.Monad.State as SM
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Play.Engine.Movement as MV

import Debug.Trace

data Loc
  = Up
  | Down

data TextBox
  = TextBox
  { _avatar :: Maybe SDL.Texture
  , _font :: SDLF.Font
  , _text :: T.Text
  , _posY :: !Loc
  , _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !IPoint
  , _textPart :: Int
  , _textSpeed :: Int
  , _textTimer :: Int
  }

makeFieldsNoPrefix ''TextBox

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("unispace", MySDL.Font "assets/fonts/unispace/unispace.ttf")
  ]

make :: Loc -> Int -> T.Text -> Maybe SDL.Texture -> Maybe SDLF.Font -> Result TextBox
make _ _ _ _ Nothing =
  throwError ["A font was not provided to TextBox."]
make py spd txt mavatar (Just fnt) =
  pure $ TextBox
    { _avatar = mavatar
    , _font = fnt
    , _text = txt
    , _posY = py
    , _pos = Point 0 0
    , _size = Point 0 0
    , _textPart = 0
    , _textSpeed = spd
    , _textTimer = spd
    }

update :: Input -> TextBox -> Result (Maybe TextBox)
update input tb
  | keyPressed KeyA input = pure Nothing
  | otherwise = do
    wSize <- _windowSize <$> SM.get
    let
      locY = case tb ^. posY of
        Up -> 20
        Down -> (wSize ^. y - 220)

    pure $ pure $ tb
      & set pos (Point 20 locY)
      & set size (Point (wSize ^. x - 40) 200)


render :: SDL.Renderer -> TextBox -> IO ()
render renderer tb
  | tb ^. size == Point 0 0 =
    pure ()
  | otherwise = do
  let
    rect = toRect (tb ^. pos) (tb ^. size)

  SDL.rendererDrawColor renderer SDL.$= Linear.V4 0 0 0 150
  SDL.fillRect renderer (Just rect)
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 100 130 180 200
  SDL.drawRect renderer (Just rect)

  text <- SDL.createTextureFromSurface renderer
    =<< SDLF.solid (tb ^. font) (Linear.V4 255 255 255 255) (tb ^. text)
  ti <- SDL.queryTexture text
  SDL.copy
    renderer
    text
    Nothing
    (Just $ toRect
      (fmap (+20) $ tb ^. pos)
      (Point (fromIntegral $ SDL.textureWidth ti) (fromIntegral $ SDL.textureHeight ti))
    )
  SDL.destroyTexture text
