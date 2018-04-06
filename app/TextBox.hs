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

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings
import Play.Engine.Input
import Control.Lens
import Control.Monad.Except
import qualified Linear
import qualified Control.Monad.State as SM
import qualified Data.Text as T


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
  | keyClicked KeyA input
  , (tb ^. textPart) == T.length (tb ^. text)
  = pure Nothing
  | otherwise = do
    wSize <- _windowSize <$> SM.get
    let
      locY = case tb ^. posY of
        Up -> 20
        Down -> (wSize ^. y - 220)

    pure $ pure $ tb
      & set pos (Point 20 locY)
      & set size (Point (wSize ^. x - 40) 200)
      & over textPart
        (\tp ->
           if
             | keyClicked KeyA input -> T.length (tb ^. text)
             | tb ^. textSpeed == 0 -> T.length (tb ^. text)
             | tb ^. textTimer == 0 && tp < T.length (tb ^. text) -> tp+1
             | otherwise -> tp
        )
      & over textTimer (\t -> if t <= 0 then tb ^. textSpeed else t - 1)


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

  if tb ^. textPart <= 0
    then pure ()
    else do
      loc <- case tb ^. avatar of
        Nothing -> pure (fmap (+20) $ tb ^. pos)
        Just av -> do
          SDL.copy
            renderer
            av
            Nothing
            (Just $ toRect
              (fmap (+20) $ tb ^. pos)
              (Point 96 96)
            )
          pure (Point (20 + tb ^. pos . x + 96 + 20) (tb ^. pos . y + 20))

      txt <- SDL.createTextureFromSurface renderer
        =<< SDLF.solid (tb ^. font) (Linear.V4 255 255 255 255) (T.take (tb ^. textPart) (tb ^. text))
      ti <- SDL.queryTexture txt
      SDL.copy
        renderer
        txt
        Nothing
        (Just $ toRect
          loc
          (Point (fromIntegral $ SDL.textureWidth ti) (fromIntegral $ SDL.textureHeight ti))
        )
      SDL.destroyTexture txt
