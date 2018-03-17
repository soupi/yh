{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module ShootingBox where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet
import qualified Play.Engine.ScrollingBackground as SBG


data MainChar
  = MainChar
  { _pos :: !Point
  , _size :: !Size
  , _speed :: !Int
  , _texture :: SDL.Texture
  , _hitTimer :: !Int
  }

makeFieldsNoPrefix ''MainChar

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("rin", "assets/rin.png")
  ]

mkMainChar :: [(String, SDL.Texture)] -> Result MainChar
mkMainChar ts = do
  case lookup "rin" ts of
    Nothing ->
      throwError ["Texture not found: rin"]
    Just rint ->
      pure $
        MainChar
          { _pos = Point 380 800
          , _size = Size 64 64
          , _speed = 4
          , _texture = rint
          , _hitTimer = -1
          }

update :: Input -> MainChar -> Result (MainChar, DL.DList Bullet -> DL.DList Bullet)
update input mc = do
  wsize <- _windowSize <$> SM.get
  let
    move = keysToMovement (mc ^. speed) input

    addBullets
      | keyClicked KeyA input =
        DL.append $ DL.fromList (newBullet mc)
      | otherwise = id

  pure
    ( mc
      & over pos (`addPoint` move)
      & fixPos wsize
      & set (size . sW) (if keyPressed KeyB input then 32 else 64)
    , addBullets
    )

newBullet :: MainChar -> [Bullet]
newBullet mc
  | mc ^. size . sW <= 32 =
    [ mkBullet (mc ^. texture) 8 5 100 ((mc ^. pos) `addPoint` Point (mc ^. size . sW `div` 2) 0)
    ]
  | otherwise =
    [ mkBullet (mc ^. texture) 8 5 100 ((mc ^. pos) `addPoint` Point (mc ^. size . sW `div` 4) 0)
    , mkBullet (mc ^. texture) 8 5 100 ((mc ^. pos) `addPoint` Point ((mc ^. size . sW `div` 4) * 3) 0)
    ]

render :: SDL.Renderer -> MainChar -> IO ()
render renderer mc = do
  SDL.textureBlendMode (mc ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (mc ^. texture) SDL.$= 255
  SDL.copy renderer (mc ^. texture) Nothing (Just $ toRect (mc ^. pos) (mc ^. size))
