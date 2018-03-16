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
          , _size = Size 96 96
          , _speed = 3
          , _texture = rint
          }

update :: Input -> MainChar -> Result (MainChar, DL.DList Bullet -> DL.DList Bullet)
update input mc = do
  let
    move = keysToMovement (mc ^. speed) input

    addBullets
      | keyClicked KeyA input =
        DL.append $ DL.fromList [mkBullet (mc ^. texture) 8 0 (mc ^. pos)]
      | otherwise = id

        
  pure (over pos (`addPoint` move) mc, addBullets)

render :: SDL.Renderer -> MainChar -> IO ()
render renderer mc =
  SDL.copy renderer (mc ^. texture) Nothing (Just $ toRect (mc ^. pos) (mc ^. size))
