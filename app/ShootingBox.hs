{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ShootingBox where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Data.Maybe
import Control.Monad.Except
import Control.Lens
import Control.DeepSeq
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL
import qualified Data.Map as M

import qualified Play.Engine.Movement as MV
import qualified Play.Engine.Sprite as Spr
import Bullet


data MainChar
  = MainChar
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _sprite :: !Spr.Sprite
  , _hitTimer :: {-# UNPACK #-} !Int
  , _bulletsTimer :: {-# UNPACK #-} !Int
  , _health :: {-# UNPACK #-} !Int
  }

makeFieldsNoPrefix ''MainChar

instance NFData MainChar where
  rnf (MainChar {_pos, _size, _hitTimer, _movement, _health, _bulletsTimer}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _direction
    `seq` rnf _movement
    `seq` rnf _hitTimer
    `seq` rnf _bulletsTimer
    `seq` rnf _health
    `seq` rnf _transparency

instance Eq MainChar where
  mc1 == mc2 =
    mc1 ^. pos == mc2 ^. pos
    && mc1 ^. size == mc2 ^. size

instance Ord MainChar where
  mc1 <= mc2 =
    mc1 ^. pos <= mc2 ^. pos
    && mc1 ^. size <= mc2 ^. size

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("rin-sprites", MySDL.Texture "rin_spritemap.png")
  ]


mkMainChar :: M.Map String SDL.Texture -> Result MainChar
mkMainChar ts = do
  case M.lookup "rin-sprites" ts of
    Nothing ->
      throwError ["Texture not found: rin-sprites"]
    Just rint ->
      pure $
        MainChar
          { _pos = Point 380 800
          , _size = charSize
          , _sprite =
            fromJust
              $ Spr.make
              $ Spr.MakeArgs
              { mkActionmap = M.fromList [("normal", 0)]
              , mkAction = "normal"
              , mkTexture = rint
              , mkSize = Point 300 300
              , mkMaxPos = 1
              , mkSpeed = 60
              }
          , _hitTimer = -1
          , _bulletsTimer = 5
          , _health = 1
          , _movement = MV.make $ MV.defArgs
            { MV.maxspeed = Point 5 5
            , MV.accel = Point 3.5 3.5
            }
          }

charSize :: Size
charSize = Point 32 48

update :: Input -> MainChar -> Result (MainChar, DL.DList Bullet -> DL.DList Bullet)
update input mc = do
  wsize <- _windowSize <$> SM.get
  let
    dir = keysToMovement 1 input
    (mv, move) =
      MV.update dir
        . set MV.maxSpeed (if keyPressed KeyB input then Point 1.2 1.2 else Point 4 4)
        $ (mc ^. movement)

    addBullets
      | keyPressed KeyA input
      , mc ^. bulletsTimer == 0 =
        DL.append $ DL.fromList (newBullet mc)
      | otherwise = id

    newMC =
      mc
      & over pos (`addPoint` move)
      & fixPos wsize
      & set (size . x) (if keyPressed KeyB input then charSize ^. x `div` 2 else charSize ^. x)
      & set movement mv
      & over sprite (Spr.update Nothing False)
      & over hitTimer (\t -> if t <= 0 then -1 else t - 1)
      & over bulletsTimer (\t -> if t > 0 then t - 1 else if keyPressed KeyA input then 5 else 0)

    result =
      if mc ^. health <= 0 && mc ^. hitTimer < 0
        then (set size (Point 0 0) mc, id)
        else (newMC, addBullets)

  pure result

newBullet :: MainChar -> [Bullet]
newBullet mc
  | mc ^. size . x == charSize ^. x =
    [ mkBullet (mc ^. sprite . Spr.texture) (Point 0 (-1)) mv 2 100 ((mc ^. pos) `addPoint` Point (mc ^. size . x `div` 4) 0)
    , mkBullet (mc ^. sprite . Spr.texture) (Point 0 (-1)) mv 2 100 ((mc ^. pos) `addPoint` Point ((mc ^. size . x `div` 4) * 3) 0)
    ]
  | otherwise =
    [ mkBullet (mc ^. sprite . Spr.texture) (Point 0 (-1)) mv 5 100 ((mc ^. pos) `addPoint` Point (mc ^. size . x `div` 2) 0)
    ]

  where
    mv = MV.make $ MV.defArgs
      { MV.maxspeed = Point 0 10
      , MV.accel = Point 0 10
      }

checkHit :: DL.DList Bullet -> MainChar -> MainChar
checkHit bullets mc
  | any (isJust . isTouching mc) bullets && mc ^. health > 0
  = mc
    & over health (flip (-) (DL.head bullets ^. damage))
    & \enemy' -> set hitTimer (if enemy' ^. health <= 0 then hitTimeout * 4 else hitTimeout) enemy'
  | otherwise
  = mc

hitTimeout = 20

render :: SDL.Renderer -> Camera -> MainChar -> IO ()
render renderer cam mc =
  unless (mc ^. health < 0 && mc ^. hitTimer < 0) $ do
    let
      rect = toRect (cam $ mc ^. pos) (mc ^. size)
      h = fromIntegral $ mc ^. health * 3
    if mc ^. hitTimer > 0 && mc ^. hitTimer `mod` 10 < 5
    then do
      SDL.rendererDrawColor renderer SDL.$= Linear.V4 (255 - h) (255 - h) 255 255
      SDL.drawRect renderer (Just rect)
      SDL.fillRect renderer (Just rect)
    else do
      Spr.render renderer cam (mc ^. pos) (mc ^. size) (mc ^. sprite)
      -- SDL.textureBlendMode (mc ^. texture) SDL.$= SDL.BlendAlphaBlend
      -- SDL.textureAlphaMod  (mc ^. texture) SDL.$= 255
      -- SDL.copy renderer (mc ^. texture) Nothing (Just rect)

get mc l
  | mc ^. health <= 0 && mc ^. hitTimer < 0 = Nothing
  | otherwise = pure $ mc ^. l
