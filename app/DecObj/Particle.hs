{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module DecObj.Particle where

import qualified SDL
import SDL.Vect (Point(P))

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Lens
import GHC.Generics
import Control.DeepSeq
import qualified Control.Monad.State as SM

import qualified DecorationObject as DecObj
import qualified Play.Engine.Movement as MV

data Particle
  = Particle
  { _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !Size
  , _movement :: {-# UNPACK #-} !MV.Movement
  , _direction :: {-# UNPACK #-} !FPoint
  , _texture :: SDL.Texture
  , _texturePos :: {-# UNPACK #-} !IPoint
  , _textureSize :: {-# UNPACK #-} !IPoint
  , _deathTime :: {-# UNPACK #-} !Int
  , _timers :: {-# UNPACK #-} !Timers
  }

data Timers
  = Timers
  { _deathAnimationTimer :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show, Generic, NFData)


instance NFData Particle where
  rnf (Particle {..}) =
    rnf _pos
    `seq` rnf _size
    `seq` rnf _movement
    `seq` rnf _direction
    `seq` rnf _texturePos
    `seq` rnf _textureSize
    `seq` rnf _deathTime
    `seq` rnf _timers

makeFieldsNoPrefix ''Particle
makeFieldsNoPrefix ''Timers

data MakeParticle
  = MakeParticle
  { mkPos :: {-# UNPACK #-} !IPoint
  , mkSize :: {-# UNPACK #-} !Size
  , mkDir :: {-# UNPACK #-} !FPoint
  , mkMov :: {-# UNPACK #-} !MV.Movement
  , mkTexture :: !SDL.Texture
  , mkTexturePos :: {-# UNPACK #-} !IPoint
  , mkTextureSize :: {-# UNPACK #-} !IPoint
  , mkDeathTime :: {-# UNPACK #-} !Int
  }

make :: MakeParticle -> Particle
make MakeParticle{..} =
  Particle
    { _pos = mkPos
    , _size = mkSize
    , _direction = mkDir
    , _movement = mkMov
    , _texture = mkTexture
    , _texturePos = mkTexturePos
    , _textureSize = mkTextureSize
    , _deathTime = mkDeathTime
    , _timers = initEnemyTimers mkDeathTime
    }

toDecObj :: Particle -> DecObj.DecorationObject
toDecObj part = DecObj.make
  part
  (\i p -> (,[]) <$> update i p)
  renderFading

initEnemyTimers :: Int -> Timers
initEnemyTimers death = Timers
  { _deathAnimationTimer = death
  }

update :: Input -> Particle -> Result (Maybe Particle)
update _ part = do
  wsize <- _windowSize <$> SM.get
  let
    (mv, move) =
      MV.update (part ^. direction)
        $ (part ^. movement)

    part' =
      part
      & over pos (`addPoint` move)
      & set movement mv
      & set timers (updateTimers part)

  pure
    ( if part' ^. timers . deathAnimationTimer <= 0
        || not (isInWindow wsize (part ^. pos) (part ^. size))
        then Nothing
        else pure part'
    )

updateTimers :: Particle -> Timers
updateTimers part =
  (part ^. timers)
    & over deathAnimationTimer
      (\t ->
        if
          | t >= 0 -> t - 1
          | otherwise -> -1
      )

renderFading :: SDL.Renderer -> Camera -> Particle -> IO ()
renderFading renderer cam part = do
  SDL.textureBlendMode (part ^. texture) SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod  (part ^. texture) SDL.$=
    ( floor @Float
      $ (255 *)
      $ fromIntegral (part ^. timers . deathAnimationTimer)
      / fromIntegral (max (part ^. deathTime) 1)
    )
  let
    rect = toRect (cam $ part ^. pos) (part ^. size)
    clip = SDL.Rectangle (P $ part ^. texturePos) (part ^. size)
  SDL.copy renderer (part ^. texture) (Just $ fmap fromIntegral clip) (Just rect)
  SDL.textureAlphaMod  (part ^. texture) SDL.$= 255
