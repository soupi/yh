{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module DecObj.SplitTexture where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings
import Control.Lens

import qualified DecObj.Particle as Part
import qualified DecorationObject as DecObj
import qualified Play.Engine.Movement as MV


data MakeSplitTexture
  = MakeSplitTexture
  { mkPos :: {-# UNPACK #-} !IPoint
  , mkSize :: {-# UNPACK #-} !Size
  , mkSplit :: {-# UNPACK #-} !IPoint
  , mkTexture :: !SDL.Texture
  , mkTexturePos :: {-# UNPACK #-} !IPoint
  , mkDeathTime :: {-# UNPACK #-} !Int
  }

make :: MakeSplitTexture -> Result [DecObj.DecorationObject]
make MakeSplitTexture{..} = do
  let
    (w, h) =
      ( mkSize ^. x `div` max 1 (mkSplit ^. x)
      , mkSize ^. y `div` max 1 (mkSplit ^. y)
      )
    dirs =
      map (\((*) (pi / 180) -> d) -> Point (cos d) (sin d))
        . mkAngles 135
        $ mkSplit ^. x * mkSplit ^. y
    zipped =
      zip
        [ (a, b)
        | a <- [0..(mkSplit ^. x - 1)]
        , b <- [0..(mkSplit ^. y - 1)]
        ]
        dirs
  pure $ flip map zipped $ \((x',y'), dir) ->
    Part.toDecObj $ Part.make $ Part.MakeParticle
      { Part.mkPos = mkPos `addPoint` Point (x' * w) (y' * h)
      , Part.mkSize = Point w h
      , Part.mkDir = dir
      , Part.mkMov = MV.fastGradualStart $ Point 0.6 0.6 `mulPoint` dir
      , Part.mkTexture = mkTexture
      , Part.mkTexturePos = mkTexturePos `addPoint` Point (x' * w) (y' * h)
      , Part.mkTextureSize = Point w h
      , Part.mkDeathTime = mkDeathTime
      }
