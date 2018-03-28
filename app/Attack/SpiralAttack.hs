{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Attack.SpiralAttack where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Types
import Control.Lens
import Control.DeepSeq
import qualified Data.DList as DL

import Bullet
import Attack
import qualified Attack
import qualified Movement as MV


make :: Int -> (Int, Float) -> SDL.Texture -> Attack
make numOfOutPoints (every, change) txt =
  Attack.make
    (MV.make (Point 1 1) (Point 3 3))
    (mkAngles numOfOutPoints)
    every
    txt
    spiralUpdate

  where
    spiralUpdate :: IPoint -> IPoint -> Attack -> (DL.DList Bullet, Attack)
    spiralUpdate posi sz attack =
      Attack.update posi sz attack
        & over (_2 . outAngles) (map $ (+) change)

    mkAngles :: Int -> [Float]
    mkAngles ((`mod` 100) -> n) =
      let
        m = 360 / fromIntegral n
      in
        force $ map ((*) m . fromIntegral) [1..n]
