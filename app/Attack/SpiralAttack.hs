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


make :: Int -> Int -> (Int, Float) -> FPoint -> SDL.Texture -> Attack
make numOfOutPoints initAngle' (every, change) speed txt =
  Attack.make
    (MV.make (Point 1 1) speed)
    (mkAngles initAngle' numOfOutPoints)
    every
    txt
    spiralUpdate

  where
    spiralUpdate :: IPoint -> IPoint -> Attack -> (DL.DList Bullet, Attack)
    spiralUpdate posi sz attack =
      Attack.update posi sz attack
        & over (_2 . outAngles) (map $ (+) change)

    mkAngles :: Int -> Int -> [Float]
    mkAngles initAngle ((`mod` 100) -> n) =
      let
        m = 360 / fromIntegral n
      in
        force $ map ((+) (fromIntegral initAngle) . (*) m . fromIntegral) [1..n]
