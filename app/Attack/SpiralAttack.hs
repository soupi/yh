{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Attack.SpiralAttack where

import qualified SDL

import Play.Engine.Types
import Control.Lens
import Control.DeepSeq
import qualified Data.DList as DL

import Bullet
import qualified Attack as A
import qualified Play.Engine.Movement as MV


make :: Int -> Int -> (Int, Float) -> FPoint -> SDL.Texture -> A.Attack
make numOfOutPoints initAngle' (every, change) speed txt =
  A.make
    (MV.make (Point 1 1) speed)
    (mkAngles initAngle' numOfOutPoints)
    every
    txt
    spiralUpdate

  where
    spiralUpdate :: IPoint -> IPoint -> A.Attack -> (DL.DList Bullet, A.Attack)
    spiralUpdate posi sz attack =
      A.update posi sz attack
        & over (_2 . A.outAngles) (map $ (+) change)

    mkAngles :: Int -> Int -> [Float]
    mkAngles initAngle ((`mod` 100) -> n) =
      let
        m = 360 / fromIntegral n
      in
        force $ map ((+) (fromIntegral initAngle) . (*) m . fromIntegral) [1..n]
