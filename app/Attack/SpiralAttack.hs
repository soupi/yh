{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Attack.SpiralAttack where

import qualified SDL

import Play.Engine.Types
import Control.Lens
import qualified Data.DList as DL

import Bullet
import Play.Engine.Utils
import qualified Attack as A
import qualified Play.Engine.Movement as MV


make :: Int -> Int -> (Int, Float) -> MV.Movement -> SDL.Texture -> A.Attack
make numOfOutPoints initAngle' (every, change) mv txt =
  A.make
    mv
    (mkAngles initAngle' numOfOutPoints)
    every
    txt
    spiralUpdate

  where
    spiralUpdate :: IPoint -> IPoint -> A.Attack -> (DL.DList Bullet, A.Attack)
    spiralUpdate posi sz attack =
      A.update posi sz attack
        & over (_2 . A.outAngles) (map $ (+) change)

