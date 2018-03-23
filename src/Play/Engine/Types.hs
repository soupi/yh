{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}

module Play.Engine.Types where

import Control.Lens (makeLenses)
import GHC.Generics
import Control.DeepSeq

-----------
-- Types --
-----------

data Point a
  = Point
  { _x :: !a
  , _y :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Generic, NFData)

type IPoint = Point Int
type FPoint = Point Float
type Size = Point Int

makeLenses ''Point

pointToTuple :: Point a -> (a, a)
pointToTuple (Point !x !y) = (x, y)

sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Point !w !h) = (w, h)
