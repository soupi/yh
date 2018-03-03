{-# LANGUAGE TemplateHaskell #-}

module Play.Types where

import Control.Lens (makeLenses)

-----------
-- Types --
-----------

data Point
  = Point
  { _pX :: !Int
  , _pY :: !Int
  }
  deriving (Show, Read, Eq, Ord)

data Size
  = Size
  { _sW :: !Int
  , _sH :: !Int
  }
  deriving (Show, Read, Eq, Ord)

makeLenses ''Point
makeLenses ''Size

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point !x !y) = (x, y)

sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size !w !h) = (w, h)
