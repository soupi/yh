{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}

module Play.Types where

import Control.Lens (makeLenses)
import GHC.Generics
import Control.DeepSeq

-----------
-- Types --
-----------

data Point
  = Point
  { _pX :: {-# UNPACK #-} !Int
  , _pY :: {-# UNPACK #-} !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, NFData)

data Size
  = Size
  { _sW :: {-# UNPACK #-} !Int
  , _sH :: {-# UNPACK #-} !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, NFData)

makeLenses ''Point
makeLenses ''Size

pointToTuple :: Point -> (Int, Int)
pointToTuple (Point !x !y) = (x, y)

sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Size !w !h) = (w, h)
