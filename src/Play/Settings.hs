{-# LANGUAGE TemplateHaskell #-}

module Play.Settings where

import qualified SDL
import qualified Control.Monad.State as SM
import Control.Monad.Except

import Data.Tuple
import Play.Types
import Play.Input
import Control.Lens
import GHC.Generics
import Control.DeepSeq

data Settings
  = Settings
  { _windowSize :: !Size
  , _keyMap :: ![(Key, SDL.Scancode)]
  , _keyStats :: !Keys
  }
  deriving (Show)

makeLenses ''Settings

type Result a = SM.StateT Settings (Except [String]) a

runResult settings m = fmap swap $ runExcept $ SM.runStateT m settings

def :: Settings
def = Settings
  { _windowSize = Size 800 600
  , _keyMap = defKeyMap
  , _keyStats = initKeyStats
  }
