{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.Settings where

import qualified SDL
import qualified Control.Monad.State as SM
import Control.Monad.Except

import Data.Tuple
import Play.Engine.Types
import Play.Engine.Input
import Control.Lens

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
  { _windowSize = Point 800 600
  , _keyMap = defKeyMap
  , _keyStats = initKeyStats
  }
