{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module DecorationObject where

import qualified SDL

import Play.Engine.Utils
import Play.Engine.Input
import Play.Engine.Settings
import Control.Lens
import Control.DeepSeq

data DecorationObject
  = forall s. DecObj (DecorationObject' s)

make s u r = DecObj $ DecObj' s u r

data DecorationObject' a
  = DecObj'
  { _state :: !a
  , _update :: Input -> a -> Result (Maybe a, [DecorationObject])
  , _render :: SDL.Renderer -> Camera -> a -> IO ()
  }

instance NFData a => NFData (DecorationObject' a) where
  rnf dec =
    rnf (_state dec)

instance Eq a => Eq (DecorationObject' a) where
  d1 == d2 =
    _state d1 == _state d2

instance Ord a => Ord (DecorationObject' a) where
  d1 <= d2 =
    _state d1 <= _state d2

makeFieldsNoPrefix ''DecorationObject'

