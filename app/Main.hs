{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Play.Engine.Runner

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified Play.Engine.State as State
import qualified Play.Engine.LoadTextures as LT
import qualified GameState as SB

main :: IO ()
main = do
  runGame settings (Stack SB.mkGameState [])

settings :: Settings
settings = def
  { _windowSize = Size 800 1000
  }
