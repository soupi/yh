{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Play.Engine.Runner

import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified GameState as GS
import qualified Script.Level1 as L1

main :: IO ()
main = do
  runGame settings (GS.mkGameState L1.level1 `Stack` [])

settings :: Settings
settings = def
  { _windowSize = Point 800 1000
  }
