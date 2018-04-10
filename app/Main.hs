{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Play.Engine.Runner

import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified StartScreen as S

main :: IO ()
main = do
  runGame settings (S.make `Stack` [])

settings :: Settings
settings = def
  { _windowSize = Point 800 1000
  }
