{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Run where

import Play.Runner

import qualified Play.MySDL.MySDL as MySDL
import Play.Input
import Play.Types
import Play.Utils
import Play.Settings
import qualified Play.State as State
import qualified Play.MovingBox as MB

run :: IO ()
run = do
  runGame (Stack (MB.initStateState 1) [])
