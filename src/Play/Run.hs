{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Play.Run where

import Play.Engine.Runner

import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Input
import Play.Engine.Types
import Play.Engine.Utils
import Play.Engine.Settings
import qualified Play.Engine.State as State
import qualified Play.MovingBox as MB

run :: IO ()
run = do
  runGame (Stack (MB.initStateState 1) [])
