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
import qualified Play.Engine.LoadTextures as LT

run :: IO ()
run = do
  runGame (Stack (LT.mkState [("rin", "assets/rin.png")] $ MB.mkState) [])
