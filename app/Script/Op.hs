{-# LANGUAGE OverloadedStrings #-}

module Script.Op where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified TextBox as TB
import qualified Data.Map as M
import qualified Play.Engine.Sprite as Spr
import qualified Play.Engine.State as State
import qualified Script.Level1 as L1
import qualified GameState as GS


op :: ScriptData
op = Script
  wantedAssets
  opScript


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  TB.wantedAssets
  ++ [ ("test", MySDL.Texture "test.jpg")
     , ("music", MySDL.Music "shushushu.ogg")
     ]


opScript :: MySDL.Resources -> Script
opScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =
  [ PlayMusic ("music", M.lookup "music" ms)

  , let
      spr = Spr.make . Spr.simpleArgs (Point 800 1000) =<< M.lookup "test" ts
    in
      LoadTextBox act{ changeSprite = spr } $
        TB.make TB.Up 6 "..." Nothing (M.lookup "unispace" fs)

  , LoadTextBox noAction $
    TB.make TB.Up 5 "I sent assassins to your campsite." (M.lookup "saito" ts) (M.lookup "unispace" fs)


  , LoadTextBox noAction $
    TB.make TB.Down 3 "!!!" (M.lookup "rin" ts) (M.lookup "unispace" fs)

  , StopMusic
  , Wait noAction 60

  , Wait act{ command = State.Replace $ GS.mkGameState L1.level1 } 60
  ]

