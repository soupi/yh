{-# LANGUAGE OverloadedStrings #-}

module Script.Level1 where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified TextBox as TB
import Debug.Trace


level1 :: ScriptData
level1 = Script
  wantedAssets
  lScript


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("chikua", MySDL.Texture "assets/imgs/chikua.png")
     , ("saito",  MySDL.Texture "assets/imgs/saito.png")
     , ("saito2", MySDL.Texture "assets/imgs/saito2.png")
     ]


lScript :: [(String, SDL.Texture)] -> [(String, SDLF.Font)] -> Script
lScript ts fs =
  [ LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Up 6 "..." Nothing (lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Up 5 "I sent assassins to your campsite." (lookup "saito" ts) (lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Down 3 "!!!" (lookup "rin" ts) (lookup "unispace" fs)

  , Wait noAction 60

  , spawnTwoCDEs (Left ()) (Right ()) ts
  , Wait noAction 300
  , spawnTwoCDEs (Right ()) (Left ()) ts
  , Wait noAction 300
  , spawnTwoCDEs (Left ()) (Right ()) ts
  , Wait noAction 100
  , spawnTwoCDEs (Right ()) (Left ()) ts
  , WaitUntil noAction (const $ null)
  , Wait noAction 200
  , Wait act{ stopTheWorld = True } 30
  , goToLoc $ Point 380 800
  , Spawn $ sequence [SSE.make (Point 350 (-100)) ts]
  
  , WaitUntil noAction (const $ null)

  , Wait noAction 100

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Up 5 "orz" (lookup "saito" ts) (lookup "unispace" fs)
  ]

spawnTwoCDEs dir1 dir2 ts =
  Spawn $ sequence
    [ CDE.make (Point 300 (-180)) dir1 ts
    , CDE.make (Point 400 (-180)) dir2 ts
    ]

