{-# LANGUAGE OverloadedStrings #-}

module Script.Level1 where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified TextBox as TB
import qualified Data.Map as M


level1 :: ScriptData
level1 = Script
  wantedAssets
  lScript


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("chikua", MySDL.Texture "chikua.png")
     , ("saito",  MySDL.Texture "saito.png")
     , ("saito2", MySDL.Texture "saito2.png")
     , ("music", MySDL.Music "shushushu.ogg")
     , ("rin", MySDL.Texture "rin.png")
     ]


lScript :: MySDL.Resources -> Script
lScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = _ms } =
  -- [ PlayMusic ("music", M.lookup "music" _ms)
  
  [ LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Up 6 "..." Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Up 5 "I sent assassins to your campsite." (M.lookup "saito" ts) (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Down 3 "!!!" (M.lookup "rin" ts) (M.lookup "unispace" fs)

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
    TB.make TB.Up 5 "orz" (M.lookup "saito" ts) (M.lookup "unispace" fs)
  ]

spawnTwoCDEs dir1 dir2 ts =
  Spawn $ sequence
    [ CDE.make (Point 300 (-180)) dir1 ts
    , CDE.make (Point 400 (-180)) dir2 ts
    ]

