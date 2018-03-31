module Script.Level1 where

import qualified SDL

import Script
import Play.Engine.Types
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import Debug.Trace

wantedAssets :: [(String, FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets


level1 :: [(String, SDL.Texture)] -> Script
level1 ts =
  [ spawnTwoCDEs (Left ()) (Right ()) ts
  , Wait noAction 300
  , spawnTwoCDEs (Right ()) (Left ()) ts
  , Wait noAction 300
  , spawnTwoCDEs (Left ()) (Right ()) ts
  , Wait noAction 100
  , spawnTwoCDEs (Right ()) (Left ()) ts
  , WaitUntil noAction (const $ null)
  , Wait noAction 200
  , goToLoc $ Point 380 800
  , Spawn $ sequence [SSE.make (Point 350 (-100)) ts]
  ]

spawnTwoCDEs dir1 dir2 ts =
  Spawn $ sequence
    [ CDE.make (Point 300 (-180)) dir1 ts
    , CDE.make (Point 400 (-180)) dir2 ts
    ]

