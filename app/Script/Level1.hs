{-# LANGUAGE OverloadedStrings #-}

module Script.Level1 where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.Static as St
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified Enemy.Fast as Fast
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified TextBox as TB
import qualified Data.Map as M


level1 :: State.State
level1 =
  GS.mkGameState $ Script
    wantedAssets
    lScript


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("test", MySDL.Texture "test.jpg")
     , ("bga", MySDL.Texture "bga.png")
     ]
  ++ [ ("chikua", MySDL.Texture "chikua.png")
     , ("saito",  MySDL.Texture "saito.png")
     , ("saito2", MySDL.Texture "saito2.png")
     , ("music", MySDL.Music "shushushu.ogg")
     , ("rin", MySDL.Texture "rin.png")
     ]


lScript :: MySDL.Resources -> Script
lScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = _ms } =
  -- [ PlayMusic ("music", M.lookup "music" _ms)
  
  -- [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  -- , WaitUntil noAction (const $ null)

  [ LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 6 "..." Nothing (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 5 "I sent assassins to your campsite." (M.lookup "saito" ts) (M.lookup "unispace" fs)

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Bottom 3 "!!!" (M.lookup "rin" ts) (M.lookup "unispace" fs)

  , Wait noAction 60
  ] ++
  -- First sequence
  concat
    ( replicate 3 $ concat
      [ spawnStaticAndWait 200 300 ts
      , spawnStaticAndWait 600 200 ts
      , spawnStaticAndWait 400 400 ts
      , spawnStaticAndWait 250 250 ts
      , spawnStaticAndWait 650 400 ts
      , spawnStaticAndWait 350 350 ts
      ]
    ) ++
  -- Mixing in up
  concat
    [ spawnStaticAndWait 200 300 ts
    , spawnStaticAndWait 600 200 ts
    , [Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]]
    , spawnStaticAndWait 400 400 ts
    , spawnStaticAndWait 250 250 ts
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    ] ++
  -- Mixing in up
  concat
    [ spawnStaticAndWait 200 300 ts
    , spawnStaticAndWait 600 200 ts
    , spawnStaticAndWait 400 400 ts
    , spawnStaticAndWait 250 250 ts
    , [Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]]
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    ] ++
  -- Mixing in up more
  concat
    [ spawnStaticAndWait 200 300 ts
    , [Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]]
    , spawnStaticAndWait 600 200 ts
    , spawnStaticAndWait 400 400 ts
    , [Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]]
    , spawnStaticAndWait 250 250 ts
    , spawnStaticAndWait 650 400 ts
    , spawnStaticAndWait 350 350 ts
    , [spawnTwoCDEs (Right ()) (Left ()) ts]
    ] ++

  -- First wave done
  [ WaitUntil noAction (const $ null)
  , Wait noAction 200
  , Wait act{ stopTheWorld = True } 30
  , goToLoc $ Point 380 800
  ] ++

  -- Second wave
  concat
    [ [ Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]
      , Wait noAction 100
      ]
    , spawnStaticAndWait 400 300 ts
    , [ Spawn $ sequence [CDE.make (Point 700 (-180)) (Left ()) ts]
      , Wait noAction 100
      ]
    , [ Spawn $ sequence [CDE.make (Point 650 (-150)) (Left ()) ts]
      , Wait noAction 100
      ]
    , [ Spawn $ sequence [CDE.make (Point 100 (-180)) (Right ()) ts]
      , Wait noAction 100
      ]
    , spawnStaticAndWait 600 300 ts
    , spawnStaticAndWait 250 250 ts
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 100
      , spawnTwoCDEs (Right ()) (Left ()) ts
      , Wait noAction 100
      ]
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 60
      , spawnTwoCDEs (Right ()) (Left ()) ts
      ]
    , spawnStaticAndWait 600 300 ts
    , spawnStaticAndWait 250 250 ts
    , [ spawnTwoCDEs (Left ()) (Right ()) ts
      , Wait noAction 60
      , spawnTwoCDEs (Right ()) (Left ()) ts
      ]
    ] ++

  -- Second wave done
  [ WaitUntil noAction (const $ null)
  , Wait noAction 200
  , Wait act{ stopTheWorld = True } 30
  , goToLoc $ Point 380 800
  ] ++

  -- Boss
  [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  , WaitUntil noAction (const $ null)

  , Wait noAction 100

  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 5 "orz" (M.lookup "saito" ts) (M.lookup "unispace" fs)
  ]

spawnTwoCDEs dir1 dir2 ts =
  Spawn $ sequence
    [ CDE.make (Point 300 (-180)) dir1 ts
    , CDE.make (Point 400 (-180)) dir2 ts
    ]

spawnStaticAndWait posx target ts =
  [ Spawn $ sequence
    [ St.make (Point posx (-100)) (Point 0 1) target ts
    ]
  , Wait noAction 120
  ]
