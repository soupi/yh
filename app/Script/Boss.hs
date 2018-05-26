{-# LANGUAGE OverloadedStrings #-}

module Script.Boss where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Enemy.CrossDown as CDE
import qualified Enemy.SideToSideSpiral as SSE
import qualified Enemy.Fast as Fast
import qualified Play.Engine.State as State
import qualified GameState as GS
import qualified TextBox as TB
import qualified Data.Map as M


boss :: State.State
boss =
  GS.mkGameState $ Script
    wantedAssets
    lScript


wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  CDE.wantedAssets
  ++ SSE.wantedAssets
  ++ TB.wantedAssets
  ++ [ ("bga", MySDL.Texture "bga.png")
     ]
  ++ [ ("chikua", MySDL.Texture "chikua.png")
     , ("saito",  MySDL.Texture "saito.png")
     , ("saito2", MySDL.Texture "saito2.png")
     , ("music", MySDL.Music "battle.ogg")
     , ("music-end", MySDL.Music "shushushu.ogg")
     , ("rin", MySDL.Texture "rin.png")
     ]


lScript :: MySDL.Resources -> Script
lScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = _ms } =
  
  [ goToLoc $ Point 380 800
  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 5 "It's not over yet!" (M.lookup "saito" ts) (M.lookup "unispace" fs)
  ] ++

  -- Boss
  [ Spawn $ sequence [Fast.make (Point 350 (-100)) ts]
  , WaitUntil noAction (const $ null)

  , Wait noAction 150

  -- End
  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 5 "orz" (M.lookup "saito" ts) (M.lookup "unispace" fs)
  , LoadTextBox act{ stopTheWorld = True } $
    TB.make TB.Top 15 "I'll be back!!!" (M.lookup "saito" ts) (M.lookup "unispace" fs)
  , Wait noAction 400
  , PlayMusic ("music-end", M.lookup "music-end" _ms)
  ] ++
  cycle
    (concat $ replicate 5
      [ LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Top 3 "Thanks for playing!" (M.lookup "saito" ts) (M.lookup "unispace" fs)
      , LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Bottom 3 "Thanks for playing!" (M.lookup "rin" ts) (M.lookup "unispace" fs)
      ] ++ pure
      [ LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Top 5 "Thanks for playing!" (M.lookup "chikua" ts) (M.lookup "unispace" fs)
      , LoadTextBox act{ stopTheWorld = True } $
        TB.make TB.Bottom 3 "Thanks for playing!" (M.lookup "rin" ts) (M.lookup "unispace" fs)
      ]
    )
