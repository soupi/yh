{-# LANGUAGE OverloadedStrings #-}

module Script.Introduction where

import qualified Play.Engine.MySDL.MySDL as MySDL

import Script
import Play.Engine.Types
import qualified Data.Text as T
import qualified Data.Map as M
import qualified TextBox as TB
import qualified Play.Engine.Sprite as Spr
import qualified Play.Engine.State as State
import qualified Script.Level1 as L1
import qualified GameState as GS


intro :: ScriptData
intro = Script
  wantedAssets
  introScript

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  TB.wantedAssets
  ++ [ ("test", MySDL.Texture "test.jpg")
     , ("bga", MySDL.Texture "bga.png")
     , ("music", MySDL.Music "shushushu.ogg")
     ]


introScript :: MySDL.Resources -> Script
introScript MySDL.Resources{ MySDL.textures = ts, MySDL.fonts = fs, MySDL.music = ms } =

  [ PlayMusic ("music", M.lookup "music" ms)

  , let
      sprargs rint =
        Spr.make $ Spr.MakeArgs
          { mkActionmap = M.fromList [("normal", 0)]
          , mkAction = "normal"
          , mkTexture = rint
          , mkSize = Point 800 1000
          , mkMaxPos = 4
          , mkSpeed = 30
          }
      spr = sprargs =<< M.lookup "bga" ts
    in
      Wait act{ changeSprite = spr } 0

  , LoadTextBox noAction $
    TB.make TB.Bottom 15 ". . ." Nothing (M.lookup "unispace" fs)


  , nixVoice ts fs "Hi."
  , nixVoice ts fs "I'm Nix."

  , nixVoice ts fs "30, self employed. A hacker."
  , nixVoice ts fs $ T.unlines
    [ "I offer my services to companies by exposing their"
    , "security holes and help fixing them."
    ]
  , nixVoice ts fs "I've been doing it for a few years now."
  , nixVoice ts fs "It's honest work, I promise."
  , nixVoice ts fs "It's just..."
  , nixVoice ts fs "A bit sparse sometimes."
  , nixVoice ts fs $ T.unlines
    [ "So in days like today, I scrap the net looking for"
    , "work."
    ]
  , nixVoice ts fs "And I think that today, I hit the jackpot."

  , StopMusic
  , Wait noAction 60

  , Wait act{ command = State.Replace $ GS.mkGameState L1.level1 } 60
  ]


nixVoice ts fs txt =
  LoadTextBox noAction $
    TB.make TB.Bottom 3 txt
    (M.lookup "nix" ts)
    (M.lookup "unispace" fs)
