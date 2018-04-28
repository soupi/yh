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
import qualified VN


intro :: State.State
intro =
  VN.make 1 $ Script
    wantedAssets
    introScript

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  TB.wantedAssets
  ++ [ ("test", MySDL.Texture "test.jpg")
     , ("bga", MySDL.Texture "bga.png")
     , ("music", MySDL.Music "shushushu.ogg")
     , ("nix", MySDL.Texture "rin.png")
     , ("jas", MySDL.Texture "saito.png")
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
  , nixVoice ts fs $ T.unwords
    [ "Not the criminal kind, mind you."
    ]
  , nixVoice ts fs $ T.unwords
    [ "I offer my services and expertise to"
    , "companies. I expose and help fix security"
    , "holes in their systems."
    ]
  , nixVoice ts fs "I've been doing it for a few years now."
  , nixVoice ts fs "It's honest work, I promise."
  , nixVoice ts fs "It's just... a bit sparse sometimes."
  , nixVoice ts fs $ T.unwords
    [ "So in days like today, I scrap some mailing"
    , "lists, looking for work."
    ]
  , nixVoice ts fs "And I think that today, I hit the jackpot."

  , StopMusic
  , Wait noAction 60

  , nixVoice ts fs $ T.unwords
    [ "JAS, an up-and-coming security start-up has"
    , "been making their name known. I've heard"
    , "their new security product is top notch."
    ]

  , nixVoice ts fs $ T.unwords
    [ "And they've just announced a challenge for"
    , "hackers such as myself."
    ]

  , LoadTextBox noAction $
    TB.make TB.Bottom 3
    (T.unwords
      [ "\"Find security holes in our system, and we'll"
      , "award you with 20,000$!\""
      ]
    )
    Nothing
    (M.lookup "unispace" fs)

  , nixVoice ts fs $ T.unwords
    [ "With this kind of money, I could live"
    , "comfortably for a few months without"
    , "worring about anything!"
    ]

  , nixVoice ts fs $ T.unwords
    [ "And it's always a good idea to get to know"
    , "the latest technology before everyone else."
    ]

  , nixVoice ts fs "So with that in mind..."

  , nixVoice ts fs "Challenge accepted!"

  , Wait noAction 120

  , jasVoice ts fs "..."
  , jasVoice ts fs "Okay. We've confirmed your participation in our challenge."
  , jasVoice ts fs $ T.unwords $
    [ "To verify that you've exposed a weakness in our software,"
    , "you'll have to provide us with some proof."
    ]
  , jasVoice ts fs "For example, the password for the system's admin account will be sufficient."
  , jasVoice ts fs "Notice that this challenge has a time limit of 72 hours."
  , jasVoice ts fs "Good luck."
  , Wait noAction 60
  , nixVoice ts fs "Let the hacking begin!"

  , Wait act{ command = State.Replace L1.level1 } 60
  ]


nixVoice ts fs txt =
  LoadTextBox noAction $
    TB.make TB.Bottom 3 txt
    (M.lookup "nix" ts)
    (M.lookup "unispace" fs)

jasVoice ts fs txt =
  LoadTextBox noAction $
    TB.make TB.Bottom 3 txt
    (M.lookup "jas" ts)
    (M.lookup "unispace" fs)
