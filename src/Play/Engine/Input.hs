{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Play.Engine.Input where

import Data.Tuple
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL
import Play.Engine.Types
import qualified Data.Map as M
import GHC.Generics
import Control.DeepSeq


data Input
  = Input
  { inputKeys :: !Keys
  , responses :: ![MySDL.Response]
  }

type Keys = M.Map Key Action

data Action
  = Click
  | Hold
  | Release
  | Idle
  deriving (Show, Eq, Ord, Generic, NFData)

data Key
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyQuit
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic, NFData)

empty :: Input
empty = Input mempty mempty

initKeyStats :: Keys
initKeyStats = M.fromList $ zip [minBound..maxBound] (cycle [Idle])

defKeyMap :: [(Key, SDL.Scancode)]
defKeyMap = map swap
  [ (SDL.ScancodeW, KeyUp)
  , (SDL.ScancodeS, KeyDown)
  , (SDL.ScancodeA, KeyLeft)
  , (SDL.ScancodeD, KeyRight)
  , (SDL.ScancodeUp, KeyUp)
  , (SDL.ScancodeDown, KeyDown)
  , (SDL.ScancodeLeft, KeyLeft)
  , (SDL.ScancodeRight, KeyRight)
  , (SDL.ScancodeEscape, KeyQuit)
  , (SDL.ScancodeQ, KeyQuit)
  , (SDL.ScancodeZ, KeyA)
  , (SDL.ScancodeX, KeyB)
  , (SDL.ScancodeC, KeyC)
  , (SDL.ScancodeV, KeyD)
  ]

makeEvents :: Keys -> [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> [(Key, SDL.Scancode)] -> Keys
makeEvents !current _ !isKeyPressed =
  updateKeys current
  . M.fromListWith max
  . fmap (fmap isKeyPressed)

updateKeys :: Keys -> M.Map Key Bool -> Keys
updateKeys !keys !newStates =
  flip M.mapWithKey keys $ \k s ->
    case (s, testKey k newStates) of
      (Idle, True) -> Click
      (Click, True) -> Hold
      (Hold, True) -> Hold
      (Release, True) -> Click
      (Idle, False) -> Idle
      (Click, False) -> Release
      (Hold, False) -> Release
      (Release, False) -> Idle

testKey :: Key -> M.Map Key Bool -> Bool
testKey key = maybe False id . M.lookup key

keyReleased :: Key -> Input -> Bool
keyReleased key = maybe False (== Release) . M.lookup key . inputKeys

keyClicked :: Key -> Input -> Bool
keyClicked key = maybe False (== Click) . M.lookup key . inputKeys

keyPressed :: Key -> Input -> Bool
keyPressed key = maybe False (/= Idle) . M.lookup key . inputKeys

keyIdle :: Key -> Input -> Bool
keyIdle key = maybe False (== Idle) . M.lookup key . inputKeys

keysToMovement :: Float -> Input -> FPoint
keysToMovement speed keys =
  let
      singleMove k1 k2
        | keyPressed k1 keys && not (keyPressed k2 keys) = -speed
        | keyPressed k2 keys && not (keyPressed k1 keys) =  speed
        | otherwise = 0
      hori = singleMove KeyUp KeyDown
      vert = singleMove KeyLeft KeyRight
  in Point vert hori
