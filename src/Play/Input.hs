module Play.Input where

import Data.Tuple
import qualified SDL
import Play.Types
import qualified Data.Map as M


data Input
  = Input
  { inputKeys :: !Keys
  }
  deriving Show

type Keys = M.Map Key Bool

data Key
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  deriving (Show, Read, Eq, Ord)

defKeyMap :: [(Key, SDL.Scancode)]
defKeyMap = map swap
  [ (SDL.ScancodeW, KeyUp)
  , (SDL.ScancodeS, KeyDown)
  , (SDL.ScancodeA, KeyLeft)
  , (SDL.ScancodeD, KeyRight)
  ]

makeEvents :: [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> [(Key, SDL.Scancode)] -> Keys
makeEvents _ keyPressed =
  M.fromListWith (||)
  . fmap (fmap keyPressed)

testKey :: Key -> Keys -> Bool
testKey key = maybe False id . M.lookup key

keysToMovement :: Keys -> Point
keysToMovement keys =
  let
      singleMove k1 k2
        | testKey k1 keys && not (testKey k2 keys) = -1
        | testKey k2 keys && not (testKey k1 keys) =  1
        | otherwise = 0
      hori = singleMove KeyUp KeyDown
      vert = singleMove KeyLeft KeyRight
  in Point vert hori
