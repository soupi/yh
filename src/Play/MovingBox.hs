{-# LANGUAGE TemplateHaskell #-}

module Play.MovingBox where

import qualified SDL
import qualified Play.MySDL.MySDL as MySDL

import Data.Word
import Play.Types
import Play.Input
import Play.Settings
import qualified Play.State as State
import qualified Control.Monad.State as SM
import Control.Monad.Except
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear

data State
  = State
  { _pos :: !Point
  , _size :: !Size
  , _speed :: !Int
  , _color :: !Word8
  }
  deriving Show

makeLenses ''State

initStateState :: Word8 -> State.State
initStateState c = State.mkState (initState c) update render

initState :: Word8 -> State
initState c = State
  { _pos = Point 350 260
  , _size = Size 100 80
  , _speed = 5
  , _color = c
  }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  let
    move = keysToMovement (_speed state) input
    newState =
      state
        & over (pos . pX) ((+) (move ^. pX))
        & over (pos . pY) ((+) (move ^. pY))
  if
    | keyReleased KeyQuit input ->
      throwError []
    | keyReleased KeyA input ->
      pure (State.Push $ initStateState (state ^. color + 1), state)
    | keyReleased KeyB input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  let rects = VS.fromList [toRect state]
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  let
    c = state ^. color * 50 `mod` 255
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 (c - 50 `mod` 255) (c `mod` 255) (c + 50 `mod` 255) 255
  SDL.drawRects renderer rects
  SDL.fillRects renderer rects

toRect :: State -> SDL.Rectangle C.CInt
toRect state =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . over both fromIntegral . pointToTuple $ state ^. pos)
    (uncurry Linear.V2 . over both fromIntegral . sizeToTuple $ state ^. size)
