{-# LANGUAGE TemplateHaskell #-}

module Play.MovingBox where

import qualified SDL
import qualified Play.MySDL.MySDL as MySDL

import Play.Types
import Play.Input
import Play.Settings
import qualified Play.State as State
import qualified Control.Monad.State as SM
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear

data State
  = State
  { _pos :: !Point
  , _size :: !Size
  , _dir :: !Point
  }
  deriving Show

makeLenses ''State

initStateState :: State.State
initStateState = State.mkState initState update render

initState :: State
initState = State
  { _pos = Point 0 0
  , _size = Size 100 80
  , _dir = Point 5 5
  }

update :: Input -> State -> Result (State.Command, State)
update _ state = do
  wSize <- _windowSize <$> SM.get
  pure . (State.None,) $
    state
      & over (pos . pX) ((+) (state ^. dir . pX))
      & over (pos . pY) ((+) (state ^. dir . pY))
      & over (dir . pX)
        (if
           | (state ^. pos . pX) + (state ^. size . sW) >= (wSize ^. sW) -> negate . abs
           | (state ^. pos . pX) <= 0 -> abs
           | otherwise -> id
        )
      & over (dir . pY)
        (if
           | (state ^. pos . pY) + (state ^. size . sH) >= (wSize ^. sH) -> negate . abs
           | (state ^. pos . pY) <= 0 -> abs
           | otherwise -> id
        )

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  let rects = VS.fromList [toRect state]
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  SDL.drawRects renderer rects
  --SDL.fillRects renderer rects

toRect :: State -> SDL.Rectangle C.CInt
toRect state =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . over both fromIntegral . pointToTuple $ state ^. pos)
    (uncurry Linear.V2 . over both fromIntegral . sizeToTuple $ state ^. size)
