{-# LANGUAGE TemplateHaskell #-}

module Play.MovingBox where

import qualified SDL
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Word
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import Control.Monad.Except
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear


data MainChar
  = MainChar
  { _pos :: !Point
  , _size :: !Size
  , _speed :: !Int
  , _texture :: SDL.Texture
  }

data State
  = State
  { _mc :: MainChar
  }

makeLenses ''MainChar
makeLenses ''State

mkState :: [(String, SDL.Texture)] -> Result State.State
mkState texts = do
  state <- initState texts
  pure $ State.mkState
    state
    update
    render

initState :: [(String, SDL.Texture)] -> Result State
initState ts = do
  case lookup "rin" ts of
    Nothing ->
      throwError ["Texture not found: rin"]
    Just t -> pure $ State $ MainChar
      { _pos = Point 350 260
      , _size = Size 100 80
      , _speed = 5
      , _texture = t
      }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  let
    move = keysToMovement (state ^. mc . speed) input
    newState =
      state
        & over (mc . pos . pX) ((+) (move ^. pX))
        & over (mc . pos . pY) ((+) (move ^. pY))
  -- state stack manipulation
  if
    | keyReleased KeyQuit input ->
      throwError []
    | keyReleased KeyA input -> do
      next <- mkState [("rin", state ^. mc . texture)]
      pure (State.Push next, state)
    | keyReleased KeyB input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SDL.copy renderer (state ^. mc . texture) Nothing (Just $ toRect $ state ^. mc)

toRect :: MainChar -> SDL.Rectangle C.CInt
toRect mc =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . over both fromIntegral . pointToTuple $ mc ^. pos)
    (uncurry Linear.V2 . over both fromIntegral . sizeToTuple $ mc ^. size)
