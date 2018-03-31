{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.Load where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Image as SDL
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
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Async

data State
  = State
  { _timer :: Int
  , _filepaths :: [(String, MySDL.ResourceType FilePath)]
  , _nextState :: [(String, SDL.Texture)] -> [(String, SDLF.Font)] -> Result State.State
  }

makeLenses ''State

mkState
  :: [(String, MySDL.ResourceType FilePath)]
  -> ([(String, SDL.Texture)] -> [(String, SDLF.Font)] -> Result State.State)
  -> State.State
mkState files next =
  State.State $ State.StateF (initState files next) update render

initState
  :: [(String, MySDL.ResourceType FilePath)]
  -> ([(String, SDL.Texture)] -> [(String, SDLF.Font)] -> Result State.State)
  -> State
initState = State 90

update :: Input -> State -> Result ([MySDL.Request], (State.Command, State))
update _ s@(State t _ _)
  | t > 0 = pure ([], (State.None, s & over timer (\x -> x - 1)))
update input s@(State timer files next) =
  case (files, responses input) of
    ([], []) -> pure ([], (State.None, s))
    ([], [MySDL.Exception e]) -> throwError [e]
    ([], [MySDL.ResourcesLoaded textures fonts]) -> do
      next' <- next textures fonts
      pure ([], (State.Replace next', s))
    ([], rs) -> throwError ["Unexpected number of responses: " ++ show (length rs)]
    (files, _) -> pure ([MySDL.Load files], (State.None, set filepaths [] s))

render :: SDL.Renderer -> State -> IO ()
render renderer state =
  let c = fromIntegral $ state ^. timer
  in void $ MySDL.setBGColor (Linear.V4 (20 `mod` 255) ((10 + (c `div` 3)) `mod` 255) ((20 + (c `div` 2)) `mod` 255) 255) renderer
