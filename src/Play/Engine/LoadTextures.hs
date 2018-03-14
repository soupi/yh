{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.LoadTextures where

import qualified SDL
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
  { _color :: Word8
  , _timer :: Int
  , _filepaths :: [(String, FilePath)]
  , _nextState :: [(String, SDL.Texture)] -> Result State.State
  }

makeLenses ''State

mkState :: [(String, FilePath)] -> ([(String, SDL.Texture)] -> Result State.State) -> State.State
mkState files next = State.State $ State.StateF (initState files next) update render

initState :: [(String, FilePath)] -> ([(String, SDL.Texture)] -> Result State.State) -> State
initState = State 0 120

update :: Input -> State -> Result ([MySDL.Request], (State.Command, State))
update _ s@(State c t _ _)
  | t > 0 = pure ([], (State.None, s & over timer (\x -> x - 1) & set color (c + 1)))
update input s@(State c timer files next) =
  case (files, responses input) of
    ([], []) -> pure ([], (State.None, set color 100 s))
    ([], [MySDL.Exception e]) -> throwError [e]
    ([], [MySDL.TexturesLoaded textures]) -> do
      next' <- next textures
      pure ([], (State.Replace next', s))
    ([], rs) -> throwError ["Unexpected number of responses: " ++ show (length rs)]
    (files, _) -> pure ([MySDL.LoadTextures files], (State.None, set filepaths [] s))

render :: SDL.Renderer -> State -> IO ()
render renderer state =
  let c = state ^. color `mod` 255
  in void $ MySDL.setBGColor (Linear.V4 (50 `mod` 255) (c `mod` 255) (c + 50 `mod` 255) 255) renderer
