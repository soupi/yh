{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Play.Engine.State where

import Prelude hiding (head)
import qualified Play.Engine.MySDL.MySDL as MySDL
import SDL
import Play.Engine.Utils
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except


-----------
-- State --
-----------

-- | StateF describes a generic interface for a game State
--   * state: for the state data
--   * update: how to update the state
--   * render: how to render the state
data StateF a
  = StateF
  { state :: a
  , update
      :: Input
      -> a
      -- it might fail with a reason
      -> Result ([MySDL.Request], (Command, a))
  , render :: SDL.Renderer -> a -> IO ()
  }

-- | Existentially Quantified State
data State
  = forall s. State (StateF s)

-- | Create a State from the interface
mkState
  :: a
  -> (Input -> a -> Result (Command, a))
  -> (SDL.Renderer -> a -> IO ())
  -> State
mkState s u r = State (StateF s (\i a -> fmap pure $ u i a) r)

-- a sample definition of a state. Does nothing.
sample :: State
sample = mkState
  ()
  (\_ () -> pure (None, ()))
  (const pure)
  


-----------------
-- State stack --
-----------------

-- | A command for the state stack, to be returned by `update`
data Command
  = Done -- ^ You can remove me from the stack
  | None -- ^ Keep me at the top of the stack
  | Push State -- ^ Push a new state
  | Replace State -- ^ Replace me with a new state

-- | Update the top state on the stack
updater :: Input -> Stack State -> Result ([MySDL.Request], Stack State)
updater input states = do
  (reqs, (cmd, newState)) <- updateState input (head states)
  (reqs,) <$> case cmd of
    Done -> case pop states of
      (_, Nothing) -> throwError ["Unexpected empty stack of states"]
      (_, Just rest) -> pure rest
    None -> pure $ replace newState states
    Replace otherState -> pure $ replace otherState states
    Push otherState -> pure $ push otherState (replace newState states)

-- | Update an existentially quantified State
updateState :: Input -> State -> Result ([MySDL.Request], (Command, State))
updateState input = \case
  State s ->
    flip fmap ((update s) input (state s)) $ \case
      (reqs, (cmd, newState)) -> (reqs, (cmd, State $ s { state = newState }))

-- | Render the top state on the stack
renderer :: SDL.Renderer -> Stack State -> IO ()
renderer sdlRenderer states = case head states of
  State s ->
    (render s) sdlRenderer (state s)
