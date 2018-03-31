module Script where

import Data.Maybe
import qualified SDL
import qualified SDL.Font as SDLF
import qualified Play.Engine.MySDL.MySDL as MySDL

import Enemy
import qualified TextBox as TB
import Play.Engine.Settings
import Play.Engine.Utils
import Play.Engine.Types
import Control.Monad.Except
import qualified Play.Engine.Input as I
import qualified Data.Text as T

data Command
  = Wait !Actions Int
  | WaitUntil !Actions (IPoint -> [Enemy] -> Bool)
  | Spawn (Result [Enemy])
  | LoadTextBox !Actions (Result TB.TextBox)
  | WaitTextBox !Actions TB.TextBox

data ScriptData
  = Script
  { assets :: [(String, MySDL.ResourceType FilePath)]
  , script :: [(String, SDL.Texture)] -> [(String, SDLF.Font)] -> Script
  }

type Script = [Command]

data Actions
  = Actions
  { moveMC :: Maybe IPoint
  , spawn :: [Enemy]
  , stopTheWorld :: Bool
  }

noAction :: Actions
noAction = Actions
  { moveMC = Nothing
  , spawn = []
  , stopTheWorld = False
  }

act :: Actions
act = noAction

update :: I.Input -> IPoint -> [Enemy] -> Script -> Result (Actions, Script)
update input mcPos enemies = \case
  [] -> pure (noAction, [])
  Wait acts i : rest
    | i <= 0 -> pure (acts, rest)
    | otherwise -> pure (acts, Wait acts (i-1) : rest)

  WaitUntil acts test : rest
    | test mcPos enemies -> pure (acts, rest)
    | otherwise -> pure (acts, WaitUntil acts test : rest)

  Spawn spawned : rest ->
    (, rest) . (\s -> noAction { spawn = s }) <$> spawned

  LoadTextBox acts rtb : rest -> do
    tb <- rtb
    pure (acts, WaitTextBox acts tb : rest)

  WaitTextBox acts tb : rest -> do
    TB.update input tb >>= \case
      Nothing -> pure  (acts, rest)
      Just tb' -> pure (acts, WaitTextBox acts tb' : rest)

goToLoc :: IPoint -> Command
goToLoc p =
  WaitUntil
    (act { moveMC = Just p })
    (\mcPos _ -> isAround p mcPos (Point 20 20))

render :: SDL.Renderer -> Script -> IO ()
render renderer =
  maybe (pure ()) f . listToMaybe
  where
    f = \case
      WaitTextBox _ tb ->
        TB.render renderer tb
      _ -> pure ()

getNewText :: [MySDL.Response] -> Maybe MySDL.Response
getNewText = \case
  [] -> Nothing
  MySDL.NewText txt : _ ->
    pure $ MySDL.NewText txt
  MySDL.Exception e : _ -> pure $ MySDL.Exception e
  _ : r -> getNewText r
