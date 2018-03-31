module Script where

import Data.Maybe
import qualified SDL
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
  | LoadTextBox TB.Loc T.Text
  | WaitTextBoxTexture TB.Loc
  | WaitTextBox TB.TextBox

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

update :: I.Input -> IPoint -> [Enemy] -> Script -> Result ([MySDL.Request], (Actions, Script))
update input mcPos enemies = \case
  [] -> pure ([], (noAction, []))
  Wait acts i : rest
    | i <= 0 -> pure ([], (acts, rest))
    | otherwise -> pure ([], (acts, Wait acts (i-1) : rest))

  WaitUntil acts test : rest
    | test mcPos enemies -> pure ([], (acts, rest))
    | otherwise -> pure ([], (acts, WaitUntil acts test : rest))

  Spawn spawned : rest ->
    ([],) . (, rest) . (\s -> noAction { spawn = s }) <$> spawned

  LoadTextBox loc text : rest -> do
    pure
      ( [MySDL.MakeText ("unispace", "assets/fonts/unispace/unispace.ttf") text]
      , (noAction, WaitTextBoxTexture loc : rest)
      )

  WaitTextBoxTexture loc : rest ->
    case getNewText $ I.responses input of
      Nothing ->
        pure ([], (noAction, WaitTextBoxTexture loc : rest))
      Just (MySDL.Exception e) ->
        throwError [e]
      Just (MySDL.NewText txt) -> do
        let tb = TB.make loc Nothing txt
        pure ([], (noAction, WaitTextBox tb : rest))

  WaitTextBox tb : rest -> do
    TB.update input tb >>= \case
      Nothing -> pure ([], (noAction, rest))
      Just tb' -> pure ([], (noAction, WaitTextBox tb' : rest))

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
      WaitTextBox tb ->
        TB.render renderer tb
      _ -> pure ()

getNewText :: [MySDL.Response] -> Maybe MySDL.Response
getNewText = \case
  [] -> Nothing
  MySDL.NewText txt : _ ->
    pure $ MySDL.NewText txt
  MySDL.Exception e : _ -> pure $ MySDL.Exception e
  _ : r -> getNewText r
