module Script where

import Enemy
import Play.Engine.Settings

data Command
  = Wait Int
  | WaitUntil ([Enemy] -> Bool)
  | Spawn (Result [Enemy])

type Script = [Command]


update :: [Enemy] -> Script -> Result ([Enemy], Script)
update enemies = \case
  [] -> pure ([], [])
  Wait i : rest
    | i <= 0 -> pure ([], rest)
    | otherwise -> pure ([], Wait (i-1) : rest)

  WaitUntil test : rest
    | test enemies -> pure ([], rest)
    | otherwise -> pure ([], WaitUntil test : rest)

  Spawn spawned : rest ->
    (, rest) <$> spawned
