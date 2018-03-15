{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module ShootingBox where

import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Input
import Play.Engine.Settings
import Control.Monad.Except
import Control.Lens
import qualified Play.Engine.State as State
import qualified Control.Monad.State as SM
import qualified Linear
import qualified Data.DList as DL


import Bullet
import qualified Play.Engine.ScrollingBackground as SBG


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
  , _bullets :: DL.DList Bullet
  , _newBullet :: Point -> Bullet
  , _bg :: SBG.SBG
  }

makeFieldsNoPrefix ''MainChar
makeFieldsNoPrefix ''State

mkState :: [(String, SDL.Texture)] -> Result State.State
mkState texts = do
  state <- initState texts
  pure $ State.mkState
    state
    update
    render

wantedAssets :: [(String, FilePath)]
wantedAssets =
  [ ("bg", "assets/bg.png")
  , ("rin", "assets/rin.png")
  ]

initState :: [(String, SDL.Texture)] -> Result State
initState ts = do
  case (,) <$> lookup "rin" ts <*> lookup "bg" ts of
    Nothing ->
      throwError ["Texture not found: rin or bg"]
    Just (rint, bgt) ->
      pure $ State
        (g 380 800 96 96)
        (DL.fromList [])
        (mkBullet rint 8 5)
        (SBG.mkSBG bgt 1 (Size 800 1000) (Point 0 0))
      where
        g x y sw sh =
          MainChar
            { _pos = Point x y
            , _size = Size sw sh
            , _speed = 3
            , _texture = rint
            }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  let
    move = keysToMovement 5 input

    addBullets
      | keyClicked KeyA input =
        DL.append $ DL.fromList [(state ^. newBullet) (state ^. mc . pos)]
      | otherwise = id

    newState =
      state
        & over (mc . pos)
          ( over pX ((+) (move ^. pX))
          . over pY ((+) (move ^. pY))
          )
        & over bullets (updateList updateBullet . addBullets)
        & over bg SBG.updateSBG

  -- state stack manipulation
  if
    | keyReleased KeyQuit input ->
      throwError []
    | keyReleased KeyB input -> do
      next <- mkState [("rin", state ^. mc ^. texture)]
      pure (State.Push next, state)
    | keyReleased KeyQuit input ->
      pure (State.Done, state)
    | otherwise ->
      pure (State.None, newState)

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SBG.render renderer (state ^. bg)
  SDL.copy renderer (state ^. mc . texture) Nothing (Just $ toRect (state ^. mc . pos) (state ^. mc . size))
  forM_ (state ^. bullets) $ \bullet ->
    SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect (bullet ^. pos) (bullet ^. size))
