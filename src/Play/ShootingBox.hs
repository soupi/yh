{-# LANGUAGE TemplateHaskell #-}

module Play.ShootingBox where

import qualified SDL
import qualified SDL
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Word
import Data.Foldable
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
import qualified Data.DList as DL

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
  , _bullets :: DL.DList MainChar
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
    Just t ->
      pure $ State
        (g 380 500 96 96)
        (DL.fromList [])
      where
        f x y =
          MainChar
            { _pos = Point ((300+) $ x * 10) (400 + (y * 20))
            , _size = Size 30 30
            , _speed = 3
            , _texture = t
            }
        g x y sw sh =
          MainChar
            { _pos = Point x y
            , _size = Size sw sh
            , _speed = 3
            , _texture = t
            }

update :: Input -> State -> Result (State.Command, State)
update input state = do
  wSize <- _windowSize <$> SM.get
  let
    move = keysToMovement 5 input

    updateBullet b =
      if b ^. pos . pY < 5
        then []
        else [over (pos . pY) (flip (-) (b ^. speed)) b]

    addBullets
      | keyClicked KeyA input =
        DL.append $ DL.fromList [set size (Size 30 30) (state ^. mc)]
      | otherwise = id

    newState =
      state
        & over (mc . pos)
          ( over pX ((+) (move ^. pX))
          . over pY ((+) (move ^. pY))
          )
        & over bullets (updateList updateBullet . addBullets)

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

updateList :: (a -> [a]) -> DL.DList a -> DL.DList a
updateList f = DL.foldr (\x acc -> DL.fromList (f x) `DL.append` acc) DL.empty

render :: SDL.Renderer -> State -> IO ()
render renderer state = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  SDL.copy renderer (state ^. mc ^. texture) Nothing (Just $ toRect $ state ^. mc)
  forM_ (state ^. bullets) $ \bullet ->
    SDL.copy renderer (bullet ^. texture) Nothing (Just $ toRect bullet)

toRect :: MainChar -> SDL.Rectangle C.CInt
toRect mc =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . over both fromIntegral . pointToTuple $ mc ^. pos)
    (uncurry Linear.V2 . over both fromIntegral . sizeToTuple $ mc ^. size)
