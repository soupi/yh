{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module TextBox where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified Play.Engine.MySDL.MySDL as MySDL

import Data.Monoid ((<>))
import Data.Maybe
import Control.Arrow
import Play.Engine.Utils
import Play.Engine.Types
import Play.Engine.Settings
import Play.Engine.Input
import Control.Lens
import Control.Monad.Except
import qualified Linear
import qualified Control.Monad.State as SM
import qualified Data.Text as T
import qualified Play.Engine.ListZipper as Zip


data Loc
  = Top
  | Bottom
  | All
  deriving (Show, Eq, Ord)

data TextBox
  = TextBox
  { _avatar :: Maybe SDL.Texture
  , _font :: SDLF.Font
  , _text :: Zip.ListZipper (Int, T.Text)
  , _posY :: !Loc
  , _pos :: {-# UNPACK #-} !IPoint
  , _size :: {-# UNPACK #-} !IPoint
  , _textSpeed :: Int
  , _textTimer :: Int
  }

makeFieldsNoPrefix ''TextBox

wantedAssets :: [(String, MySDL.ResourceType FilePath)]
wantedAssets =
  [ ("unispace", MySDL.Font "unispace/unispace.ttf")
  ]

make :: Loc -> Int -> T.Text -> Maybe SDL.Texture -> Maybe SDLF.Font -> Result TextBox
make _ _ _ _ Nothing =
  throwError ["A font was not provided to TextBox."]
make py spd txt mavatar (Just fnt) =
  pure $ TextBox
    { _avatar = mavatar
    , _font = fnt
    , _text =
      uncurry
        (Zip.ListZipper [])
      . (Prelude.head &&& tail)
      . zip (cycle [0])
      . concat
      . fmap (splitLine $ isJust mavatar)
      . T.lines
      $ txt
    , _posY = py
    , _pos = Point 0 0
    , _size = Point 0 0
    , _textSpeed = spd
    , _textTimer = spd
    }

splitLine :: Bool -> T.Text -> [T.Text]
splitLine hasAvatar txt =
  groupWhile (\t1 t2 -> plus1' (T.length t1) + T.length t2 <= (if hasAvatar then 44 else 52)) ""
  . T.words
  $ txt


groupWhile :: (T.Text -> T.Text -> Bool) -> T.Text -> [T.Text] -> [T.Text]
groupWhile test t1 = \case
  t2 : rest
    | test t1 t2 ->
      groupWhile test (t1 <> (if t1 == "" then "" else " ") <> t2) rest
    | otherwise  -> t1 : groupWhile test t2 rest
  [] -> [t1]

plus1' :: Int -> Int
plus1' n
  | n == 0 = 0
  | otherwise = n + 1

update :: Input -> TextBox -> Result (Maybe TextBox)
update input tb
  | keyClicked KeyA input
  , let (l, t) = Zip.last (tb ^. text) in l == T.length t
  = pure Nothing
  | otherwise = do
    wSize <- _windowSize <$> SM.get
    let
      locY = case tb ^. posY of
        Top -> 20
        All -> 20
        Bottom -> (wSize ^. y - 220)

    pure $ pure $ tb
      & set pos (Point 20 locY)
      & set size
        (Point (wSize ^. x - 40) $
          if tb ^. posY == All
            then wSize ^. y - 40
            else 200
        )
      & over text
        (\(txt :: Zip.ListZipper (Int, T.Text)) ->
           if
             | keyClicked KeyA input
             || tb ^. textSpeed == 0 ->
               fmap (\(_, t) -> (T.length t, t)) txt
             | tb ^. textTimer == 0
             && fst (Zip.get txt) < T.length (snd $ Zip.get txt) ->
               Zip.overCurr (first (+1)) txt
             | tb ^. textTimer == 0 ->
               Zip.nextStop txt
             | otherwise ->
               txt
        )
      & over textTimer (\t -> if t <= 0 then tb ^. textSpeed else t - 1)


render :: SDL.Renderer -> TextBox -> IO ()
render renderer tb
  | tb ^. size == Point 0 0 =
    pure ()
  | otherwise = do
  let
    rect = toRect (tb ^. pos) (tb ^. size)

  SDL.rendererDrawColor renderer SDL.$= Linear.V4 0 0 0 150
  SDL.fillRect renderer (Just rect)
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 0 160 200
  SDL.drawRect renderer (Just rect)

  loc <- case tb ^. avatar of
    Nothing -> pure (fmap (+20) $ tb ^. pos)
    Just av -> do
      SDL.copy
        renderer
        av
        Nothing
        (Just $ toRect
          (fmap (+20) $ tb ^. pos)
          (Point 96 96)
        )
      pure (Point (20 + tb ^. pos . x + 96 + 20) (tb ^. pos . y + 20))
  void $ traverse (renderText renderer tb loc) $ Zip.addIndex (tb ^. text)

renderText :: SDL.Renderer -> TextBox -> IPoint -> (Int, (Int, T.Text)) -> IO ()
renderText renderer tb loc (idx, (txtPart, txt)) =
  if txtPart == 0
    then pure ()
    else do
      texture <- SDL.createTextureFromSurface renderer
        =<< SDLF.solid
          (tb ^. font)
          (Linear.V4 255 255 255 255)
          (T.take txtPart txt)
      ti <- SDL.queryTexture texture
      SDL.copy
        renderer
        texture
        Nothing
        (Just $ toRect
          (over y ((+) (idx * 35)) loc)
          (Point (fromIntegral $ SDL.textureWidth ti) (fromIntegral $ SDL.textureHeight ti))
        )
      SDL.destroyTexture texture
