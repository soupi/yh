{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

module Play.Engine.ScrollingBackground where

import qualified SDL

import Play.Engine.Types
import Play.Engine.Utils
import Control.Lens

data SBG
  = SBG
  { _sbg1 :: BG
  , _sbg2 :: BG
  , _size :: !Size
  , _speed :: !Int
  , _texture :: SDL.Texture
  }

data BG
  = BG
  { _pos :: !IPoint
  }

makeFieldsNoPrefix ''BG
makeFieldsNoPrefix ''SBG

mkSBG :: SDL.Texture -> Int -> Size -> IPoint -> SBG
mkSBG txt spd sz position =
  let
    bg = BG
      { _pos = position
      }
  in
    SBG
    { _sbg1 = over (pos . y) (flip (-) (sz ^. y)) bg
    , _sbg2 = bg
    , _size = sz
    , _speed = negate spd
    , _texture = txt
    }

updateSBG :: SBG -> SBG
updateSBG sbg =
  let
    sbg' =
      if (sbg ^. sbg2 . pos . y) > (sbg ^. size . y)
        then
          sbg
            & over (sbg1 . pos . y) (flip (-) (sbg ^. size . y))
            & set sbg2 (sbg ^. sbg1) 
        else
          sbg
  in
    sbg'
      & over (sbg1 . pos . y) (flip (-) (sbg' ^. speed))
      & over (sbg2 . pos . y) (flip (-) (sbg' ^. speed))

render :: SDL.Renderer -> Camera -> SBG -> IO ()
render renderer cam sbg = do
  SDL.copy renderer (sbg ^. texture) Nothing (Just $ toRect (cam $ sbg ^. sbg1 . pos) (sbg ^. size))
  SDL.copy renderer (sbg ^. texture) Nothing (Just $ toRect (cam $ sbg ^. sbg2 . pos) (sbg ^. size))
