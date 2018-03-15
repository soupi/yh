{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
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
  { _pos :: !Point
  }

makeFieldsNoPrefix ''BG
makeFieldsNoPrefix ''SBG

mkSBG :: SDL.Texture -> Int -> Size -> Point -> SBG
mkSBG txt spd sz position =
  let
    bg = BG
      { _pos = position
      }
  in
    SBG
    { _sbg1 = over (pos . pY) (flip (-) (sz ^. sH)) bg
    , _sbg2 = bg
    , _size = sz
    , _speed = negate spd
    , _texture = txt
    }

updateSBG :: SBG -> SBG
updateSBG sbg =
  let
    sbg' =
      if (sbg ^. sbg2 . pos . pY) > (sbg ^. size . sH)
        then
          sbg
            & over (sbg1 . pos . pY) (flip (-) (sbg ^. size . sH))
            & set sbg2 (sbg ^. sbg1) 
        else
          sbg
  in
    sbg'
      & over (sbg1 . pos . pY) (flip (-) (sbg' ^. speed))
      & over (sbg2 . pos . pY) (flip (-) (sbg' ^. speed))

render :: SDL.Renderer -> SBG -> IO ()
render renderer sbg = do
  SDL.copy renderer (sbg ^. texture) Nothing (Just $ toRect (sbg ^. sbg1 . pos) (sbg ^. size))
  SDL.copy renderer (sbg ^. texture) Nothing (Just $ toRect (sbg ^. sbg2 . pos) (sbg ^. size))
