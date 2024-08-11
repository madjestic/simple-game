{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP    #-}

module Game where

import Control.Monad.State as State
import SDL hiding (get)
import Unsafe.Coerce      ( unsafeCoerce )
--import Control.Lens       ( toListOf, view, (^..), (^.), bimap)

import GameState
import Input

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

gameLoop :: Renderer -> StateT GameState IO ()
gameLoop renderer = do
  liftIO $ delay 1
  quitGame' <- handleEvents
  when debug $ get >>= (liftIO . print)
  GameState t _ _ mp <- get
  let mousePos = (\(V2 x y) -> (unsafeCoerce x,unsafeCoerce y)) (unP mp)
  rendererDrawColor renderer $= V4 (fromIntegral t) (fst mousePos) (snd mousePos) 255
  clear renderer
  present renderer
  unless quitGame' $ gameLoop renderer

defaultGameState :: StateT GameState IO ()
defaultGameState = do
  pure ()

runGame :: Window -> IO ()
runGame window = do
  renderer <- createRenderer window (-1) defaultRenderer
  evalStateT (do
      defaultGameState
      gameLoop renderer)
      defaultGame 
