{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad.State
import SDL.Time (delay)

data GameState =
     GameState
     {
       tick :: Integer
     } deriving Show

defaultState :: StateT GameState IO ()
defaultState = do
  return ()

inc :: GameState -> Integer -> GameState
inc (GameState c) n = GameState ( c + n )

loop :: StateT GameState IO GameState
loop = do
  liftIO $ delay 100
  state <- get
  modify $ flip inc 1
  liftIO $ print state
  loop

startState :: GameState
startState = GameState { tick = 0 }

runGame :: IO GameState
runGame = do
  evalStateT
    updateState
    startState
    where
      updateState :: StateT GameState IO GameState
      updateState = do
        defaultState
        loop

main :: IO ()
main = do
  putStrLn "Starting Game"
  runGame
  putStrLn "Exiting Game"
