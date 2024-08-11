{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad.State
import qualified Control.Concurrent
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, tryTakeMVar, readMVar, threadDelay, forkIO)
import SDL.Time (delay)
import Data.Maybe (fromMaybe)

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

gameLoop :: MVar Integer -> Integer -> StateT GameState IO GameState
gameLoop mvar count0 = do
  liftIO $ delay 100
  state <- get
  modify $ flip inc (1 + count0)
  liftIO . putStrLn $ "gameLoop state: " ++ show (tick state)
  count1 <- liftIO $ tryTakeMVar mvar
  let count = fromMaybe count0 count1
  gameLoop mvar count

startState :: GameState
startState = GameState { tick = 0 }

runGame :: MVar Integer -> IO GameState
runGame mvar = do
  evalStateT
    updateState
    startState
    where
      updateState :: StateT GameState IO GameState
      updateState = do
        defaultState
        gameLoop mvar 0
        
counterThread :: MVar Integer -> Integer -> IO ()
counterThread mvar n = do
  threadDelay 1000000
  putMVar mvar n
  putStrLn $ "Counter: " ++ show n
  counterThread mvar (n + 1)

main :: IO ()
main = do
  let initCount = 1
  putStrLn "Starting Game"
  count <- newMVar initCount
  _ <- forkIO $ counterThread count initCount
  runGame count
  putStrLn "Exiting Game"
