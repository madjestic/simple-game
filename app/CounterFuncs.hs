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

data CFunc =
     CFunc
     { mvar  :: MVar Integer
     , value :: Integer
     } 

gameLoop :: CFunc -> StateT GameState IO GameState
gameLoop cf = do
  let count0 = value cf
  liftIO $ delay 100
  state <- get
  modify $ flip inc 1
  liftIO . putStrLn $ "gameLoop state: " ++ show (tick state)
  count1 <- liftIO $ tryTakeMVar (mvar cf)
  let count = fromMaybe count0 count1
  gameLoop cf{value = count} 

startState :: GameState
startState = GameState { tick = 0 }

runGame :: CFunc -> IO GameState
runGame fc = do
  evalStateT
    updateState
    startState
    where
      updateState :: StateT GameState IO GameState
      updateState = do
        defaultState
        gameLoop fc 
        
counterA :: MVar Integer -> Integer -> IO ()
counterA mvar n = do
  threadDelay 1000000
  putMVar mvar n
  putStrLn $ "Counter: " ++ show n
  counterA mvar (n + 1)

main :: IO ()
main = do
  let initCount = 1
  putStrLn "Starting Game"
  
  count <- newMVar initCount
  _ <- forkIO $ counterA count initCount
  runGame $ CFunc count initCount
  putStrLn "Exiting Game"

