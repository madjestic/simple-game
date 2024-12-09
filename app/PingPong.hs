{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad.State
import Control.Monad (when)  
import qualified Control.Concurrent
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, tryTakeMVar, readMVar, threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import System.Random ( randomRIO, randomIO )

--data Game = ...

data Actor =
  Actor
  { lable :: String
  , ping  :: MVar Bool
  , score :: MVar Bool
  }

data Actor' =
  Actor'
  { lable' :: String
  , ping'  :: Bool
  } deriving Show
  
toActor' :: Actor -> IO Actor'
toActor' act = do
  ping' <- readMVar (ping act)
  return
    Actor'
    { lable' = lable act
    , ping'  = ping'
    }

gameLoop :: (Actor, Actor) -> (Integer, Integer) -> IO ()
gameLoop (p0,p1) (s0,s1) = do
  --threadDelay 1000000
  s0'' <- takeMVar (score p0)
  s1'' <- takeMVar (score p1)
  let
    s0' = if s0'' then 1 else 0
    s1' = if s1'' then 1 else 0
  print $ "score :" ++ show(s0, s1)
  gameLoop (p0,p1) (s0 + s0', s1 + s1')

update :: Actor -> IO ()
update x = do
  delta <- randomRIO (-1000000, 2000000)
  threadDelay $ 1000000 + delta
  ping' <- takeMVar (ping x)
  --putStrLn $ lable x ++ ": " ++ if ping' then "Ping!" else "Pong."
  let (pingS, pingV, scoreV) = match ping'
  
  putStrLn $ lable x ++ ": " ++ pingS
  
  putMVar (ping x) pingV
  when scoreV $ putMVar (score x) scoreV
  update x
    where
      match ping' = case (lable x, ping') of
        ("A", True)  -> ("Ping!", False, False)
        ("B", False) -> ("Pong.", True, False)
        _ -> ("drops the ball!", not ping', True)

main :: IO ()
main = do
  ping'  <- newMVar True
  score' <- newMVar False
  
  let
    actor0 =
      Actor
      { lable = "A"
      , ping  = ping'
      , score = score'
      }
    actor1 =
      Actor
      { lable = "B"
      , ping  = ping'
      , score = score'
      }
  axs <- mapM toActor' [actor0, actor1]
  _ <- forkIO $ update actor0
  _ <- forkIO $ update actor1
  gameLoop (actor0, actor1) (0,0)

