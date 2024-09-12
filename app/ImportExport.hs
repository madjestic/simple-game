{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad.State
import qualified Control.Concurrent
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, tryTakeMVar, readMVar, threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.List
import System.Random ( randomRIO, randomIO )

data Commodity
  = Commodity
  { lableC   :: String
  , balance  :: Integer
  } deriving Show

data Actor
  = Actor
  { lableA  :: String
  , commod  :: MVar Commodity
  , delta   :: Integer
  , capital :: Integer
  }

data Contract
  = Contract
  { lableCC  :: String
  , agent0   :: Actor -- Agent
  , agent1   :: Actor -- Contragent
  , commodCC :: Commodity
  , time     :: Integer
  } deriving Show

instance Show Actor where
  show :: Actor -> String
  show (Actor l _ d c) =
    "Actor: "   ++ show l ++ "\n" ++
    if d > 0
    then
      "sells:   " ++ show (abs d) ++ "\n" ++
      "capital: " ++ show c
    else
      "buys:    " ++ show (abs d) ++ "\n" ++
      "capital: " ++ show c
  
updateCommod :: Actor -> IO ()
updateCommod act = do
  c' <- takeMVar (commod act)
  putMVar (commod act) (c' { balance  = balance c' + delta act })

updateActor :: Integer -> Actor -> IO Actor
updateActor delta' act = do
  print act
  c' <- readMVar $ commod act
  return $ act { capital = capital act + (delta act - delta') }

matchActors :: [Actor] -> ([Actor], [Actor])
matchActors acts = (producers', consumers') 
  where
    (producers, consumers) = break (\act -> delta act >= 0) acts
    producers' = sortBy (\x y -> compare (delta x) (delta y)) producers
    consumers' = sortBy (\x y -> compare (delta x) (delta y)) consumers

makeContracts :: ([Actor], [Actor]) -> ([Contract], [Contract])
makeContracts (ps, cs) = (psc, csc)
  where
    psc = undefined 
    csc = undefined 
  
gameLoop :: MVar Commodity -> [Actor] -> IO ()
gameLoop cmv acts = do
  threadDelay 1000000
  c0 <- readMVar cmv
  let d0 = balance c0
  mapM_ updateCommod acts
  
  c1 <- readMVar cmv
  let d  = d0 - balance c1
  
  acts <- mapM (updateActor d) acts
  mv'   <- readMVar cmv
  putStrLn $ show mv' ++ "\n"
  gameLoop cmv acts

toMVars :: [Actor] -> IO [MVar Actor]
toMVars acs = do
  mapM newMVar acs

main :: IO ()
main = do
  let water
        = Commodity
        { lableC  = "Water"
        , balance = 0
        }
  waterMV <- newMVar water

  let earth
        = Actor
        { lableA  = "Earth"
        , commod  = waterMV
        , delta   = 1
        , capital = 1000
        }

  let moon
        = Actor
        { lableA  = "Moon"
        , commod  = waterMV
        , delta   = -1
        , capital = 1000
        }
        
  gameLoop waterMV [earth, moon]
