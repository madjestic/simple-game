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

data Game
  =  Game
  { tick    :: Integer
  , actors  :: [Actor]
  , commods :: [Commodity]
  , crs     :: [Contract]
  } deriving Show

data Commodity
  = Commodity
  { lableC   :: String
  , balance  :: Integer
  , deltaC   :: Integer
  } deriving Show

data Actor
  = Actor
  { lableA  :: String
  , commod  :: MVar Commodity
  , delta   :: Integer
  , capital :: Integer
  }

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

data Contract
  = Contract
  { lableCC  :: String
  , agent0   :: MVar Actor     -- Agent
  , agent1   :: MVar Actor     -- Contragent
  , commodCC :: MVar Commodity
  , time     :: Integer
  }

instance Show Contract where
  show :: Contract -> String
  show (Contract l _ _ _ t) =
    "Contract: " ++ show l ++ "\n" ++
    "time: "     ++ show t
  
updateActor :: Actor -> Contract -> IO Actor
updateActor act contr = do  
  --let result = act { capital = capital act + (delta act - delta') }
  act' <- resolve act contr -- resolve contract
  let result = act { capital = undefined }
  
  act' <- readMVar $ commod act
  return $ result

resolve :: Actor -> Contract -> IO Actor
resolve act contr = do
  c0 <- readMVar $ commod act
  let
    d0     = balance c0
    delta' =  d0 - balance c0
  print act

  return $ act { capital = capital act + (delta act - delta') }

matchActors :: [Actor] -> [(Actor, Actor)]
matchActors acts = zip producers' consumers' 
  where
    (producers, consumers) = break (\act -> delta act >= 0) acts
    producers' = sortBy (\x y -> compare (delta x) (delta y)) producers
    consumers' = sortBy (\x y -> compare (delta x) (delta y)) consumers

makeContracts :: [(Actor, Actor)] -> ([Contract], [Contract])
makeContracts aprs = (psc, csc) -- aprs - Actor Pairs
  where
    psc = undefined 
    csc = undefined

runGame :: Game -> IO Game
runGame game = do
  evalStateT
    updateState
    game
    
    where
      updateState :: StateT Game IO Game
      updateState = do
        innerMonad
        gameLoop
        
          where
            innerMonad :: StateT Game IO ()
            innerMonad = do return ()
      

gameLoop :: StateT Game IO Game
gameLoop = do
  lift $ threadDelay 1000000
  
  g0 <- get
  lift $ mapM_ preTrade $ actors g0
  crs' <- updateContracts $ crs g0
  modify $ updateGameContracts crs'
  
  gameLoop
    where
      updateGameContracts :: [Contract] -> Game -> Game
      updateGameContracts crs0 g0 = g0 { crs = crs0 }
      
      preTrade :: Actor -> IO ()
      preTrade act = do
        act' <- takeMVar (commod act)
        putMVar (commod act) (act' { balance  = balance act' + delta act })
        
        cmd  <- readMVar (commod act)
        putStrLn $ show cmd ++ "\n"

      updateContracts :: [Contract] -> StateT Game IO [Contract]
      updateContracts crs = do
        lift $ mapM updateContract crs
        return undefined

      updateContract :: Contract -> IO Contract
      updateContract cr = do
        return undefined
          
toMVars :: [Actor] -> IO [MVar Actor]
toMVars acs = do
  mapM newMVar acs

main :: IO ()
main = do
  
  let water
        = Commodity
        { lableC  = "Water"
        , balance = 0
        , deltaC  = 0  
        }
  waterMV <- newMVar water

  let earth
        = Actor
        { lableA  = "Earth"
        , commod  = waterMV
        , delta   = 1
        , capital = 1000
        }
  earthMV <- newMVar earth        

  let moon
        = Actor
        { lableA  = "Moon"
        , commod  = waterMV
        , delta   = -1
        , capital = 1000
        }
        
  moonMV  <- newMVar moon
  
  let cr0
        = Contract
        { lableCC  = "Earth -> Moon"
        , agent0   = earthMV
        , agent1   = moonMV
        , commodCC = waterMV
        , time     = 10
        }

  let game
        = Game
        { tick    = 0
        , actors  = [moon, earth]
        , commods = [water]
        , crs     = [cr0]
        }
  
  runGame game
  return ()
