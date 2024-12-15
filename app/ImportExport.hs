{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad.State
import Control.Monad (when)
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
  , time     :: Int
  , status   :: Status
  }

data Status = Valid | Invalid
  deriving (Show, Eq)

instance Show Contract where
  show :: Contract -> String
  show (Contract l _ _ _ t s) = "\n" ++
    "Contract: " ++ show l ++ "\n" ++
    "time: "     ++ show t ++ "\n" ++
    "status: "   ++ show s
  
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

loop :: Game -> IO ()
loop g0 = do
  putStrLn $ "# Step: " ++ show (tick g0) 
  threadDelay 1000000
  mapM_ preTrade $ actors g0
  
  crs' <- updateContracts $ crs g0
  let g1 = g0
        { tick = tick g0 + 12
        , crs = crs' }
  
  draw g1

  loop $ gc g1 -- garbage collect invalid contracts
    where
      preTrade :: Actor -> IO ()
      preTrade act = do
        act'    <- takeMVar (commod act)
        putMVar (commod act) (act' { balance  = balance act' + delta act })

      updateContracts :: [Contract] -> IO [Contract]
      updateContracts = mapM updateContract

      updateContract :: Contract -> IO Contract
      updateContract cr = do
        threadDelay 300000
        let prob = 25
        if (status cr == Valid) && time cr >= 0 
          then do
            rnd <- randomRIO (0, 100) :: IO Int
            putStrLn $ "rnd :" ++ show rnd
            if rnd >= prob
              then do
                   return $ cr { time = time cr - 1}
              else do
                   return $ cr { status = Invalid }
          else do
            cv <- readMVar (commodCC cr)
            putStrLn $ "-> Result: " ++ show cv
            return cr

gc :: Game -> Game
gc g0 = -- collect invalid contracts
  g0 { crs = filter (\cr' -> status cr' == Valid) $ crs g0 }

draw :: Game -> IO ()
draw g0 = do
  mapM_ (\(ds, ls) -> do putStrLn ds >> flip' ls) (draw' <$> crs g0 :: [(String, Int)])
  
draw' :: Contract -> (String, Int)
draw' cr = 
  ("[E" ++ h p ++ [chr] ++ t p ++ "M]" -- h,t - head,tail
   ++ "\n" ++
   statusUpdate, ls)
  where
    chr = case status cr of
      Valid   -> '>'
      Invalid -> 'X'
    (statusUpdate, ls) = case status cr of
      Valid   -> ("Contract Status: OK", 10)
      Invalid -> ("Contract Status: INVALID"  ++ "\n"
                  ++ " -!!!!!!!!!!!!!!!" ++ "\n"
                  ++ " -!!! -ALERT -!!!" ++ "\n"
                  ++ " -!!!!!!!!!!!!!!!" ++ "\n"
                  ++ "\n -Pirates -attacked -the -transport!", 6)
    d   = 10 :: Int             -- distance
    p   = d - time cr
    h p = replicate p       '+' -- head
    t p = replicate (d-p-1) '-' -- tail

flip' :: Int -> IO ()
flip' n =
  mapM_ (\_ -> putStrLn "") [0..n-1]
          
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
  waterCommodMVar <- newMVar water

  let earth
        = Actor
        { lableA  = "Earth"
        , commod  = waterCommodMVar
        , delta   = 1
        , capital = 1000
        }
  earthMV <- newMVar earth        

  let moon
        = Actor
        { lableA  = "Moon"
        , commod  = waterCommodMVar
        , delta   = -1
        , capital = 1000
        }
        
  moonMV  <- newMVar moon
  
  let cr0
        = Contract
        { lableCC  = "Earth > Moon"
        , agent0   = earthMV
        , agent1   = moonMV
        , commodCC = waterCommodMVar
        , time     = 10
        , status   = Valid
        }

  let game
        = Game
        { tick    = 0
        , actors  = [moon, earth]
        , commods = [water]
        , crs     = [cr0]
        }
  
  putStrLn $ "\n ===> A Space Opera <===\n" 
  threadDelay 1000000

  loop game

  putStrLn "  Game Over  "
