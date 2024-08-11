import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, takeMVar, putMVar)
import Control.Monad (forever)

-- Function to run the counter in the secondary thread
counterThread :: MVar Int -> Int -> IO ()
counterThread mvar n = do
  threadDelay 100000
  putMVar mvar n    
  putStrLn $ "Counter: " ++ show n
  counterThread mvar (n + 1)

mainThread :: MVar Int -> IO ()
mainThread mvar = forever $ do
  count <- takeMVar mvar
  if count `mod` 10 == 0
    then putStrLn $ "Tick: " ++ show count
    else return ()

main :: IO ()
main = do
  mvar <- newMVar 0                  -- Create an MVar to hold the counter value
  _ <- forkIO $ counterThread mvar 0 -- Fork the secondary thread
  mainThread mvar                    -- Run the main thread
