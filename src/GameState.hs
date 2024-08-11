module GameState where

--import Control.Monad.State as State
import SDL.Vect
import Foreign.C.Types

data GameState =
     GameState
     {
       tick      :: Integer
     , increment :: Integer
     , quitGame  :: Bool
     , mpos      :: Point V2 CInt
     } deriving Show

defaultGame :: GameState
defaultGame =
     GameState
     {
       tick      = 128
     , increment = 0
     , quitGame  = False
     , mpos      = P (V2 0 0)
     } 
