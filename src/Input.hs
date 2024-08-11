module Input(handleEvents)
where

--import Foreign.C.Types
import Control.Monad.State as State
import SDL

import GameState

-- import Debug.Trace as DT

mapKeyEvents
  :: [(Scancode, StateT GameState IO ())]
mapKeyEvents =
  [
    (ScancodeW, inc   10)
  , (ScancodeS, inc (-10))
  , (ScancodeQ, exit True)
  ]

exit :: Bool -> StateT GameState IO ()
exit b = modify $ quit' b

quit' :: Bool -> GameState -> GameState
quit' b g = g { quitGame = b}

inc :: Integer -> StateT GameState IO ()
inc n = modify $ inc' n

inc' :: Integer -> GameState -> GameState
inc' k (GameState c _ q _) =
  GameState
  { tick      = c + k
  , increment = k
  , quitGame  = q
  }

updateMouse :: StateT GameState IO ()
updateMouse = do
  mp <- getAbsoluteMouseLocation
  modify $ updateMpos mp
  where
    updateMpos xy game =
      game { mpos = xy }

processEvent :: (Monad m) => [(Scancode , m ())] -> Event -> m ()
processEvent mapping e =
  let mk = case eventPayload e of
             KeyboardEvent keyboardEvent -> Just
               ( keyboardEventKeyMotion keyboardEvent == Pressed
               , keysymScancode (keyboardEventKeysym keyboardEvent))
             _ -> Nothing
  in case mk of
       Nothing     -> return ()
       Just (e', k) -> case lookup k mapping of
                        Nothing -> return ()
                        Just k  -> k

updateKeyboard :: (Monad m) => [(Scancode, m ())] -> [Event] -> m ()
updateKeyboard ns = mapM_ (processEvent ns)

isQuit :: EventPayload -> Bool
isQuit ev =
  case ev of
    KeyboardEvent keyboardEvent -> 
      keyboardEventKeyMotion keyboardEvent                  == Pressed
      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
    QuitEvent -> True
    _         -> False

handleEvents :: StateT GameState IO Bool
handleEvents = do
  events <- SDL.pollEvents
  updateKeyboard mapKeyEvents events
  updateMouse
  let result = any isQuit $ fmap eventPayload events :: Bool
  return result
