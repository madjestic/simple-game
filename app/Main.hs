{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL

-- import Graphics.RedViz.Rendering (openWindow)

import Game

main :: IO ()
main = do
  let
    resX = 800
    resY = 600
  
  initializeAll
  window   <- createWindow "Simple Game" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX`div`2) (resY`div`2)))
  _ <- cursorVisible $= True
  
  runGame renderer
  putStrLn "Exiting Game"
  
