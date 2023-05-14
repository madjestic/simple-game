{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL

import Game

main :: IO ()
main = do
  initializeAll
  window   <- createWindow "Stepper_SDL" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runGame renderer
  putStrLn "Exiting Game"
  
