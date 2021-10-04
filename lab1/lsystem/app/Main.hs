module Main where

import LSystem

import Graphics.WorldTurtle

eval :: Float -> Float -> Dir -> TurtleCommand ()
eval step angle F = forward step
eval step angle R = right angle
eval step angle L = left angle

shape = evolve $ evolve axiom

main :: IO ()
main = runTurtle $ do
  mapM_ (eval 90 90) shape
