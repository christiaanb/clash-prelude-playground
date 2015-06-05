{-# LANGUAGE LambdaCase #-}
module SCaseTest where

import CLaSH.Prelude
import CLaSH.Signal.SynchronousCase

topEntity :: Signal (Maybe Integer) -> Signal Integer
topEntity x = match x $ \case
  Just_ a en  -> let s = regEn 0 en (a + s) in s
  Nothing_ en -> -1

testInput :: Signal (Maybe Integer)
testInput = match oscillate $ \case
    True_ en -> let s = regEn 0 en (s + 1) in Just <$> s
    False_ _ -> signal Nothing
  where
    oscillate = register False (not <$> oscillate)
