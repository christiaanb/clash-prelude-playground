{-# LANGUAGE LambdaCase #-}
module SCaseTest where

import CLaSH.Prelude
import CLaSH.Signal.SynchronousCase

-- | Oscillate between @False@ and @True@ every cycle
oscillate = register False (not <$> oscillate)

-- | Counter which increment by the value of @dIn@ when @enable@ is @True@
counter enable dIn = cnt
  where
    cnt = regEn 0 enable (cnt + dIn)

topEntity :: Signal (Maybe Integer) -> Signal Integer
topEntity x = match x $ \case
  Just_ a en  -> counter en a
  Nothing_ en -> -1

testInput :: Signal (Maybe Integer)
testInput = match oscillate $ \case
    True_ en -> Just <$> counter en 1
    False_ _ -> signal Nothing
