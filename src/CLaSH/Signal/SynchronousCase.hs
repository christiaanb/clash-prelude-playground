{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}
module CLaSH.Signal.SynchronousCase
  (Match (..))
where

import CLaSH.Signal.Bundle   (unbundle')
import CLaSH.Signal          (not1)
import CLaSH.Signal.Explicit (Clock (..), Signal', sclock)
import CLaSH.Signal.Internal (joinSignal#)

import GHC.TypeLits          (KnownNat, KnownSymbol)

class Match a where
  data MatchCon a :: Clock -> *
  match :: (KnownSymbol name, KnownNat rate)
        => Signal' ('Clk name rate) a
        -> (MatchCon a ('Clk name rate) -> Signal' ('Clk name rate) b)
        -> Signal' ('Clk name rate) b

instance Match Bool where
  data MatchCon Bool clk = True_ (Signal' clk Bool) | False_ (Signal' clk Bool)
  match xs f = joinSignal# xs'
    where
      xs' = fmap (\case { True  -> f (True_ xs)
                        ; False -> f (False_ $ not1 xs)})
                 xs

instance Match (Maybe a) where
  data MatchCon (Maybe a) clk = Just_ (Signal' clk a) (Signal' clk Bool)
                              | Nothing_ (Signal' clk Bool)
  match xs f = joinSignal# xs'
    where
      (xs',a',justEn,nothingEn) =
        unbundle' sclock $
        fmap (\case { Just a  -> (f (Just_ a' justEn),a,True,False)
                    ; Nothing -> (f (Nothing_ nothingEn),undefined,False,True)})
             xs

instance Match (Either a b) where
  data MatchCon (Either a b) clk = Left_  (Signal' clk a) (Signal' clk Bool)
                                 | Right_ (Signal' clk b) (Signal' clk Bool)
  match xs f = joinSignal# xs'
    where
      (xs',a',b',leftEn,rightEn) =
        unbundle' sclock $
        fmap (\case { Left a  -> (f (Left_ a' leftEn),a,undefined,True,False)
                    ; Right b -> (f (Right_ b' rightEn),undefined,b,False,True)})
             xs
