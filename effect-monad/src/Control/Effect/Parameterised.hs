-- Used to make the types extra clear
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This module implement parameterised monads due to Bob Atkey
-- (see 'Parameterised Notions of Computing' JFP 2009)
-- also defined in Control.Monad.Indexed (category-extras)

module Control.Effect.Parameterised ((>>), PMonad (..), failE, ifThenElse) where

-- Bye Monads... as we know them
import Prelude hiding (Monad (..), fail)

-- Hello Parameterised Monads
class PMonad (pm :: k -> k -> * -> *) where
  -- Lift pure values into effect-invariant computations
  return :: a -> pm inv inv a

  -- Sequentially compose effectful computations
  (>>=) :: pm pre interm t -> (t -> pm interm post t') -> pm pre post t'

-- Other boilerplate
(>>) :: PMonad pm => pm pre mid t -> pm mid post t' -> pm pre post t'
x >> y = x >>= const y

-- TODO: find why does the name matter here
{-@ failE :: { s: String | False } -> _ @-}
failE :: String -> m inv inv a
failE = error

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y