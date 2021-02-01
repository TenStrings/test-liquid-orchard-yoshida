{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--prune-unsorted"        @-}

module Lib where

import qualified Control.Concurrent.Chan       as C
import           Control.Effect.Sessions
import           Data.Type.Set           hiding ( (:->) )
import           GHC.Exts                       ( Constraint
                                                , IsString
                                                )
import           GHC.TypeLits
import           Unsafe.Coerce
import           Prelude                 hiding ( Monad(..)
                                                , fail
                                                , print
                                                , putStrLn
                                                )
import qualified Prelude                       as P

mainFunc = run $ do
  -- divProc >>
  -- simpleDelg >>
  -- repProc >>
  gtProc


data NonZero = NZ Int deriving (Show)

{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

divServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) = do
  x      <- recv c
  (NZ y) <- recv c
  send d (x `div` y)

-- divClient :: _ -> _ -> _
divClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) = do
  send c (2 :: Int)
  send c (NZ 0)
  answer <- recv d
  putStrLn $ "result " ++ show answer

divProc =
  new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

-- Delegation

-- serverD :: _ -> _
serverD (c :: (Chan (Ch "c"))) = do
  chRecvSeq c (\(d :: Chan (Ch "x")) -> send d (NZ 0))

clientD (c :: Chan (Op "c")) = new
  (\(d :: (Chan (Ch "d")), d') -> do
    chSend c d
    (NZ y) <- recv d'
    putStrLn $ show y
  )

simpleDelg = new $ \(c, c') -> serverD c `par` clientD c'

-- Recursive

repInp c p = affineFix
  (\f -> \() -> do
    (x :: Int) <- recv c
    print $ "received: " ++ show x
    case x of
      0 -> subL $ subEnd c $ return ()
      n -> subR $ do
        k <- chRecv c
        (k p) `par` (f ())
  )
  ()

serverA (c :: (Chan (Ch "c"))) = repInp
  c
  (\(d :: (Chan (Ch "d"))) -> do
    (NZ x) <- recv d
    print x
  )

-- clientA :: _ -> _
clientA (c :: (Chan (Op "c"))) = new
  (\(d :: Chan (Ch "d"), d') -> do
    send c (1 :: Int)
    rsend c d
    send d' (NZ 0)
    send c  (0 :: Int)
  )

repProc = new $ \(c, c') -> do
  (serverA c) `par` (clientA c')

-- Exp

gtServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) = do
  (TN x) <- recv c
  (TN y) <- recv c
  send d $ NZ (x - y)

gtClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) = do
  send c $ miunsafe2
  send c $ miunsafe2
  answer <- recv d
  putStrLn $ "result " ++ show answer

gtProc = new $ \(c, c') -> new $ \(d, d') -> gtServer c d `par` gtClient c' d'

-- THIS SHOULD BE UNSAFE
miunsafe2 :: TaggedN 1
miunsafe2 = TN 1

data TaggedN (n :: Nat) = TN Int

{-@ TN :: forall (n :: Symbol). {v:Int | n ~~ v } -> TaggedN n @-}
