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

import qualified Control.Concurrent.Chan as C
import Control.Effect.Sessions
import Data.Type.Set hiding ((:->))
import GHC.Exts (Constraint)
import GHC.TypeLits
import Unsafe.Coerce
import Prelude hiding (Monad (..), fail, print, putStrLn)
import qualified Prelude as P

mainFunc = run $ 
  -- divProc
  simpleDelg

data NonZero = NZ Int deriving (Show)

{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

divServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) =
  do
    x <- recv c
    (NZ y) <- recv c
    send d (x `div` y)

-- divClient :: _ -> _ -> _
divClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) =
  do
    send c (2 :: Int)
    send c (NZ 0)
    answer <- recv d
    putStrLn $ "result " ++ show answer

divProc = new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

serverD :: _ -> _
serverD (c :: (Chan (Ch "c"))) = do chRecvSeq c (\(d :: Chan (Ch "x")) -> send d (NZ 0))

clientD (c :: Chan (Op "c")) =
       new (\(d :: (Chan (Ch "d")), d') ->
                             do  chSend c d
                                 (NZ y) <- recv d'
                                 putStrLn $ show y)

simpleDelg = new $ \(c, c') -> serverD c `par` clientD c'
