{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-@ LIQUID "--prune-unsorted"        @-}

module Lib
  ( mainFunc,
    divClient,
    divServer,
    divProc,
  )
where

import Control.Effect.Sessions
import GHC.Exts (Constraint)
import GHC.TypeLits
import Prelude hiding (Monad (..), fail, print, putStrLn)

mainFunc = run $
  do
    putStrLn "Running division magic service"
    divProc

data NonZero = NZ Int

{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

divServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) =
  do
    x <- recv c
    (NZ y) <- recv c
    send d (x `div` y)

divClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) =
  let divisor = 0
   in do
        send c 2
        send c (NZ divisor)
        answer <- recv d
        putStrLn $ "Got " ++ show answer

divProc = new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

-- unsafe :: GT 0
-- unsafe = G 1

-- zerofail :: GT 0
-- unsafe = G 0

-- nonzero:: NonZero
-- nonzero = NZ 0