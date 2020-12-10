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
  do
    putStrLn "Running division magic service"
    divProc

data NonZero = NZ Int deriving (Show)

{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

divServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) =
  do
    x <- recv c
    (NZ y) <- recv c
    send d (x `div` y)

-- divClient ::
--   Chan ( 'Op "c") ->
--   Chan ( 'Op "d") ->
--   Process
--     '[ 'Op "c" 'Control.Effect.Sessions.:-> (Int ':! (NonZero ':! 'End)),
--        'Op "d" 'Control.Effect.Sessions.:-> (Int ':? 'End)
--      ]
--     ()
divClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) =
  do
    send c (2 :: Int)
    send c (NZ 0)
    answer <- recv d
    putStrLn $ "result " ++ show answer

divProc = new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'