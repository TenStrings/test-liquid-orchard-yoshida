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
import EMonad
import GHC.Exts (Constraint)
import GHC.TypeLits
import Unsafe.Coerce
import Prelude hiding (Monad (..), fail, print, putStrLn)
import qualified Prelude as P

mainFunc = run $
  do
    putStrLn "Running division magic service"
    divProc

-- mainFunc = getEMonad newTest'
-- mainFunc = getEMonad newTest
-- mainFunc = getEMonad newTest''

data NonZero = NZ Int deriving (Show)

{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

divServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) =
  do
    x <- recv c
    (NZ y) <- recv c
    send d (x `div` y)

divClient ::
  Chan ( 'Op "c") ->
  Chan ( 'Op "d") ->
  Process
    '[ 'Op "c" 'Control.Effect.Sessions.:-> (Int ':! (NonZero ':! 'End)),
       'Op "d" 'Control.Effect.Sessions.:-> (Int ':? 'End)
     ]
    ()
divClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) =
  do
    send c (2 :: Int)
    send c (NZ 1)
    answer <- (recv d :: Process _ Int)
    putStrLn $ "result " ++ show answer

divProc = new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

-- this fails
-- nonzero:: NonZero
-- nonzero = NZ 0

-- this doesn't
nonzero :: NonZero
nonzero = NZ 1

newTest'' = eclean $ do
  chan <- eliftIO C.newChan
  chan <- eclean $ return (EMkChan chan :: EChan "chan")
  esend chan $ NZ 1
  (NZ t) <- erecv chan
  eliftIO $ P.putStrLn $ show t
  return ()