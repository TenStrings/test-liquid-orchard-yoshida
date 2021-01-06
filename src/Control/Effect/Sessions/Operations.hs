-- This file defines effectful operations which encode the core operations
-- of the (session type) pi-calculus.

{-# LANGUAGE TypeOperators, DataKinds, GADTs, RankNTypes, FlexibleInstances,
              MultiParamTypeClasses, FlexibleContexts, IncoherentInstances,
              TypeFamilies, MagicHash, UnboxedTuples, ConstraintKinds #-}

{-@ LIQUID "--no-termination"       @-}
--- TODO: maybe try to remove this 

module Control.Effect.Sessions.Operations where

import Control.Effect.Sessions.Process
import Data.Type.FiniteMap

import Unsafe.Coerce
import Control.Concurrent ( threadDelay )
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as Conc

import Control.Monad.STM
import Control.Concurrent.STM.TMVar

{-| A process can be run if it is /closed/ (i.e., empty channel environment) -}
run :: Process '[] a -> IO a
run = getProcess

{-| Lift IO computations to a process -}
liftIO :: IO a -> Process '[] a
liftIO = Process

{-| Print to stdout in a process -}
print :: Show a => a -> Process '[] ()
print = liftIO . (Prelude.print)

{-| putStrLn in a process -}
putStrLn = liftIO . Prelude.putStrLn

-- The simplest operations, send and receive of primitive values,
-- take a named channel 'Chan c' and return a 'Process' computation
-- indexed by the session environment '[c :-> S]' where 'S' is either a
-- send or receive action (terminated by 'End').

{-| Send a primitive-typed value -}
send :: Chan c -> t -> Process '[c :-> t :! End] ()
send (MkChan c) t = Process $ C.writeChan (unsafeCoerce c) t 

{-| Receive a primitive-typed value -}
recv :: Chan c -> Process '[c :-> t :? End] t
recv (MkChan c) = Process $ C.readChan (unsafeCoerce c)

-- The 'new' combinator models $\nu$,  which takes
-- a function mapping from a pair of two channels names
-- 'Ch c' and 'Op C' to a session with behaviour 's', and creates
-- a session where any mention to 'Ch c' or 'Op c' is removed:

{-| Create a new channel and pass its two endpoints to the supplied continuation
     (the first parameter). This channel is thus only in scope for this continuation -}
new :: (Duality env c)
          =>  ((Chan (Ch c), Chan (Op c)) -> Process env t)
          ->  Process (env :\ (Op c) :\ (Ch c)) t
new f = Process $ C.newChan >>= (\c -> getProcess $ f (MkChan c, MkChan c))

{-| Parallel compose two processes, if they contain balanced sessions -}
par :: (BalancedPar env env') =>
        Process env () -> Process env' () -> Process (DisjointUnion env env') ()
par (Process x) (Process y) =  Process $ do res <- newEmptyTMVarIO
                                            res' <- newEmptyTMVarIO
                                            Conc.forkIO $ (x >>= (atomically . (putTMVar res)))
                                            Conc.forkIO $ (y >>= (atomically . (putTMVar res')))
                                            () <- atomically $ do { takeTMVar res }
                                            () <- atomically $ do { takeTMVar res' }
                                            return ()

{-| Turn all session types into 'balancing checked' session types |-}
type family AllBal (env :: [Map Name Session]) :: [Map Name Session] where
            AllBal '[] = '[]
            AllBal ((c :-> s) ': env) = (c :-> Bal s) ': (AllBal env)