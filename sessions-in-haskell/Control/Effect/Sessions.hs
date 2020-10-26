module Control.Effect.Sessions
  ( Process (..),
    Chan (..),
    Name (..),
    Symbol,
    Session (..),
    Delegate (..),
    Dual,
    DualP,
    SessionSeq,
    Balanced,
    BalancedPar,
    NotBal,
    Effect (..),
    Control.Effect.fail,
    run,
    Map (..),
    (:@),
    Union,
    (:\),
    send,
    recv,
    new,
    par,
    rsend,
    chSend,
    chRecv,
    chRecvSeq,
    sub,
    subL,
    subR,
    subEnd,
    affineFix,
    caseUnion,
    print,
    putStrLn,
    liftIO,
    ifThenElse,
  )
where

import Control.Effect
import Control.Effect.Sessions.Operations
import Control.Effect.Sessions.Process
import Data.Type.FiniteMap
import GHC.TypeLits
import Prelude hiding (Monad (..), print, putStrLn)

chRecvSeq c k = (chRecv c) >>= (\kf -> kf k)
