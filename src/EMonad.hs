{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-@ LIQUID "--prune-unsorted"        @-}

module EMonad where

import Control.Effect.Sessions
import Data.Type.Set hiding ((:->))
import GHC.Exts (Constraint)
import GHC.TypeLits
import Prelude hiding (Monad (..), fail, print, putStrLn)
import qualified Prelude as P
import qualified Control.Concurrent.Chan as C
import Unsafe.Coerce
import qualified Control.Concurrent as Conc
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Fork (fork)

type family LConcat (a :: [*]) (b :: [*]) where
  LConcat '[] ys = ys
  LConcat (x ': xs) ys = x ': (LConcat xs ys)

type family EClean (a :: [*]) where
    EClean xs = '[]

data EMonad (s :: [*]) a = EMonad { getEMonad :: IO a }

instance Effect EMonad where
   type Plus EMonad s t = LConcat s t
   type Unit EMonad     = '[]
   type Inv EMonad s t  = () 

   return :: a -> EMonad (Unit EMonad) a
   return a = EMonad (P.return a)

   (>>=) :: (Inv EMonad s t) => EMonad s a -> (a -> EMonad s' b) -> EMonad (Plus EMonad s s') b
   x >>= k = EMonad ((P.>>=) (getEMonad x) (getEMonad . k))

newtype Tagged (k :: Symbol) b = Tagged { untag :: b } deriving Show

eprint :: Show t => t -> EMonad '[t] ()
eprint t = EMonad $ P.putStrLn $ show t
-- 
data EChan (n :: Symbol) = forall a . EMkChan (C.Chan a)

esend :: EChan cname -> t -> EMonad '[t] () 
esend (EMkChan c) t = EMonad $ C.writeChan (unsafeCoerce c) t

erecv :: EChan cname -> EMonad '[t] t
erecv (EMkChan c) = EMonad $ C.readChan (unsafeCoerce c)

eliftIO :: IO a -> EMonad '[] a
eliftIO = EMonad

eclean :: EMonad l a -> EMonad (EClean l) a
eclean = EMonad . fork . getEMonad