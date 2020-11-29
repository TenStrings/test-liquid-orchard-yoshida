module Fork where

import qualified Control.Concurrent as Conc
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Prelude

fork :: IO a -> IO a
fork x = do 
    res <- newEmptyTMVarIO 
    Conc.forkIO $ (x >>= (atomically . (putTMVar res)))
    atomically $ do { takeTMVar res }