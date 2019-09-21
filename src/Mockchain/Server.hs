{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This module implements a 'Server' which receives 'broadcast' 'Transaction's.

A 'TXID' is assigned to each 'Transaction', whose 'Status' can be queried with
'get'.

After some time a 'Pending' 'Transaction' is either 'Accepted' or 'Rejected',
based on the criteria set out in the 'Config' that was given to 'new' to
create the 'Server'.

-}

module Mockchain.Server
    ( Config (..)
    , Status (Pending, Accepted, Rejected)
    , Server, new, broadcast, get
    , Result (..)
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, ToJSON, toEncoding
                     , genericToEncoding, defaultOptions
                     )


-- base ----------------------------------------------------------------------
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool (bool)
import           Data.Foldable (traverse_)
import           Data.Function ((&))
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- hashable-time -------------------------------------------------------------
import           Data.Hashable.Time ()


-- generic-lens --------------------------------------------------------------
import           Data.Generics.Labels ()


-- lens ----------------------------------------------------------------------
import           Control.Lens.Setter ((.~))


-- mockchain -----------------------------------------------------------------
import           Mockchain.Transaction
                     ( Transaction (Transaction), TXID, assign
                     )


-- stm -----------------------------------------------------------------------
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue
                     ( TQueue, newTQueueIO, tryReadTQueue, writeTQueue
                     )


-- time ----------------------------------------------------------------------
import           Data.Time.Clock
                     ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime
                     )


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H


------------------------------------------------------------------------------
-- | The configuration for a 'Server'.
data Config = Config
    { timeout :: !NominalDiffTime
      -- ^ The number of seconds older than which we automatically reject
      -- transactions
    , threshold :: !Double
      -- ^ The minimum fee\/amount ratio that we want, below which we
      -- automatically reject transactions
    }
  deriving
    ( Eq, Ord, {-Read,-} Show, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
instance Monoid Config where
    mempty = Config 300 0.1


------------------------------------------------------------------------------
-- This is a bit of a funny 'Semigroup' instance, although it is valid.
-- Basically we want a 'Monoid' instance to represent the default values,
-- which means we need to supply a 'Semigroup' instance as well. The '<>'
-- operator for this instance prefers values that are different from defaults
-- given by 'mempty', favouring the left hand side if the difference is the
-- same. This satisfies the 'Monoid' laws.
instance Semigroup Config where
    Config timeout threshold <> Config timeout' threshold' =
        Config timeout'' threshold''
      where
        Config dtimeout dthreshold = mempty
        timeout'' = maxdiff dtimeout timeout timeout'
        threshold'' = maxdiff dthreshold threshold threshold'
        maxdiff default_ a b = bool a b $ delta a <= delta b
          where
            delta = abs . (default_ -)


------------------------------------------------------------------------------
-- | A 'Transaction' is 'Pending' until it is 'Accepted' or 'Rejected'.
data Status = Pending | Accepted | Rejected
  deriving
    ( Eq, Ord, Read, Show, Enum, Bounded, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON Status where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
instance Semigroup Status where
    (<>) = max


------------------------------------------------------------------------------
instance Monoid Status where
    mempty = minBound


------------------------------------------------------------------------------
-- | Implements the logic for acceptance of 'Transaction's
accepting :: ()
    => Config
    -- ^ The configurable parameters for acceptance of transactions
    -> Transaction
    -- ^ The transaction
    -> UTCTime
    -- ^ The time at which the transaction was broadcast
    -> UTCTime
    -- ^ The current time
    -> Status
    -- ^ Whether we would accept or reject the transaction
accepting (Config timeout treshold) (Transaction _ _ amount fee) time now =
    bool Rejected Accepted $ duration <= timeout && ratio >= treshold
  where
    duration = diffUTCTime now time
    ratio = realToFrac $ fee / amount


------------------------------------------------------------------------------
-- | A 'Result' contains everything we know about a 'Transaction' and is
-- returned by 'get'.
data Result = Result
    { txid :: !TXID
    , transaction :: !Transaction
    , time :: !UTCTime
    , status :: !Status
    }
  deriving
    ( Eq, Ord, Read, Show, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON Result where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
-- | A 'Server' contains a store of all known 'Transaction's and their
-- 'Status'es, and a queue of pending to-be-confirmed transactions which get
-- processed in 10-second intervals.
data Server = Server !(IORef (HashMap TXID Result)) !(TQueue TXID)


------------------------------------------------------------------------------
-- | Creates a new 'Server' with no known transactions. This forks a thread
-- that processes pending transactions from the queue.
new :: MonadIO m => Config -> m Server
new config = liftIO $ do
    state <- newIORef mempty
    queue <- newTQueueIO
    _ <- forkIO $ thread state queue
    pure $ Server state queue
  where
    -- we could just process everything straight away (and block using
    -- readTQueue), but instead we poll with an explicit delay in order to
    -- simulate the time between blocks
    thread state queue = forever $ loop *> threadDelay 10000000
      where
        -- process as much as we can from the queue without blocking
        loop = atomically (tryReadTQueue queue) >>= traverse_ go
          where
            go txid = getCurrentTime >>= modify state . subgo >> loop
              where
                subgo now = H.adjust subsubgo txid
                  where
                    subsubgo result@(Result _ transaction time _) = result'
                      where
                        result' = result & #status .~ status'
                        status' = accepting config transaction time now


------------------------------------------------------------------------------
-- | Adds a 'Transaction' to the @'Server'@\'s queue and returns a 'TXID' that
-- can be used to query the 'Status' of the 'Transaction' with 'get'.
broadcast :: MonadIO m => Transaction -> Server -> m TXID
broadcast transaction (Server state queue) = liftIO $ do
    now <- getCurrentTime
    let txid = assign transaction now
    let result = Result txid transaction now mempty
    modify state $ H.insert txid result
    atomically $ writeTQueue queue txid
    pure txid


------------------------------------------------------------------------------
-- | Returns the 'Result' (which contains the 'Transaction' and its 'Status')
-- associated with the given 'TXID' if it is known to the given 'Server'.
get :: MonadIO m => TXID -> Server -> m (Maybe Result)
get txid (Server state _) = liftIO $ H.lookup txid <$> readIORef state


------------------------------------------------------------------------------
modify :: IORef a -> (a -> a) -> IO ()
modify ref f = atomicModifyIORef' ref $ flip (,) () . f
