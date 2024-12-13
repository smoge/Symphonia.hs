{-# LANGUAGE
     BangPatterns
   , DataKinds
   , KindSignatures
   , LambdaCase
   #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.SCServer.State (
     BufferId(..)
   , NodeId(..)
   , SyncId(..)

   , SCServerState(..)

   , ConnProtocol(..)

   , setServerClientId

   , setServerMaxBufferIds

   , numberOfSyncIdsToDrop

   , makeEmptySCServerState

   -- We might not need to export these (or the global equivalents) at all:
   , getNextAvailable'
   , getNextAvailables'
   ) where

import Vivid.OSC (OSC)
import Vivid.SC.Server.Types
import Vivid.SynthDef.Types

import Network.Socket (Socket)

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM -- (readTVar, atomically, writeTVar, newTVar, TVar, TMVar)
import Control.Monad (when)
import Data.Bits
import Data.Int (Int32)
-- import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Prelude

data SCServerState
   = SCServerState
   -- We use 'IORef Maybe's instead of MVars so we can use weak pointer
   --   finalizers with older versions of GHC:
  { _scServerState_socketConnectStarted :: TVar Bool
  , _scServerState_socket :: !(TMVar Socket) -- !(TVar (Maybe Socket))
  , _scServerState_listener :: !(TMVar ThreadId) -- !(TVar (Maybe ThreadId))

  , _scServerState_availableBufferIds :: !(TVar [BufferId])
  , _scServerState_maxBufIds :: !(TVar Int32)
  , _scServerState_availableNodeIds :: !(TVar [NodeId])
  , _scServerState_availableSyncIds :: !(TVar [SyncId])
  , _scServerState_syncIdMailboxes :: !(TVar (Map SyncId (MVar ())))
  , _scServerState_serverMessageFunction :: !(TVar (OSC -> IO ()))
  , _scServerState_definedSDs :: !(TVar (Set (SDName, Int))) -- Int is the hash
  , _scServerState_connProtocol :: TVar ConnProtocol -- This doesn't change after boot, but we could e.g. disconnect and reconnect after boot
  }

data ConnProtocol
   = ConnProtocol_UDP
   | ConnProtocol_TCP
 deriving (Show, Read, Eq, Ord)

setServerClientId :: SCServerState -> Int32 -> IO ()
setServerClientId serverState clientId = do
   when (clientId < 0 || clientId > 31) $
      error "client id must be betw 0 and 31"
   atomically $ writeTVar (_scServerState_availableNodeIds serverState) $
      -- The client id is the first 5 bits of a positive int:
      -- Note the incrementing gets weird once we hit the (.&.) -- should
      -- fix if anyone plans to use more than 33 million nodes
      map f [1000..]
 where
   f :: Int32 -> NodeId
   f nodeNum = NodeId $
         ((clientId `shiftL` ((finiteBitSize nodeNum-5)-1)) .|.) $
            ((maxBound `shiftR` 5) .&. nodeNum)

numberOfSyncIdsToDrop :: Int
numberOfSyncIdsToDrop = 10000

makeEmptySCServerState :: IO SCServerState
-- We don't do this with 'atomically' because you can't put 'atomically' in
--   'unsafePerformIO' (or, apparently you can with the "!_ =" hack I was
--   doing, but let's do the recommended way):
makeEmptySCServerState = do -- atomically $ do
   sockConnectStarted <- newTVarIO False
   sockIORef <- newEmptyTMVarIO -- newTVar Nothing -- newIORef Nothing
   listenerIORef <- newEmptyTMVarIO -- newTVar Nothing -- newIORef Nothing

   availBufIds <- newTVarIO $ drop 512 $ map BufferId [0..]
   -- these'll be allocated when we connect (and get a clientId):
   availNodeIds <- newTVarIO $ map (NodeId . ((1 `shiftL` 26) .|.)) [1000..]
   maxBufIds <- newTVarIO 1024
   syncIds <- newTVarIO $ drop numberOfSyncIdsToDrop $ map SyncId [0..]
   syncMailboxes <- newTVarIO $ Map.empty
   serverMessageFunction <- newTVarIO $ \_ -> return ()
   definedSDs <- newTVarIO $ Set.empty
   connProtocolVar <- newTVarIO ConnProtocol_UDP

   return $ SCServerState
          { _scServerState_socketConnectStarted = sockConnectStarted
          , _scServerState_socket = sockIORef
          , _scServerState_listener = listenerIORef
          , _scServerState_availableBufferIds = availBufIds
          , _scServerState_maxBufIds = maxBufIds
          , _scServerState_availableNodeIds = availNodeIds
          , _scServerState_availableSyncIds = syncIds
          , _scServerState_syncIdMailboxes = syncMailboxes
          , _scServerState_serverMessageFunction = serverMessageFunction
          , _scServerState_definedSDs = definedSDs
          , _scServerState_connProtocol = connProtocolVar
          }

-- | If you've started the SC server with a non-default number of buffer ids,
--   (e.g. with the \"-b\" argument), you can reflect that here
-- 
--   Note that the buffer ids start at 512, to not clash with any that
--   another client (e.g. sclang) has allocated
setServerMaxBufferIds :: SCServerState -> Int32 -> IO ()
setServerMaxBufferIds serverState newMax =
   atomically $
      writeTVar (_scServerState_maxBufIds serverState) newMax

getNextAvailable' :: SCServerState -> (SCServerState -> TVar [a]) -> IO a
getNextAvailable' serverState getter =
   getNextAvailables' serverState 1 getter >>= \case
      [x] -> return x
      _ -> error "i don't even - 938"

getNextAvailables' :: SCServerState -> Int -> (SCServerState -> TVar [a]) -> IO [a]
getNextAvailables' serverState numToGet getter = do
   let !_ = serverState
   atomically $ do
      let avail = getter serverState
      (ns, rest) <- splitAt numToGet <$> readTVar avail
      writeTVar avail rest
      return ns

