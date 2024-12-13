-- | This is unscheduled - the server will do what you tell it to
--   as soon as it can. That can mean there'll be slight delays
--   because of time it took to compute what to do or because of
--   network latency. If you want more precise timing look at
--   "Scheduled"
-- 
--   Doing \"VividAction\"s in IO can be like a sketchpad:
--   it's the quickest way to get an idea out.
--   The cool thing is you can take an action that you're sketching
--   and put a function in front of it to get more precise timing
--   E.g. if you have the function:
--
--   @
--   playTone = do
--      synth <- play $ 0.1 ~* sinOsc (freq_ 440)
--      wait 1
--      free synth
--   @
-- 
--   You can play it quickly with just:
-- 
--   > playTone
-- 
--   But if you want precise timing all you need to do is say e.g.:
-- 
--   > playScheduledIn 0.01 playTone

{-# LANGUAGE
     BangPatterns
   , InstanceSigs
   , FlexibleInstances
   , OverloadedStrings
   , TypeSynonymInstances
   , ViewPatterns
   #-}
   -- , Safe

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module Vivid.Actions.IO (
     defineSDFromFileWith
   ) where

import Vivid.Actions.Class
import Vivid.OSC (OSC(..), encodeOSC, Timestamp(..), timestampFromUTC)
import Vivid.SC.Server.Commands as SCCmd
import Vivid.SCServer.State (BufferId(..), NodeId(..), SyncId(..), getNextAvailable', SCServerState(..))
import Vivid.SCServer.Connection ({-getMailboxForSyncId,-} getSCServerSocket', waitForSync_io', sendMaybePad)
import Vivid.SynthDef

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar)
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile)
import Data.Hashable
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import System.Directory (getTemporaryDirectory)

instance VividAction (ReaderT SCServerState IO) where
   callOSC :: OSC -> ReaderT SCServerState IO ()
   callOSC message = callBS (encodeOSC message)

   callBS :: ByteString -> ReaderT SCServerState IO ()
   callBS bs = ReaderT $ \serverState -> do
      sock <- getSCServerSocket' serverState
      connProtocol <- readTVarIO (_scServerState_connProtocol serverState)
      _ <- sendMaybePad connProtocol sock bs
      return ()

   sync :: ReaderT SCServerState IO ()
   sync = do
      wait (0.01 :: Float) -- Just to make sure you don't "sync" before calling
                           --   the command you want to sync (temporary)
      sid <- newSyncId
      callOSC $ SCCmd.sync sid
      waitForSync sid

   waitForSync :: SyncId -> ReaderT SCServerState IO ()
   waitForSync syncId =
      ReaderT $ \serverState ->
         waitForSync_io' serverState syncId

   wait :: Real n => n -> ReaderT SCServerState IO ()
   wait t = ReaderT $ \_ ->
      threadDelay $ round (realToFrac (t * 10^(6::Int)) :: Double)

   getTime :: ReaderT SCServerState IO Timestamp
   getTime = ReaderT $ \_ -> timestampFromUTC <$> getCurrentTime

   newBufferId :: ReaderT SCServerState IO BufferId
   newBufferId = ReaderT $ \serverState -> do
      maxBufIds <- readTVarIO (_scServerState_maxBufIds serverState)
      BufferId nn <- getNextAvailable' serverState _scServerState_availableBufferIds
      -- TODO: this 'mod' may not help anything:
      return . BufferId $ nn `mod` maxBufIds

   newNodeId :: ReaderT SCServerState IO NodeId
   newNodeId = ReaderT $ \serverState ->
      getNextAvailable' serverState _scServerState_availableNodeIds

   newSyncId :: ReaderT SCServerState IO SyncId
   newSyncId = ReaderT $ \serverState ->
      getNextAvailable' serverState _scServerState_availableSyncIds

   fork :: ReaderT SCServerState IO () -> ReaderT SCServerState IO ()
   fork action = ReaderT $ \serverState -> do
      _ <- forkIO (runReaderT action serverState)
      return ()

   defineSD :: SynthDef a -> ReaderT SCServerState IO ()
   defineSD synthDef@(SynthDef name _ _) = ReaderT $ \serverState -> do
      hasBeenDefined <- (((name, hash synthDef) `Set.member`) <$>) $
         readTVarIO (_scServerState_definedSDs serverState)
      -- unless hasBeenDefined $ (`runReaderT` serverState) $ do
      -- unless hasBeenDefined $ (serverState runReaderT ) $ do
      unless hasBeenDefined $ do
         (`runReaderT` serverState) $ oscWSync $ \syncId ->
            callOSC $
               SCCmd.d_recv [sdToLiteral synthDef] (Just $ SCCmd.sync syncId)
         atomically $ modifyTVar (_scServerState_definedSDs serverState) $
            ((name, hash synthDef) `Set.insert`)

-- This could be written as ReaderT SCServerState:
-- | Synchronous
defineSDFromFileWith :: SCServerState -> SynthDef a -> IO ()
defineSDFromFileWith serverState theSD = do
   tempDir <- getTemporaryDirectory
   let fName = tempDir++"/" ++ show (hash theSD) ++ ".scsyndef"
   BS.writeFile fName $ encodeSD theSD
   (`runReaderT` serverState) $ oscWSync $ \syncId ->
      callOSC $ SCCmd.d_load fName (Just $ SCCmd.sync syncId)

