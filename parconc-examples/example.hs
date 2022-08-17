-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment ( getArgs )
import Control.Distributed.Process hiding ( bracket )
import Control.Distributed.Process.Node ( initRemoteTable )
import Control.Distributed.Process.Backend.SimpleLocalnet hiding (startSlave)
-- import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad ( forM_, replicateM, forever )
import Control.Monad.Catch ( bracket )
import Data.Maybe ( catMaybes )
import qualified Control.Distributed.Process.Node as Node

import qualified Data.ByteString.Char8 as BSC (pack)
import Control.Distributed.Process.Internal.Types (ProcessId(ProcessId), LocalProcessId (LocalProcessId))
import Network.Socket (getNameInfo)
import Network.Transport as NT
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)

master :: Backend -> [ NodeId ] -> Process ()
master backend slaves = do
    -- Do something interesting with the slaves
    liftIO . putStrLn $ "Slaves: " ++ show slaves
    -- Terminate the slaves when the master terminates (this is optional)
    terminateAllSlaves backend

main :: IO ()
main = do
    args <- getArgs

    case args of
        [ "master", host, port ] -> do
            backend <- initializeBackend host port initRemoteTable
            startMaster backend (master backend)
        [ "slave", host, port ] -> do
            backend <- initializeBackend host port initRemoteTable
            startSlave backend
        [ "findPeers", host, port ] -> do
            backend <- initializeBackend host port initRemoteTable
            forever $ do
              peers <- findPeers backend 1000000
              print =<< getCurrentTime
              putStrLn $ "Peers: " ++ show peers
              threadDelay 3000000
        [ "findSlaves", host, port ] -> do
            backend <- initializeBackend host port initRemoteTable
            findSlavesIO backend
        _ -> error "eaxmple.hs error: invalid args"

-- | Find slave nodes
findSlavesIO :: Backend -> IO ()
findSlavesIO backend = do
    node <- newLocalNode backend
    Node.runProcess node (findSlavesProc backend)

findSlavesProc :: Backend -> Process ()
findSlavesProc backend = do
    nodes <- liftIO $ findPeers backend 1000000
    --  ^ Fire off asynchronous requests for the slave controller

    forever $ do
      bracket (mapM monitorNode nodes) (mapM unmonitor) $ \_ -> do
          -- fire off whereis requests
          forM_ nodes $ \nid -> whereisRemoteAsync nid "slaveController"

          -- Wait for the replies
          whereIsReplies <- catMaybes <$> replicateM (length nodes)
              (receiveWait
              [ --  match (return . Just @WhereIsReply)
              --    match (\(WhereIsReply "slaveController" mPid) -> return mPid)
              --  , match (\NodeMonitorNotification {} -> return Nothing)
                match (\(WhereIsReply label mPid) -> return (Just (label, mPid)))
              , match (\nmn@NodeMonitorNotification {} -> return (Just (show nmn, Nothing)))
              ])

          liftIO $ print =<< getCurrentTime
          liftIO $ putStrLn $ "WhereIsReplies: " ++ show whereIsReplies
      liftIO $ threadDelay 3000000

    -- say . (("ProcessInfo: " ++) . show) =<< getProcessInfo (ProcessId (NodeId (NT.EndPointAddress (BSC.pack "localhost:9090:0"))) (LocalProcessId 196825021 8))


-- debugging library functions

-- | Calling 'slave' sets up a new local node and then waits. You start
-- processes on the slave by calling 'spawn' from other nodes.
--
-- This function does not return. The only way to exit the slave is to CTRL-C
-- the process or call terminateSlave from another node.
startSlave :: Backend -> IO ()
startSlave backend = do
  node <- newLocalNode backend
  Node.runProcess node slaveController

-- | The slave controller interprets 'SlaveControllerMsg's
slaveController :: Process ()
slaveController = do
    pid@(ProcessId nodeID localProcessId) <- getSelfPid
    register "slaveController" pid
    say $ "pid: " ++ show pid
    say $ "ProcessID: " ++ show nodeID ++ " | " ++ show localProcessId
    forever $ (say . show =<< getProcessInfo pid) >> liftIO (threadDelay 3000000)
    -- go
  where
    go = do
      msg :: Int <- expect
      -- case msg of
      --   SlaveTerminate -> return ()
      --   RedirectLogsTo loggerPid from -> do
      --     r <- try (reregister "logger" loggerPid)
      --     ok <- case (r :: Either ProcessRegistrationException ()) of
      --             Right _ -> return True
      --             Left _  -> do
      --               s <- try (register "logger" loggerPid)
      --               case (s :: Either ProcessRegistrationException ()) of
      --                 Right _ -> return True
      --                 Left _  -> return False
      --     pid <- getSelfPid
      --     send from (RedirectLogsReply pid ok)
      --     go
      go