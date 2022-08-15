{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forM_, replicateM)
import Data.Maybe (catMaybes)
import qualified Control.Distributed.Process.Node as Node

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend
    ["findPeers", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      peers <- findPeers backend 1000000
      putStrLn $ "Peers: " ++ show peers
    ["findSlaves", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      findSlavesIO backend
    _ -> error "eaxmple.hs error: invalid ags"

-- | Find slave nodes
findSlavesIO :: Backend -> IO ()
findSlavesIO backend = do
  node <- newLocalNode backend
  Node.runProcess node (findSlavesProc backend)
findSlavesProc :: Backend -> Process ()
findSlavesProc backend = do
  nodes <- liftIO $ findPeers backend 1000000
  -- Fire off asynchronous requests for the slave controller

  bracket
   (mapM monitorNode nodes)
   (mapM unmonitor)
   $ \_ -> do

   -- fire off whereis requests
   forM_ nodes $ \nid -> whereisRemoteAsync nid "slaveController"

   -- Wait for the replies
   whereIsReplies <- catMaybes <$> replicateM (length nodes) (
     receiveWait
       [ match (return . Just @WhereIsReply)
       , match (\NodeMonitorNotification {} -> return Nothing)
       ])

   liftIO $ print whereIsReplies