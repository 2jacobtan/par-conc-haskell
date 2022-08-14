{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.Printf (printf)
import Control.Concurrent.STM (TVar, readTVarIO, modifyTVar', newTVarIO, atomically)
import Data.Functor ((<&>))
import Data.Data ( Typeable )
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Control.Monad (forever, forM, forM_, when)
import qualified Data.Vector as Vector
import Worker (worker, Key, Value, worker__static, DBMessage (SetMsg, GetMsg), __remoteTable)
import Data.Vector (Vector)
import Data.Char (ord)

type Database = ProcessId

type DataStore = Map String String

dbServer :: [NodeId] -> Process ()
dbServer nodes = do
  say "dbServer starts\n"

  workers <- fmap Vector.fromList $ forM nodes $ \nid -> do
    say $ printf "dbServer: spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'worker)

  when (null nodes) $ liftIO $ ioError (userError "no workers")

  forever $ expect @DBMessage >>= \msg -> do
    let pidDelegate = assignDB workers msg
    case msg of
      GetMsg _ _ -> liftIO (print $ "GetMsg received:" ++ show msg)
      _ -> return ()
    -- forM_ workers $ \pid -> do
    --   -- liftIO $ print ("dbServer: sent to " ++ show nid ++ ": " ++ show msg)
    --   send pid msg
    send pidDelegate msg

assignDB :: Vector Database -> DBMessage -> Database
assignDB ps = \case
  GetMsg _ k -> go k
  SetMsg _ k _ -> go k
  where go k = (Vector.!) ps (mod (ord $ head k) (Vector.length ps))

createDB :: [NodeId] -> Process Database
createDB nodes = do
  -- let node = head nodes
  say $ printf "createDB: nodes: %s" (show nodes)
  -- spawn node $(mkStaticClosure 'dbServer)

  spawnLocal $ dbServer nodes


set :: Database -> Key -> Value -> Process ()
set db k v = do
  myPid <- getSelfPid
  send db $ SetMsg myPid k v

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  myPid <- getSelfPid
  send db $ GetMsg myPid k
  expect  @(Maybe Value)

rcdata :: RemoteTable -> RemoteTable
rcdata = Worker.__remoteTable
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
