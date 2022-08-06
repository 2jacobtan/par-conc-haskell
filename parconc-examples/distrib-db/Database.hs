{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Data
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Control.Monad (forever)

type Key   = String
type Value = String

type Database = ProcessId

type DataStore = Map String String

type Sender = ProcessId

data DBMessage = GetMsg Sender Key | SetMsg Sender Key Value
  deriving (Typeable, Generic)

instance Binary DBMessage

dbServer :: Process ()
dbServer = do
  say "dbServer starts\n"
  dbTVar <- liftIO $ newTVarIO Map.empty
  let loop =
        expect >>= \case
          GetMsg sender key -> do
            returnVal <- liftIO $ readTVarIO dbTVar <&> Map.lookup key
            send sender returnVal
          SetMsg sender key val ->
            liftIO $ atomically $ modifyTVar' dbTVar (Map.insert key val)
  forever loop

remotable ['dbServer]

createDB :: [NodeId] -> Process Database
createDB _nodes = do
  -- let node = head nodes
  -- say $ printf "createDB: spawning on %s" (show node)
  -- spawn node $(mkStaticClosure 'dbServer)

  spawnLocal dbServer


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
rcdata = id
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
