{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Worker where

import Control.Distributed.Process (Process, expect, liftIO, send, ProcessId, say)
import Control.Distributed.Process.Closure (remotable)

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forever)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Foldable (traverse_)

type Sender = ProcessId
type Key   = String
type Value = String

data DBMessage = GetMsg Sender Key | SetMsg Sender Key Value
  deriving (Typeable, Generic, Show)
instance Binary DBMessage

worker :: Process ()
worker = do
  say "workr starts\n"
  dbTVar <- liftIO $ newTVarIO Map.empty
  let loop =
        expect @DBMessage >>= \case
          msg@(GetMsg sender key) -> do
            liftIO (print $ "GetMsg received:" ++ show msg)

            returnVal <- liftIO $ readTVarIO dbTVar <&> Map.lookup key
            send sender returnVal
          msg@(SetMsg sender key val) -> do
            -- liftIO (print $ "SetMsg received:" ++ show msg)
            liftIO $ atomically $ modifyTVar' dbTVar (Map.insert key val)
  forever loop

remotable ['worker]