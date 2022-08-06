{-# LANGUAGE OverloadedStrings #-}

import Control.Distributed.Process
import Control.Monad
import Control.Monad.IO.Class
import System.IO

import DistribUtils

import Database (Database, createDB, get, rcdata, set)

main = distribMain master rcdata

master :: [NodeId] -> Process ()
master peers = do
  liftIO $ print "aoeu"

  db <- createDB peers

  liftIO $ print "~20~"

  f <- liftIO $ readFile "./distrib-db/Database.hs"
  let ws = words f

  liftIO $ print "~25~"

  zipWithM_ (set db) ws (tail ws)

  liftIO $ print "~29~"

  -- get db "module" >>= liftIO . print
  -- get db "xxxx"   >>= liftIO . print
  liftIO . print =<< get db "module"
  liftIO . print =<< get db "xxxx"

  liftIO $ print "snth"

  forever $ do
    l <- liftIO $ do putStr "key: "; hFlush stdout; getLine
    when (not (null l)) $ do
      r <- get db l
      liftIO $ putStrLn ("response: " ++ show r)

  return ()
