{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

newtype MyNum = MyNum Int
  -- deriving Show via Int

-- deriving instance Show MyNum
deriving via Int instance Show MyNum

main :: IO ()
main = do
  print $ MyNum 5
  