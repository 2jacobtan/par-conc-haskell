{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (MyNum) where

newtype MyNum = MyNum Int
  -- deriving Show via Int

-- deriving instance Show MyNum
deriving via Int instance Show MyNum

main = do
  print $ MyNum 5
  