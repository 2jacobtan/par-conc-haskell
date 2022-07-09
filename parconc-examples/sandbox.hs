{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

newtype Talk = Talk Int
  -- deriving Show via Int

-- deriving instance Show Talk
deriving via Int instance Show Talk

main = do
  print $ Talk 5