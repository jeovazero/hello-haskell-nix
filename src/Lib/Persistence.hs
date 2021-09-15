{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Lib.Persistence (PersistenceConfig(..)) where

newtype PersistenceConfig a =
  PersistenceConfig {
    pConfig :: a
  }
