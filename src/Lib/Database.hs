{-# LANGUAGE OverloadedStrings #-}

module Lib.Database (
    settings,
    PG.useTPGDatabase,
    PG.pgQuery,
    PG.PGConnection,
    PG.pgTransaction,
    PG.PGError,
    closeConnection,
    openConnection,
    withDatabase,
    withErrorHandler,
    IODB) where

import Control.Exception (bracket)
import qualified Control.Exception as C
import Database.PostgreSQL.Typed as PG
import qualified Network.Socket as Net

type IODB a = IO (Either C.SomeException a)

hDBException :: PGError -> IO ()
hDBException = print

hSomeException :: C.SomeException -> IO ()
hSomeException err = print $ show err ++ " 234"

withErrorHandler :: IO () -> IO ()
withErrorHandler effect = do
    C.catches
        effect
        [ C.Handler hDBException
        , C.Handler hSomeException
        ]

-- TODO: Get the config from ENV
settings :: PGDatabase
settings = defaultPGDatabase {
    PG.pgDBAddr = Right $ Net.SockAddrInet 5444 (Net.tupleToHostAddress (127, 0, 0, 1))
  , PG.pgDBName = "dev"
  , PG.pgDBUser = "dev"
  , PG.pgDBPass = "pass"
}

openConnection :: PGDatabase -> IO PGConnection
openConnection = PG.pgConnect

closeConnection :: PGConnection -> IO ()
closeConnection = PG.pgDisconnect

withDatabase :: PGDatabase -> (PGConnection -> IO c) -> IO c
withDatabase config = bracket (pgConnect config) closeConnection
