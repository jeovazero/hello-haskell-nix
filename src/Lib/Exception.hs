module Lib.Exception (AppException(..), AppExceptionType(..), headOrThrow, appCatch) where

import Control.Exception
    ( Exception
    , Handler(..)
    , SomeException
    , catches
    , throwIO
    )
import Database.PostgreSQL.Typed (PGError)
import qualified Database.PostgreSQL.Typed.ErrCodes as PGErrorCode
import Database.PostgreSQL.Typed.Protocol (pgErrorCode)

newtype AppException = AppException {
    unAppException :: AppExceptionType
  } deriving (Show, Eq)

data AppExceptionType
    = Unexpected String
    | DatabaseError String
    deriving (Show, Eq)

instance Exception AppException

headOrThrow :: Exception e => [a] -> e -> IO a
headOrThrow (x:_) _      = pure x
headOrThrow [] exception = throwIO exception

fallbackHandler :: SomeException -> IO AppException
fallbackHandler err = pure . AppException . Unexpected $ show err

pgErrorHandler :: PGError -> IO AppException
pgErrorHandler err = pure . AppException $ decodePgError err

appCatch :: IO a -> IO (Either AppException a)
appCatch effect = do
    catches
      (effect >>= pure . Right)
      (fmap (fmap Left) [Handler pgErrorHandler, Handler fallbackHandler])

decodePgError :: PGError -> AppExceptionType
decodePgError pgError =
    case pgErrorCode pgError of
      unique_violation -> DatabaseError "Duplicado"
      _                -> DatabaseError (show pgError)
