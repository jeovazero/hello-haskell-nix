
module Lib.Persistence.User where

import Data.UUID (UUID)
import Data.Text (Text)
import Lib.Persistence
import Lib.Data.User (NewUser,User)

class UserPersistence c where
  getUserByEmail :: PersistenceConfig c -> Text -> IO (Maybe User)
  addUser :: PersistenceConfig c -> NewUser -> IO UUID
