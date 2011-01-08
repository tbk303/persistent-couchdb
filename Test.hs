{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Prelude hiding (filter)
import Database.Persist
import Database.Persist.CouchDB
import Database.CouchDB
import Control.Monad.IO.Class
import Database.Persist.Pool
import Database.CouchDB
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

mkPersist [$persist|
Person
    name String update Eq Ne Desc
    age Int update "Asc" Lt "some ignored attribute"
    double Double update
    bool Bool update
    day Day update
    tod TimeOfDay update
    utc UTCTime update

    color String null Eq Ne
    PersonNameKey name
|]

main :: IO ()
main = do conn <- createCouchConn "localhost" 5984 
          withCouchDB conn (db "test") go

go :: CouchDBReader IO ()
go = do
    cTime <- liftIO $ getCurrentTime
    pid <- insert $ Person "Michael" 25 1.5 True (utctDay cTime) midnight cTime Nothing
    p <- get pid
    liftIO $ print pid
    liftIO $ print p
