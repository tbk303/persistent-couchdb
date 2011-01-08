{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Persist.CouchDB where

import Database.Persist
import Database.Persist.Base
import Database.CouchDB as C

import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Data.Function (on)

import Debug.Trace

import Text.JSON as J

import Control.Applicative (Applicative)

import "mtl" Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import "MonadCatchIO-transformers" Control.Monad.CatchIO

import qualified Control.Monad.IO.Class as Trans

_PERSIST_TIME_OF_DAY_ = "PersistTimeOfDay"
_PERSIST_UTC_TIME_ = "PersistUTCTime"
_PERSIST_DAY_ = "PersistDay"
_PERSIST_BYTE_STRING_ = "PersistByteString"
_PERSIST_INT_ = "PersistInt"
_PERSIST_DOUBLE_ = "PersistDouble"

newtype CouchDBReader m a = CouchDBReader (ReaderT (C.CouchConn, C.DB) m a)
    deriving (Monad, Trans.MonadIO)

instance Trans.MonadIO m => MonadIO (CouchDBReader m) where
    liftIO = Trans.liftIO

type CouchId = Key Doc

insertFields :: (PersistEntity val) => val -> JSValue
insertFields record = J.makeObj $ zip toLabels toValues
  where
    toLabels = map fst3 $ entityColumns $ entityDef record
    toValues = map (pToJ . toPersistValue) (toPersistFields record)

getFields :: (PersistEntity b) => JSValue -> b
getFields (JSObject o) = case (fromPersistValues fields) of
    Right e -> e
    Left s -> error $ s
  where
    eFields = map (readJSON . snd) $ drop 2 $ fromJSObject o
    fields = map (\(Ok a) -> a) eFields
getFields _ = error "the impossible happened: no JSObject returend"

withCouchDB :: MonadIO m => C.CouchConn -> C.DB -> CouchDBReader m a -> m a
withCouchDB conn db (CouchDBReader r) = do runReaderT r (conn, db)

instance Trans.MonadIO m => PersistBackend (CouchDBReader m) where
    -- -- | Create a new record in the database, returning the newly created
    -- -- identifier.
    -- insert :: PersistEntity val => val -> m (Key val)
    insert val =
        do (conn, db) <- CouchDBReader ask
           docId <- liftIO genDocId
           let docName = doc $ show docId
           liftIO $ runCouchDBWith conn $ newNamedDoc db docName (insertFields val)
           return $ toPersistKey $ docId

    -- -- | Replace the record in the database with the given key. Result is
    -- -- undefined if such a record does not exist.
    -- replace :: PersistEntity val => Key val -> val -> m ()
    replace k v = undefined

    -- -- | Update individual fields on a specific record.
    -- update :: PersistEntity val => Key val -> [Update val] -> m ()
    update k vs = undefined

    -- -- | Update individual fields on any record matching the given criterion.
    -- updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> m ()
    updateWhere filts upds = undefined

    -- -- | Delete a specific record by identifier. Does nothing if record does
    -- -- not exist.
    -- delete :: PersistEntity val => Key val -> m ()
    delete k = undefined

    -- -- | Delete a specific record by unique key. Does nothing if no record
    -- -- matches.
    -- deleteBy :: PersistEntity val => Unique val -> m ()
    deleteBy uval = undefined

    -- -- | Delete all records matching the given criterion.
    -- deleteWhere :: PersistEntity val => [Filter val] -> m ()
    deleteWhere vals = undefined

    -- -- | Get a record by identifier, if available.
    -- get :: PersistEntity val => Key val -> m (Maybe val)
    get k =
      do (conn, db) <- CouchDBReader ask
         let docId = fromPersistKey k
             docName = doc $ show docId
         mRet <- liftIO $ runCouchDBWith conn $ getDoc db docName
         return $ maybe Nothing (\(_,_,x)->Just (getFields x)) mRet

    -- -- | Get a record by unique key, if available. Returns also the identifier.
    -- getBy :: PersistEntity val => Unique val -> m (Maybe (Key val, val))
    getBy uval = undefined

    -- -- | Get all records matching the given criterion in the specified order.
    -- -- Returns also the identifiers.
    -- select :: PersistEntity val
    --        => [Filter val]
    --        -> [Order val]
    --        -> Int -- ^ limit
    --        -> Int -- ^ offset
    --        -> Enumerator (Key val, val) m a
    select filts ords limit offset = undefined

    -- -- | Get the 'Key's of all records matching the given criterion.
    -- selectKeys :: PersistEntity val
    --            => [Filter val]
    --            -> Enumerator (Key val) m a
    selectKeys filts = undefined

    -- -- | The total number of records fulfilling the given criterion.
    -- count :: PersistEntity val => [Filter val] -> m Int
    count filts = undefined

jOtherTypeToP :: JSObject JSValue -> Result PersistValue
jOtherTypeToP o =
  conv $ (head $ mapSnd (fromJSString . (\(JSString s) -> s)) $ fromJSObject o)
  where conv (t, arg)
          | t == _PERSIST_TIME_OF_DAY_ =  Ok $ PersistTimeOfDay $ read arg
          | t == _PERSIST_UTC_TIME_ = Ok $ PersistUTCTime $ read arg
          | t == _PERSIST_DAY_ = Ok $ PersistDay $ read arg
          | t == _PERSIST_BYTE_STRING_ = Ok $ PersistByteString $ read arg
          | t == _PERSIST_INT_ = Ok $ PersistInt64 $ read arg
          | t == _PERSIST_DOUBLE_ = Ok $ PersistDouble $ read arg
          | otherwise = error "BAD"

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f ls = map (\(l,r)->(l, f r)) ls

instance JSON PersistValue where
  -- readJSON  :: JSValue -> Result a
  readJSON (JSString s) = Ok $ PersistString $ fromJSString s
  readJSON (JSBool b) = Ok $ PersistBool b
  readJSON (JSNull) = Ok $ PersistNull
  readJSON (JSRational False i) = Ok $ PersistInt64 $ truncate $ trace ("<"++(show i)++">") i
  readJSON (JSRational True i) = Ok $ PersistDouble $ fromRational $ trace ("["++(show i)++"]") i
  readJSON (JSObject o) = jOtherTypeToP o

  -- showJSON  :: a -> JSValue
  showJSON (PersistString s) = JSString $ toJSString s
  showJSON (PersistBool x) = JSBool x
  showJSON (PersistNull) = JSNull
  showJSON (PersistInt64 i) = makeObj [(_PERSIST_INT_, showJSON $ show i)]
  showJSON (PersistDouble d) = makeObj [(_PERSIST_DOUBLE_, showJSON $ show d)]
  showJSON (PersistTimeOfDay tod) = makeObj [(_PERSIST_TIME_OF_DAY_, showJSON $ show tod)]
  showJSON (PersistUTCTime utc) = makeObj [(_PERSIST_UTC_TIME_, showJSON $ show utc)]
  showJSON (PersistDay day) = makeObj [(_PERSIST_DAY_, showJSON $ show day)]
  showJSON (PersistByteString bs) = makeObj [(_PERSIST_BYTE_STRING_, showJSON $ show bs)]

pToJ :: PersistValue -> J.JSValue
pToJ = showJSON

genDocId :: IO Int64
genDocId =
    do t <- getCurrentTime
       return $ fromIntegral . fromEnum . utcTimeToPOSIXSeconds $ t

fst3 (a, _, _) = a
