{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the persistent entity to use with the SQLite event
-- store.

module Eventful.Store.Sqlite.Entity
  ( SqliteEvent (..)
  , SqliteEventId
  , PersistEntity (..)
  , Key (..)
  , Unique (..)
  , EntityField (..)
  , migrateSqliteEvent
  ) where

import Data.Aeson
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Sql
import Data.Proxy
import qualified Data.Text as T
import Web.HttpApiData
import Web.PathPieces

import Eventful.Store.Class
import Eventful.UUID

data SqliteEvent serialized
  = SqliteEvent
  { sqliteEventProjectionId :: !UUID
  , sqliteEventVersion :: !EventVersion
  , sqliteEventData :: !serialized
  } deriving (Show)

type SqliteEventId serialized = Key (SqliteEvent serialized)

instance (PersistField serialized) => PersistEntity (SqliteEvent serialized) where
  newtype Key (SqliteEvent serialized) = SqliteEventKey { unSqliteEventKey :: SequenceNumber }
    deriving (Show, Read, Eq, Ord, PathPiece, ToHttpApiData, FromHttpApiData,
              PersistField, PersistFieldSql, ToJSON, FromJSON)

  data EntityField (SqliteEvent serialized) typ where
    SqliteEventId :: EntityField (SqliteEvent serialized) (SqliteEventId serialized)
    SqliteEventProjectionId :: EntityField (SqliteEvent serialized) UUID
    SqliteEventVersion :: EntityField (SqliteEvent serialized) EventVersion
    SqliteEventData :: EntityField (SqliteEvent serialized) serialized

  data Unique (SqliteEvent serialized) = UniqueAggregateVersion UUID EventVersion
  type PersistEntityBackend (SqliteEvent serialized) = SqlBackend

  keyToValues = (: []) . toPersistValue . unSqliteEventKey
  keyFromValues = fmap SqliteEventKey . fromPersistValue . _persistentHeadNote

  persistUniqueToFieldNames UniqueAggregateVersion{} =
    [ (HaskellName (T.pack "projectionId"), DBName (T.pack "projection_id"))
    , (HaskellName (T.pack "version"), DBName (T.pack "version"))
    ]
  persistUniqueToValues (UniqueAggregateVersion uuid vers) = [toPersistValue uuid, toPersistValue vers]
  persistUniqueKeys (SqliteEvent pid vers _) = [UniqueAggregateVersion pid vers]

  toPersistFields (SqliteEvent pid vers dat)
    = [ SomePersistField pid
      , SomePersistField vers
      , SomePersistField dat
      ]
  fromPersistValues [pid, vers, dat] =
    SqliteEvent <$>
    fromPersistValue pid <*>
    fromPersistValue vers <*>
    fromPersistValue dat
  fromPersistValues other = Left $ "Invalid fromPersistValues input: " <> T.pack (show other)

  persistFieldDef SqliteEventId =
    FieldDef
    (HaskellName (T.pack "id"))
    (DBName (T.pack "sequence_number"))
    (FTTypeCon Nothing (T.pack "SequenceNumber"))
    SqlInt64
    [T.pack "sql=sequence_number", T.pack "sql=id"]
    True
    (ForeignRef
      (HaskellName (T.pack "SqliteEvent"))
      (FTTypeCon Nothing (T.pack "SequenceNumber")))
  persistFieldDef SqliteEventProjectionId =
    FieldDef
    (HaskellName (T.pack "projectionId"))
    (DBName (T.pack "projection_id"))
    (FTTypeCon Nothing (T.pack "UUID"))
    (SqlOther (T.pack "uuid"))
    []
    True
    NoReference
  persistFieldDef SqliteEventVersion =
    FieldDef
    (HaskellName (T.pack "version"))
    (DBName (T.pack "version"))
    (FTTypeCon Nothing (T.pack "EventVersion"))
    SqlInt64
    []
    True
    NoReference
  persistFieldDef SqliteEventData =
    FieldDef
    (HaskellName (T.pack "data"))
    (DBName (T.pack "data"))
    (FTTypeCon Nothing (T.pack "serialized"))
    SqlBlob
    []
    True
    NoReference
  persistIdField = SqliteEventId

  entityDef _ =
    EntityDef
    (HaskellName (T.pack "SqliteEvent"))
    (DBName (T.pack "events"))
    (FieldDef
      (HaskellName (T.pack "id"))
      (DBName (T.pack "sequence_number"))
      (FTTypeCon Nothing (T.pack "SequenceNumber"))
      SqlInt64
      [T.pack "sql=sequence_number", T.pack "sql=id"]
      True
      (ForeignRef
        (HaskellName (T.pack "SqliteEvent"))
        (FTTypeCon Nothing (T.pack "SequenceNumber"))))
    [T.pack "sql=events"]
    [FieldDef
      (HaskellName (T.pack "projectionId"))
      (DBName (T.pack "projection_id"))
      (FTTypeCon Nothing (T.pack "UUID"))
      (SqlOther (T.pack "uuid"))
      []
      True
      NoReference,
      FieldDef
      (HaskellName (T.pack "version"))
      (DBName (T.pack "version"))
      (FTTypeCon Nothing (T.pack "EventVersion"))
      SqlInt64
      []
      True
      NoReference,
      FieldDef
      (HaskellName (T.pack "data"))
      (DBName (T.pack "data"))
      (FTTypeCon Nothing (T.pack "serialized"))
      SqlBlob
      []
      True
      NoReference]
    [UniqueDef
     (HaskellName (T.pack "UniqueAggregateVersion"))
      (DBName (T.pack "unique_aggregate_version"))
      [(HaskellName (T.pack "projectionId"),
         DBName (T.pack "projection_id")),
       (HaskellName (T.pack "version"), DBName (T.pack "version"))]
      []]
    []
    [T.pack "Show"]
    (Map.fromList [])
    False

  fieldLens SqliteEventId = lensPTH entityKey (\(Entity _ val) key -> Entity key val)
  fieldLens SqliteEventProjectionId =
    lensPTH (sqliteEventProjectionId . entityVal) (\ (Entity key val) pid -> Entity key (val {sqliteEventProjectionId = pid}))
  fieldLens SqliteEventVersion =
    lensPTH (sqliteEventVersion . entityVal) (\ (Entity key val) vers -> Entity key (val {sqliteEventVersion = vers}))
  fieldLens SqliteEventData =
    lensPTH (sqliteEventData . entityVal) (\ (Entity key val) dat -> Entity key (val {sqliteEventData = dat}))

migrateSqliteEvent :: Migration
migrateSqliteEvent = do
  let
    defs =
      [ EntityDef
        (HaskellName (T.pack "SqliteEvent"))
        (DBName (T.pack "events"))
        (FieldDef
          (HaskellName (T.pack "id"))
          (DBName (T.pack "sequence_number"))
          (FTTypeCon Nothing (T.pack "SequenceNumber"))
          SqlInt64
          [T.pack "sql=sequence_number", T.pack "sql=id"]
          True
          (ForeignRef
            (HaskellName (T.pack "SqliteEvent"))
            (FTTypeCon Nothing (T.pack "SequenceNumber"))))
        [T.pack "sql=events"]
        [FieldDef
          (HaskellName (T.pack "projectionId"))
          (DBName (T.pack "projection_id"))
          (FTTypeCon Nothing (T.pack "UUID"))
          (SqlOther (T.pack "uuid"))
          []
          True
          NoReference,
          FieldDef
          (HaskellName (T.pack "version"))
          (DBName (T.pack "version"))
          (FTTypeCon Nothing (T.pack "EventVersion"))
          SqlInt64
          []
          True
          NoReference,
          FieldDef
          (HaskellName (T.pack "data"))
          (DBName (T.pack "data"))
          (FTTypeCon Nothing (T.pack "serialized"))
          SqlBlob
          []
          True
          NoReference]
        [UniqueDef
          (HaskellName (T.pack "UniqueAggregateVersion"))
          (DBName (T.pack "unique_aggregate_version"))
          [(HaskellName (T.pack "projectionId"),
             DBName (T.pack "projection_id")),
           (HaskellName (T.pack "version"), DBName (T.pack "version"))]
          []]
        []
        [T.pack "Show"]
        (Map.fromList [])
        False]
  migrate
    defs
    (EntityDef
      (HaskellName (T.pack "SqliteEvent"))
      (DBName (T.pack "events"))
      (FieldDef
       (HaskellName (T.pack "id"))
        (DBName (T.pack "sequence_number"))
        (FTTypeCon Nothing (T.pack "SequenceNumber"))
        SqlInt64
       [T.pack "sql=sequence_number", T.pack "sql=id"]
        True
        (ForeignRef
          (HaskellName (T.pack "SqliteEvent"))
          (FTTypeCon Nothing (T.pack "SequenceNumber"))))
      [T.pack "sql=events"]
      [FieldDef
       (HaskellName (T.pack "projectionId"))
        (DBName (T.pack "projection_id"))
        (FTTypeCon Nothing (T.pack "UUID"))
        (SqlOther (T.pack "uuid"))
        []
        True
       NoReference,
        FieldDef
        (HaskellName (T.pack "version"))
        (DBName (T.pack "version"))
        (FTTypeCon Nothing (T.pack "EventVersion"))
        SqlInt64
        []
        True
        NoReference,
        FieldDef
        (HaskellName (T.pack "data"))
        (DBName (T.pack "data"))
        (FTTypeCon Nothing (T.pack "serialized"))
        SqlBlob
        []
        True
        NoReference]
      [UniqueDef
        (HaskellName (T.pack "UniqueAggregateVersion"))
        (DBName (T.pack "unique_aggregate_version"))
        [(HaskellName (T.pack "projectionId"),
           DBName (T.pack "projection_id")),
          (HaskellName (T.pack "version"), DBName (T.pack "version"))]
        []]
      []
      [T.pack "Show"]
      (Map.fromList [])
      False)


-- Following taken from persistent-template

_persistentHeadNote :: [PersistValue] -> PersistValue
_persistentHeadNote [x] = x
_persistentHeadNote xs = error $ "mkKeyFromValues: expected a list of one element, got: "
  `mappend` show xs

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensPTH :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)


-- Orphan instances

instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PersistField EventVersion where
  toPersistValue = toPersistValue . unEventVersion
  fromPersistValue = fmap EventVersion . fromPersistValue

instance PersistFieldSql EventVersion where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField SequenceNumber where
  toPersistValue = toPersistValue . unSequenceNumber
  fromPersistValue = fmap SequenceNumber . fromPersistValue

instance PersistFieldSql SequenceNumber where
  sqlType _ = sqlType (Proxy :: Proxy Int)
