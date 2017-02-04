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
import qualified Data.Text as T
import Web.HttpApiData
import Web.PathPieces

import Eventful.Store.Class
import Eventful.Store.Sqlite.Internal
import Eventful.UUID

data SqliteEvent
  = SqliteEvent
  { sqliteEventProjectionId :: !UUID
  , sqliteEventVersion :: !EventVersion
  , sqliteEventData :: !JSONString
  } deriving (Show)

type SqliteEventId = Key SqliteEvent

instance PersistEntity SqliteEvent where
  newtype Key SqliteEvent = SqliteEventKey { unSqliteEventKey :: SequenceNumber }
    deriving (Show, Read, Eq, Ord, PathPiece, ToHttpApiData, FromHttpApiData,
              PersistField, PersistFieldSql, ToJSON, FromJSON)

  data EntityField SqliteEvent typ where
    SqliteEventId :: EntityField SqliteEvent SqliteEventId
    SqliteEventProjectionId :: EntityField SqliteEvent UUID
    SqliteEventVersion :: EntityField SqliteEvent EventVersion
    SqliteEventData :: EntityField SqliteEvent JSONString

  data Unique SqliteEvent = UniqueAggregateVersion UUID EventVersion
  type PersistEntityBackend SqliteEvent = SqlBackend

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
    (FTTypeCon Nothing (T.pack "JSONString"))
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
      (FTTypeCon Nothing (T.pack "JSONString"))
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
          (FTTypeCon Nothing (T.pack "JSONString"))
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
        (FTTypeCon Nothing (T.pack "JSONString"))
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
