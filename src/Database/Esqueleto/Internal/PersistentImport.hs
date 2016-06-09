{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Re-export "Database.Persist.Sql" without any clashes with
-- @esqueleto@.
module Database.Esqueleto.Internal.PersistentImport
  ( module Database.Persist.Sql
#if ! MIN_VERSION_persistent(2,5,0)
    , PersistRecordBackend
#endif
  ) where

import Database.Persist.Sql hiding
  ( BackendSpecificFilter, Filter(..), PersistQuery(..), SelectOpt(..)
  , Update(..), delete, deleteWhereCount, updateWhereCount, selectList
  , selectKeysList, deleteCascadeWhere, (=.), (+=.), (-=.), (*=.), (/=.)
  , (==.), (!=.), (<.), (>.), (<=.), (>=.), (<-.), (/<-.), (||.)
  , listToJSON, mapToJSON, getPersistMap, limitOffsetOrder, selectSource
  , update, count )

#if ! MIN_VERSION_persistent(2,5,0)
type PersistRecordBackend record backend = (PersistEntity record, PersistEntityBackend record ~ backend)
#endif
