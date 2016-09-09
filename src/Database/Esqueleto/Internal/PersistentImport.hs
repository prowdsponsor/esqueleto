{-# LANGUAGE CPP#-}
-- | Re-export "Database.Persist.Sql" without any clashes with
-- @esqueleto@.
module Database.Esqueleto.Internal.PersistentImport
  ( module Database.Persist.Sql
  ) where

import Database.Persist.Sql hiding
  ( BackendSpecificFilter, Filter(..), SelectOpt(..)
  , Update(..), delete, deleteWhereCount, updateWhereCount, selectList
  , selectKeysList, deleteCascadeWhere, (=.), (+=.), (-=.), (*=.), (/=.)
  , (==.), (!=.), (<.), (>.), (<=.), (>=.), (<-.), (/<-.), (||.)
  , listToJSON, mapToJSON, getPersistMap, limitOffsetOrder, selectSource
  , update
#if MIN_VERSION_persistent(2,5,0)
  , PersistQueryRead(..)
#else
  , PersistQuery(..)
#endif
  )
