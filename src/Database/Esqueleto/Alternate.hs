{-# LANGUAGE GADTs
           , EmptyDataDecls
           , ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
 #-}
-- | TODO
module Database.Esqueleto.Alternate
  ( QuerySelect(..)
  , QueryExec(..)
  , QueryInner(FROM, WHERE, ON, ORDER_BY, LIMIT, OFFSET, SET)
  , INNER_JOIN(..)
  , CROSS_JOIN(..)
  , LEFT_OUTER_JOIN(..)
  , RIGHT_OUTER_JOIN(..)
  , FULL_OUTER_JOIN(..)
  , Expr(..)
  , sql
  , sqlSource
  , sqlExec
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.Int (Int64)
import Data.String (IsString)
import Database.Persist.GenericSql
import Database.Persist.Store hiding (delete)
import qualified Data.Conduit as C

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql hiding (Mode(..))


sqlSource :: ( SqlSelect a r
             , MonadLogger m
             , MonadResourceBase m )
          => QuerySelect SqlQuery SqlExpr SqlPersist a
          -> SqlPersist m (C.Source (C.ResourceT (SqlPersist m)) r)
sqlSource (SELECT          inner) = selectSource         (interpretQuery inner)
sqlSource (SELECT_DISTINCT inner) = selectDistinctSource (interpretQuery inner)


sql :: ( SqlSelect a r
       , MonadLogger m
       , MonadResourceBase m )
    => QuerySelect SqlQuery SqlExpr SqlPersist a
    -> SqlPersist m [r]
sql (SELECT          inner) = select         (interpretQuery inner)
sql (SELECT_DISTINCT inner) = selectDistinct (interpretQuery inner)


sqlExec :: ( MonadLogger m
           , MonadResourceBase m )
        => QueryExec val (SqlEntity val) SqlQuery SqlExpr SqlPersist ()
        -> SqlPersist m ()
sqlExec (DELETE inner) = delete (interpretQuery   inner)
sqlExec (UPDATE inner) = update (interpretQuery . inner)


interpretQuery :: Esqueleto query expr backend
               => QueryInner query expr backend a
               -> query a
interpretQuery (FROM f)      = from (interpretQuery . f)
interpretQuery (WHERE e)     = where_ (interpretExpr e)
interpretQuery (ON e)        = on (interpretExpr e)
interpretQuery (ORDER_BY es) = orderBy (map interpretExpr es)
interpretQuery (LIMIT n)     = limit n
interpretQuery (OFFSET n)    = offset n
interpretQuery (SET e ss)    = set (interpretExpr e) (map interpretExpr ss)
interpretQuery (Q q)         = q
interpretQuery (Fmap f q)    = fmap f (interpretQuery q)
interpretQuery (Ap f a)      = interpretQuery f <*> interpretQuery a
interpretQuery (Return x)    = return x
interpretQuery (Bind x f)    = interpretQuery x >>= interpretQuery . f


interpretExpr :: Esqueleto query expr backend
              => Expr query expr backend a
              -> expr a
interpretExpr (ASC v)      = asc (interpretExpr v)
interpretExpr (DESC v)     = desc (interpretExpr v)
interpretExpr (SUB q)      =
  case q of
    SELECT          inner -> sub_select         (interpretQuery inner)
    SELECT_DISTINCT inner -> sub_selectDistinct (interpretQuery inner)
interpretExpr (e :. f)     = (interpretExpr e) ^. f
interpretExpr (e :? f)     = (interpretExpr e) ?. f
interpretExpr (V v)        = val v
interpretExpr (IS_NULL e)  = isNothing (interpretExpr e)
interpretExpr (JUST e)     = just (interpretExpr e)
interpretExpr NULL         = nothing
interpretExpr COUNT_ROWS   = countRows
interpretExpr (NOT e)      = not_ (interpretExpr e)
interpretExpr (a :== b)    = (interpretExpr a) ==. (interpretExpr b)
interpretExpr (a :>= b)    = (interpretExpr a) >=. (interpretExpr b)
interpretExpr (a :> b)     = (interpretExpr a) >.  (interpretExpr b)
interpretExpr (a :<= b)    = (interpretExpr a) <=. (interpretExpr b)
interpretExpr (a :< b)     = (interpretExpr a) <.  (interpretExpr b)
interpretExpr (a :!= b)    = (interpretExpr a) !=. (interpretExpr b)
interpretExpr (a :&& b)    = (interpretExpr a) &&. (interpretExpr b)
interpretExpr (a :|| b)    = (interpretExpr a) ||. (interpretExpr b)
interpretExpr (a :+ b)     = (interpretExpr a) +.  (interpretExpr b)
interpretExpr (a :- b)     = (interpretExpr a) -.  (interpretExpr b)
interpretExpr (a :/ b)     = (interpretExpr a) /.  (interpretExpr b)
interpretExpr (a :* b)     = (interpretExpr a) *.  (interpretExpr b)
interpretExpr (a `LIKE` b) = (interpretExpr a) `like` (interpretExpr b)
interpretExpr (:%)         = (%)
interpretExpr (CONCAT es)  = concat_ (map interpretExpr es)
interpretExpr (a :++ b)    = (interpretExpr a) ++. (interpretExpr b)
interpretExpr (a := b)     = a =.  (interpretExpr b)
interpretExpr (a :+= b)    = a +=. (interpretExpr b)
interpretExpr (a :-= b)    = a -=. (interpretExpr b)
interpretExpr (a :*= b)    = a *=. (interpretExpr b)
interpretExpr (a :/= b)    = a /=. (interpretExpr b)
interpretExpr (E e)        = e


----------------------------------------------------------------------


data QuerySelect query expr backend a where
  -- | See 'select' and 'selectDistinct'
  SELECT, SELECT_DISTINCT
    :: QueryInner  query expr backend a
    -> QuerySelect query expr backend a


data QueryExec val constr query expr backend a where
  -- | See 'delete'.
  DELETE
    :: QueryInner           query expr backend a
    -> QueryExec val constr query expr backend a

  -- | See 'update'.
  UPDATE
    :: constr
    => (expr (Entity val) -> QueryInner query expr backend a)
    -> QueryExec val constr query expr backend a


data QueryInner query expr backend a where
  -- | See 'from'.
  FROM :: From query expr backend a
       => (a -> QueryInner query expr backend b)
       -> QueryInner query expr backend b

  -- | See 'where_'.
  WHERE :: Expr query expr backend (Value Bool)
        -> QueryInner query expr backend ()

  -- | See 'on'.
  ON :: Expr query expr backend (Value Bool)
     -> QueryInner query expr backend ()

  -- | See 'orderBy'.
  ORDER_BY :: [Expr query expr backend OrderBy]
           -> QueryInner query expr backend ()

  -- | See 'limit'.
  LIMIT :: Int64 -> QueryInner query expr backend ()

  -- | See 'offset'
  OFFSET :: Int64 -> QueryInner query expr backend ()

  -- | See 'set'.
  SET :: PersistEntity val
      => Expr query expr backend (Entity val)
      -> [Expr query expr backend (Update val)]
      -> QueryInner query expr backend ()

  -- | Embed a @query@ into this 'QueryInner'.
  Q :: query a -> QueryInner query expr backend a

  -- | 'Functor', 'Applicative', 'Monad'.
  Fmap   :: (a -> b)
         -> QueryInner query expr backend a
         -> QueryInner query expr backend b
  Ap     :: QueryInner query expr backend (a -> b)
         -> QueryInner query expr backend a
         -> QueryInner query expr backend b
  Return :: a -> QueryInner query expr backend a
  Bind   :: QueryInner query expr backend a
         -> (a -> QueryInner query expr backend b)
         -> QueryInner query expr backend b

instance Functor (QueryInner query expr backend) where
  fmap = Fmap

instance Applicative (QueryInner query expr backend) where
  pure  = Return
  (<*>) = Ap

instance Monad (QueryInner query expr backend) where
  return = Return
  (>>=)  = Bind


----------------------------------------------------------------------


data Expr query expr backend a where
  -- | See 'asc' and 'desc'
  ASC, DESC :: PersistField a
            => Expr query expr backend (Value a)
            -> Expr query expr backend OrderBy

  -- | See 'sub_select' and 'sub_selectDistinct'.
  SUB :: PersistField a
      => QuerySelect query expr backend (expr (Value a))
      -> Expr        query expr backend (Value a)

  -- | See @('^.')@.
  (:.) :: (PersistEntity val, PersistField typ)
       => Expr query expr backend (Entity val)
       -> EntityField val typ
       -> Expr query expr backend (Value typ)

  -- | See @('?.')@.
  (:?) :: (PersistEntity val, PersistField typ)
       => Expr query expr backend (Maybe (Entity val))
       -> EntityField val typ
       -> Expr query expr backend (Value (Maybe typ))

  -- | See 'val'.
  V :: PersistField typ => typ -> Expr query expr backend (Value typ)

  -- | See 'isNothing'.
  IS_NULL :: PersistField typ
          => Expr query expr backend (Value (Maybe typ))
          -> Expr query expr backend (Value Bool)

  -- | See 'just'.
  JUST :: Expr query expr backend (Value typ)
       -> Expr query expr backend (Value (Maybe typ))

  -- | See 'nothing'.
  NULL :: Expr query expr backend (Value (Maybe typ))

  -- | See 'countRows'.
  COUNT_ROWS :: Num a => Expr query expr backend (Value a)

  -- | See 'not_'.
  NOT :: Expr query expr backend (Value Bool)
      -> Expr query expr backend (Value Bool)

  (:==), (:>=), (:>), (:<=), (:<), (:!=)
    :: PersistField typ
    => Expr query expr backend (Value typ)
    -> Expr query expr backend (Value typ)
    -> Expr query expr backend (Value Bool)

  (:&&), (:||)
    :: Expr query expr backend (Value Bool)
    -> Expr query expr backend (Value Bool)
    -> Expr query expr backend (Value Bool)

  (:+), (:-), (:/), (:*)
    :: PersistField a
    => Expr query expr backend (Value a)
    -> Expr query expr backend (Value a)
    -> Expr query expr backend (Value a)

  -- | See 'LIKE'.
  LIKE :: (PersistField s, IsString s)
       => Expr query expr backend (Value s)
       -> Expr query expr backend (Value s)
       -> Expr query expr backend (Value Bool)

  -- | See @('%')@.
  (:%) :: (PersistField s, IsString s) => Expr query expr backend (Value s)

  -- | See 'concat_'.
  CONCAT :: (PersistField s, IsString s)
         => [Expr query expr backend (Value s)]
         -> Expr query expr backend (Value s)

  -- | See @('++.')@.
  (:++) :: (PersistField s, IsString s)
        => Expr query expr backend (Value s)
        -> Expr query expr backend (Value s)
        -> Expr query expr backend (Value s)

  (:=)  :: (PersistEntity val, PersistField typ)
        => EntityField val typ
        -> Expr query expr backend (Value typ)
        -> Expr query expr backend (Update val)
  (:+=), (:-=), (:*=), (:/=)
    :: (PersistEntity val, PersistField a)
    => EntityField val a
    -> Expr query expr backend (Value a)
    -> Expr query expr backend (Update val)

  -- | Embed an @expr@ into this 'Expr'.
  E :: expr a -> Expr query expr backend a


----------------------------------------------------------------------


-- | Data type that represents an @INNER JOIN@ (see 'INNER_JOIN').
data INNER_JOIN a b = a `INNER_JOIN` b

-- | Data type that represents a @CROSS JOIN@ (see 'CROSS_JOIN').
data CROSS_JOIN a b = a `CROSS_JOIN` b

-- | Data type that represents a @LEFT OUTER JOIN@ (see 'LEFT_OUTER_JOIN').
data LEFT_OUTER_JOIN a b = a `LEFT_OUTER_JOIN` b

-- | Data type that represents a @RIGHT OUTER JOIN@ (see 'RIGHT_OUTER_JOIN').
data RIGHT_OUTER_JOIN a b = a `RIGHT_OUTER_JOIN` b

-- | Data type that represents a @FULL OUTER JOIN@ (see 'RIGHT_OUTER_JOIN').
data FULL_OUTER_JOIN a b = a `FULL_OUTER_JOIN` b


instance IsJoinKind INNER_JOIN where
  smartJoin a b = a `INNER_JOIN` b
  reifyJoinKind _ = InnerJoinKind
instance IsJoinKind CROSS_JOIN where
  smartJoin a b = a `CROSS_JOIN` b
  reifyJoinKind _ = CrossJoinKind
instance IsJoinKind LEFT_OUTER_JOIN where
  smartJoin a b = a `LEFT_OUTER_JOIN` b
  reifyJoinKind _ = LeftOuterJoinKind
instance IsJoinKind RIGHT_OUTER_JOIN where
  smartJoin a b = a `RIGHT_OUTER_JOIN` b
  reifyJoinKind _ = RightOuterJoinKind
instance IsJoinKind FULL_OUTER_JOIN where
  smartJoin a b = a `FULL_OUTER_JOIN` b
  reifyJoinKind _ = FullOuterJoinKind


instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (INNER_JOIN a b)
         ) => From query expr backend (INNER_JOIN a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (CROSS_JOIN a b)
         ) => From query expr backend (CROSS_JOIN a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (LEFT_OUTER_JOIN a b)
         ) => From query expr backend (LEFT_OUTER_JOIN a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (RIGHT_OUTER_JOIN a b)
         ) => From query expr backend (RIGHT_OUTER_JOIN a b) where
  from_ = fromPreprocess >>= fromFinish

instance ( Esqueleto query expr backend
         , FromPreprocess query expr backend (FULL_OUTER_JOIN a b)
         ) => From query expr backend (FULL_OUTER_JOIN a b) where
  from_ = fromPreprocess >>= fromFinish


----------------------------------------------------------------------


-- Fixity declarations
infixl 9 :., :?
infixl 7 :*, :/
infixl 6 :+, :-
infixr 5 :++
infix  4 :==, :>=, :>, :<=, :<, :!=
infixr 3 :&&, :=, :+=, :-=, :*=, :/=
infixr 2 :||, `LIKE`, `INNER_JOIN`, `CROSS_JOIN`, `LEFT_OUTER_JOIN`, `RIGHT_OUTER_JOIN`, `FULL_OUTER_JOIN`
