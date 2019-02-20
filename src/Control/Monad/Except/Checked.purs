-- | ## Extensible checked exceptions with polymorphic variants
-- |
-- | This module provides helpers for using `Variant` with `ExceptT`. When
-- | combined, we get a mechanism for extensible, checked exceptions. That
-- | is, exceptions can be defined and used anywhere, and handled as needed.
-- | Handling an exception eliminates it from the type, giving us proof
-- | that it no longer occurs.
module Control.Monad.Except.Checked
  ( ExceptV
  , handleError
  , safe
  , throw
  , recordToVariant
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (class VariantMatchCases, Variant, case_, expand, inj, onMatch)
import Prim.Row as R
import Prim.RowList as RL
import Record (get)
import Type.Row (class RowToList, class Union)

type ExceptV exc = ExceptT (Variant exc)

-- | Catches and eliminates exceptions given a record of handlers.
-- | Unhandled exceptions are re-propragated. Record fields map
-- | to the label for the exception being handled.
-- |
-- | An example for handling HTTP exceptions might be:
-- | ```purescript
-- | request # handleError
-- |   { httpNotFound: \_ -> ...
-- |   , httpServerError: \error -> ...
-- |   }
-- | ```
handleError
  ∷ ∀ m handlers excHandled excIn excOut rl a
  . RowToList handlers rl
  ⇒ VariantMatchCases rl excHandled (ExceptV excOut m a)
  ⇒ Union excHandled excOut excIn
  ⇒ Monad m
  ⇒ { | handlers }
  → ExceptV excIn m a
  → ExceptV excOut m a
handleError cases = unwrap >>> lift >=> either (onMatch cases throwError) pure

-- | Safely removes the `ExceptT` layer when all exceptions have been handled.
safe
  ∷ ∀ m a
  . Functor m
  ⇒ ExceptV () m a
  → m a
safe = unwrap >>> map (either case_ identity)

-- | Throws an exception into an `ExceptV`. Mostly for syntax sugar.
-- |
-- | ```purescript
-- | throw { httpNotFound: unit }
-- | ```
throw :: forall a smallE bigE _1 m sym typ.
  MonadThrow (Variant bigE) m =>
  Union smallE _1 bigE =>
  IsSymbol sym =>
  R.Cons sym typ () smallE =>
  RowToList smallE (RL.Cons sym typ RL.Nil) =>
  Record smallE ->
  m a
throw v = throwError $ expand $ recordToVariant v

-- | Allows for syntax sugar. A single-element `Record` will be transformed into
-- | a `Variant`.
-- |
-- | ```purescript
-- | recordToVariant { foo: "bar" } == inj (SProxy :: SProxy "foo") "bar"
-- | ```
recordToVariant :: forall r sym typ.
  IsSymbol sym =>
  R.Cons sym typ () r =>
  RowToList r (RL.Cons sym typ RL.Nil) =>
  Record r ->
  Variant r
recordToVariant record =
  inj (SProxy :: SProxy sym) $ get (SProxy :: SProxy sym) record
