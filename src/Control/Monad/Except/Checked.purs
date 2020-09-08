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
  , handleErrors
  , safe
  ) where

import Prelude

import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Variant (class VariantMatchCases, Variant, case_, match, onMatch)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

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

-- | Similar to handleError, except it handles all errors. Has the benefit that
-- | you can execute error handlers in the parent monad `m`.
handleErrors
  ∷ ∀ m handlers excHandled excIn rl a
  . RowToList handlers rl
  ⇒ VariantMatchCases rl excHandled (m a)
  ⇒ Union excHandled () excIn
  ⇒ Monad m
  ⇒ { | handlers }
  → ExceptV excIn m a
  → m a
handleErrors cases = unwrap >=> either (match cases) pure
