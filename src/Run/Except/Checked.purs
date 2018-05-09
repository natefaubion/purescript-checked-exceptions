-- | ## Extensible checked exceptions with polymorphic variants
-- |
-- | This module provides helpers for using `Variant` with `Except`. When
-- | combined, we get a mechanism for extensible, checked exceptions. That
-- | is, exceptions can be defined and used anywhere, and handled as needed.
-- | Handling an exception eliminates it from the type, giving us proof
-- | that it no longer occurs.
module Run.Except.Checked
  ( EXCEPTV
  , handleError
  , safe
  ) where

import Prelude

import Data.Either (either)
import Data.Variant (class VariantMatchCases, Variant, case_, onMatch)
import Run (Run, expand)
import Run.Except (EXCEPT, runExcept, throw)
import Type.Row (class RowToList)

type EXCEPTV exc = EXCEPT (Variant exc)

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
  ∷ ∀ m handlers excHandled excIn excOut rl r a
  . RowToList handlers rl
  ⇒ VariantMatchCases rl excHandled (Run (except :: EXCEPTV excOut | r) a)
  ⇒ Union excHandled excOut excIn
  ⇒ Union r m (except :: EXCEPTV excOut | r)
  ⇒ { | handlers }
  → Run (except :: EXCEPTV excIn | r) a
  → Run (except :: EXCEPTV excOut | r) a
handleError cases = runExcept >>> expand >=> either (onMatch cases throw) pure

-- | Safely removes the `Except` layer when all exceptions have been handled.
safe
  ∷ ∀ r a
  . Run (except :: EXCEPTV () | r) a
  → Run r a
safe = runExcept >>> map (either case_ id)
