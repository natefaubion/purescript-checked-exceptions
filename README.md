# purescript-checked-exceptions

Extensible checked exceptions using polymorphic variants

[![Latest release](http://img.shields.io/github/release/natefaubion/purescript-checked-exceptions.svg)](https://github.com/natefaubion/purescript-checked-exceptions/releases)
[![Build status](https://travis-ci.org/natefaubion/purescript-checked-exceptions.svg?branch=master)](https://travis-ci.org/natefaubion/purescript-checked-exceptions)

## The ~~Expression~~ Exception Problem

Given some function for making HTTP requests which propagates an `HttpError`
for failures cases:

```purescript
get
  ∷ ∀ m
  . MonadHttp m
  ⇒ String
  → ExceptT HttpError m String
```

And another for writing files which propagates an `FsError` for failures cases:

```purescript
write
  ∷ ∀ m
  . MonadFs m
  ⇒ Path
  → String
  → ExceptT FsError m Unit
```

What happens when we combine them?

```purescript
getPureScript
  ∷ ∀ m
  . MonadHttp m
  ⇒ MonadFs m
  ⇒ ExceptT _ m Unit
getPureScript =
  get "http://purescript.org" >>= write "~/purescript.html"
```
```
Could not match type

  FsError

with type

  HttpError
```

Before we can get anywhere, we must unify the error types.

```purescript
getPureScript
  ∷ ∀ m
  . MonadHttp m
  ⇒ MonadFs m
  ⇒ ExceptT (Either HttpError FsError) m Unit
getPureScript = do
  resp <- withExceptT Left (get "http://purescript.org")
  rethrow Right (write "~/purescript.html" resp)
```

This gets very tedious, very quickly, because every new exception type we
introduce breaks code we've already written.

## Polymorphic Variants to the Rescue

[`Variant`](https://github.com/natefaubion/purescript-variant) lets us define
_structural_ sum types. Row types in a `Record` point to _fields_, while row
types in a `Variant` point to _tags_. That means we only have to care about
the cases we want to use, and they work together regardless of which module
defined them.

We'll start with a little bit of sugar (this helps the types go down easy):

```purescript
type RowApply (f :: # Type -> # Type) (a :: # Type) = f a

infixr 0 type RowApply as +
```

We'll define our `HttpError` variants with _rows_ instead of the usual `data`
declaration:

```purescript
type HttpServerError r = (httpServerError ∷ String | r)
type HttpNotFound    r = (httpNotFound ∷ Unit | r)
type HttpOther       r = (httpOther ∷ { status ∷ Int, body ∷ String } | r)
```

And add constructors which lift them into `Variant`:

```purescript
httpServerError ∷ ∀ r. String → Variant (HttpServerError + r)
httpServerError = inj (SProxy ∷ SProxy "httpServerError")

httpNotFound ∷ ∀ r. Variant (HttpNotFound + r)
httpNotFound = inj (SProxy ∷ SProxy "httpNotFound") unit

httpOther ∷ ∀ r. Int → String → Variant (HttpOther + r)
httpOther status body = inj (SProxy ∷ SProxy "httpOther") { status, body }
```

We can then define a helpful alias for all of our HTTP exceptions:

```purescript
type HttpError r =
  ( HttpServerError
  + HttpNotFound
  + HttpOther
  + r
  )
```

Now in another module we might do the same for FS exceptions:

```purescript
type FsPermissionDenied r = (fsPermissionDenied ∷ Unit | r)
type FsFileNotFound r     = (fsFileNotFound ∷ Path | r)

fsPermissionDenied ∷ ∀ r. Variant (FsPermissionDenied + r)
fsPermissionDenied = inj (SProxy ∷ SProxy "fsPermissionDenied") unit

fsFileNotFound ∷ ∀ r. Path → Variant (FsFileNotFound + r)
fsFileNotFound = inj (SProxy ∷ SProxy "fsFileNotFound")

type FsError r =
  ( FsPermissionDenied
  + FsFileNotFound
  + r
  )
```

Let's go back to our original example, but instead of `ExceptT` we will
substitute `ExceptV`:

```purescript
type ExceptV exc = ExceptT (Variant exc)
```

```purescript
get
  ∷ ∀ r m
  . MonadHttp m
  ⇒ String
  → ExceptV (HttpError + r) m String

write
  ∷ ∀ r m
  . MonadFs m
  ⇒ Path
  → String
  → ExceptV (FsError + r) m Unit
```

When we go to combine them, _it just works_:

```purescript
getPureScript
  ∷ ∀ r m
  . MonadHttp m
  ⇒ MonadFs m
  ⇒ ExceptV (HttpError + FsError + r) m Unit
getPureScript =
  get "http://purescript.org" >>= write "~/purescript.html"
```

Additionally, these types are completely inferrable:

```
Wildcard type definition has the inferred type

  ( httpServerError :: String
  , httpNotFound :: Unit
  , httpOther :: { status :: Int
                 , body :: String
                 }
  , fsFileNotFound :: String
  , fsPermissionDenied :: Unit
  | t0
  )
```

## Handling Errors

This library exports the `handleError` function. Given a record of exception
handlers, it will catch and route the corresponding exceptions, eliminating
them from the type.

```purescript
getPureScript # handleError
  { httpServerError: \error -> log $ "Server error:" <> error
  , httpNotFound: \_ -> log "Not found"
  }
```

```
Wildcard type definition has the inferred type

  ( fsFileNotFound :: String
  , fsPermissionDenied :: Unit
  , httpOther :: { status :: Int
                 , body :: String
                 }
  | t0
  )
```

This lets us prove that _all_ exceptions have been handled, which means
we can safely remove the `ExceptV` wrapper using the `safe` combinator.

```purescript
getPureScriptSafe
  :: forall m
   . MonadHttp m
  => MonadFs m
  => MonadLog m
  -> m Unit
getPureScriptSafe =
  safe $ getPureScript # handleError
    { httpServerError: ...
    , httpNotFound: ...
    , httpOther: ...
    , fsFileNotFound: ...
    , fsPermissionDenied ...
    }
```
