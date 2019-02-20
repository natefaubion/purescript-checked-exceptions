module Test.Main where

import Prelude

import Control.Monad.Except.Checked (ExceptV, handleError, safe, throw)
import Data.Identity (Identity)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "checked-exceptions" do
    describe "throw" do
      it "throws and catches errors" do
        let
          request :: ExceptV Errors Identity String
          request = do
              _ <- throw { foo: "foo" }
              pure "bar"
        (request # handleError
            { foo: (\s -> pure s)
            , bar: (\s -> pure "bar")
            }
         # safe) `shouldEqual` (pure "foo")

type Errors = ( foo :: String, bar :: Int )
