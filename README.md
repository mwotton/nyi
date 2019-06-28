# nyi

When working, we often want to leave something for later while we implement other
parts. Usually `error` or `undefined` are used for these parts, but these have
problems: if we don't have warnings on, they might sail through unnoticed. If
we do, on the other hand, we might not be able to build images that require
a clean build.

Instead, this module exposes a function for collecting all the usages of `nyi`
so a test can verify that there are none left. This allows both exploratory
hacking and validates that nothing slips through the test suite.

## Example

```haskell
{-# LANGUAGE TemplateHaskell}
import Debug.NYI(allNotImplemented)
import A

main = hspec $ spec $
  -- some useful tests of A functionality, then:
  describe "no nyis left" $ do
    $allNotImplemented `shouldBe` []
```

```haskell
{-# LANGUAGE TemplateHaskell}
module A(foo) where
import Debug.NYI(todo,nyi)

foo = map importantFunction

$(nyi "importantFunction")

$(todo "otherImportantFunction" "can't do this yet for reason X")

```





## current deficiencies

- It would be preferable to be allowed to use inline `nyi`, but I haven't found
  a way to extract a Dec from an Exp and have it be floated up.
- typesigs are not currently generated, so it will fail on `-pedantic` builds.
  this is an important change but not a very difficult one
- we can only detect `nyi` calls in modules that are transitively imported by
  our test suite.
