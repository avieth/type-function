# type-function

This package gives a construction which essentially promotes the arrow type,
unifying type constructors and promoted data constructors with type families
into one kind: `Function s t` alias `s :-> t`. It requires GHC 8.0, because
it uses GADT promotion and type families to compute kinds.

Since type families are not types (they cannot appear partially applied) they
are proxied by actual datatypes. A simple function like `id` at the type level
is expressed like this:

```Haskell
type Id = F IdProxy
data IdProxy (x :: k) (p :: Proxy k)
type instance EvalFunction (IdProxy x) = x
```

The `IdProxy` takes all of the arguments to the `Id` function, and a `Proxy` to
indicate the final type after evaluation. An analogue of `const` looks like
this:

```Haskell
type Const = F ConstProxy
data ConstProxy (x :: k) (y :: l) (p :: Proxy k)
type instance EvalFunction (ConstProxy x y) = x
```

Notice how the `Proxy` parameter chooses `k` for the output kind. Once the
first parameter of `ConstProxy` is given, the final kind is known.

This construction is capable of giving type-level functor and applicative
instances, allowing the programmer to use awesome functional programming
idioms even when working with types:

```Haskell
*Data.Type.Function> :kind! Const :<$> 'Just 42 :<*> 'Just '()
= 'Just 43

*Data.Type.Function> :kind! Const :<$> 'Just 42 :<*> 'Nothing
= 'Nothing
```

There's some related, but far more complicated and awkward work at
[type-lambda](https://www.github.com/avieth/type-lambda), in which type-level
functions are defined formally, with support for pattern matching.
