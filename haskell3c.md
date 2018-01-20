## 3C. Writing definitions for `=<<`




```haskell
-- Step 1: The newtype 
--         what is the name of our monad?
--         what type are we wrapping?
--         what is the name of the access fn?
newtype Foo = Foo { getFoo :: …}

-- Step 2: Definition of return and bind
--         actually say our wrapper type is a monad
--         and provide the definitions of return and mind
instance Monad Foo where

    -- definition of return
    return x = … 

    -- definition of bind
    (Foo … ) >>= f = … 
```

> **Key Point** To write a definition for `=<<` there are *two* steps.
The first step is creating the newtype. The second step is declaring the newtype as a type instance
of `Monad`. By making this declaration, we are required to provide implementations for
return and bind.



### The `Boring` Monad

```haskell
newtype Boring a = Boring { unwrap :: a }

instance Monad Boring where
  return x = Boring x
  (Boring av) >>= f =
    let Boring bv = f av
    in Boring bv

main = do
  let borings = do
      r1 <- Boring 2
      r2 <- Boring "x"
      Boring True
  print $ unwrap borings
```

### The `Backtrack` Monad

```haskell

newtype Foo a = Foo { unwrap :: (a, String) }

instance Monad Foo where
  return x = Foo (x, "")
  (Foo (av, x)) >>= f =
    let Foo (bv, u) = f av
    in Foo (bv, u ++ " backtrack " ++ x)
```

### The `Counter` Monad
This monad flips increments an integer we backtrack.



### The `EvenOdd` Monad

This monad flips between even and odd as we backtrack.


### Recurse and Backtrack


Most definitions of `=<<` use a technique I call (for a lack of a better name),
**Recurse-and-Backtrack**. There are two parts of the definition: the *recurse* part
and the *backtrack* part – which as you may have guessed, reflect heavily the nature
of recursion.


For a `x <- rhs` on line i. That means we have `(\x ->   ) =<< rhs`.
The function on the left here is the nested lambdas and binds for lines i + 1, i + 2, ..., n.
For simplicity we'll call this function `f`.

The corresponding `=<<` for `x <- rhs` will do the following:

- **Recurse**
    - call `f` with `x`, this binds `x` to the nested lambdas and binds for lines i + 1 onwards
    - `f x` evaluates to a monadic value of type `M b`
    - this will be a boxed value
- **Backtrack**
    - use pattern matching on this boxed value to extract the corresponding unboxed value (`let ... = f x`)
    - modify this unboxed value and then box it up again
    - and then use this new boxed value as the overal result of the bind
    - this value will be passed to the defintion of bind
    - this value will arrive in the backtracking step of the bind on line i - 1


This recursve-and-backtrack technique is a little counter-intuitive.
A given `=<<` does not know what happened on the previous lines, 
but does know what happened on later lines.


```haskell
newtype Foo a = Foo { unwrap :: (a, String) }

instance Monad Foo where
  return x = Foo (x, "")
  (Foo (av, x)) >>= f =
    let Foo (bv, u) = f av
    in Foo (bv, u ++ " backtrack " ++ x)

main = do
  let foos = do
      r1 <- Foo (2, "y")
      r2 <- Foo (1, "x")
      Foo (0, "base")
  print $ unwrap foos



------------------------

main = do
  let foos = do
      r1 <- Foo (2, "y")
      r2 <- Foo (1, "x") -- x = "x"
      Foo (0, "base")    -- u = "base"
  print $ unwrap foos

main = do
  let foos = do
      r1 <- Foo (2, "y")            -- v = "y"
      Foo (0, "base bracktrack x")  -- u = "base backtrack x"
  print $ unwrap foos

main = do
  let foos = do
      Foo (0, "base bracktrack x backtrack y"
  print $ unwrap foos
```

In general,

```haskell
do
  x1 <- m1
  x2 <- m2
  Foo

do
  x1 <- m1
  (\x2 -> Foo) =<< m2

do
  (\x1 -> (\x2 -> Foo) =<< m2) =<< m1 
```

The definition of bind is taking the overall result of the later lines,
taking <rhs> and produces a new foo. This new foo will be pass to the bind
of the previous line to work with. The foo on the previous line
will work with this new foo and the rhs on the previous line.

```haskell
instance Monad Foo where
  return x = Foo (x, ...)
  (Foo (av, x)) >>= f =
    let Foo (bv, u) = f av
    in Foo (bv, ...)


do
  -- ...
  _ <- Foo (av, x)
  Foo (bv, u)
```

### Do's and Don'ts of Bind Defintions



**Don't**:
- Try to inspect the pure computation
- Avoid using `x` in `return`. It means your Using the same types throughout is a no-no. 
- Add any restrictions on `a`. For example, don't try to make `a` instance of `Show`.



**Example**
Explain why we can't write a monad that counts how many times we have had an even number

*Solution*
- Monads work across all types, we can't guarantee it'll be used with an int. 
We're also not allowed to check if we are currently being used with an int (or any particular type for that matter).
- Monads can't check the values of the do-block computation.
