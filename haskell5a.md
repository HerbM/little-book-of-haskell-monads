## 5A. A Monad on top of a Monad

So far we have made a newtype an instance of the Monad typeclass.
But a newtype can accept parameters, and one of those parameters could itself
be a newtype. In this chapter, we'll show that 
- a newtype can accept a newtype as a parameter
- a newtype can accept a monad as a parameter
- a newtype that is a monad, can accept a newtype that is a monad as a parameter

----

```haskell
-- [1]: declare a newtype called ReducerT 
-- [2]: our newtype accepts a monad, m as a parameter
-- [3]: the state's pure value, (a,s) is put *inside* m    
--      --[1]--- -[2]-                                 -[3]- 
newtype ReducerT s m a = ReducerT { getReducerT :: (s -> m (a,s)) }
```

The order of `s m a` chosen in the `newtype` definition is important. 
This is because we want polymorphism over `a`. 
Recall how `Reducer s a` kept the type of `s` fixed but allowed the type of `a` to vary.
In other words, the type of the **Monadic Action results** value `a` can *change*, but the type of 
the **Accumulation value** value `s`, must remain *fixed*.
This gives us flexibility over what the type of a result is. 
When we are using various reducers, they can output different types.

```haskell
-- for a given lambda-bind involving Reducer...
--    1. the type of accumulation value s is fixed to [Int]
--    2. the type of a can change
main = do
    let reducers = Reducer (\x -> ("hi", x))            -- a :: String,     s :: [Int]
          >>= (\a -> (Reducer (\x -> (200, x))))        -- a :: Int,        s :: [Int]
          >>= (\a -> (Reducer (\x -> ([Just 'c'], x)))) -- a :: Maybe Char, s :: [Int]
    let initialState = [1,2,3,4,5]
    print $ getReducer reducers initialState
```

*Output*
```
([Just 'c'],[1,2,3,4,5])
```

In the same fashion, we would like `ReducerT` to also have polymorphism over `a`.
Recall that `Monad` can only work with one type.
When registering a newtype as a Monad, we must partially apply `(n-1)` of the `n` types.
If we want polymorphism over `a`, then `a` must be the **last type involved** 
so that is *isn't applied* when registered.
As a result when we register `StateT`, the partial application used is `(StateT s m)`.

```haskell
-- [1]: we can only pass one type to Monad
-- [2]: we apply s and m so that they are fixed
--      a is not applied as so is 'free', giving us polymorphism over a
--                           ----[1]-----
--                                   -[2]-
instance (Monad m) => Monad (ReducerT s m) where 
```
    
> **Key Point**: In a lambda-bind involving `ReducerT`, the **type of the accumulation value**
`s` and the **type of the inner monad** `m` **will remain fixed** throughout all the reducers.
This is because `a` is the **last type involved** in the type parameters of `ReducerT`: `ReducerT s m a`.

Let's look at an example of `ReducerT`.

----

**Example**    

```haskell
-- [1]: this do block belongs to the inner monad, m, not the StateT monad
-- [2]: execute monad function f inside inner monad do block
-- [3]: StateT expects values of m (a, s). use return to promote (a,s) to m (a,s)
liftToReducer :: (Monad m) => m a -> ReducerT s m a
liftToReducer x = ReducerT $ \s -> do
  innera <- x
  return (innera,s)

main = do
    let reducers = (ReducerT (\x -> return ("hi", x)))   [1]
          >>= (\_ -> (ReducerT (\x -> return (200, x))))
          >>= (\_ -> (liftToReducer . putStrLn) "hello")  [2]
          >>= (\_ -> (liftToReducer . putStrLn) "io")
          >>= (\_ -> (liftToReducer . putStrLn) "monad")
          >>= (\_ -> (ReducerT (\x -> return (["xxx"], x))))
    let initialState = [1,2,3,4,5]
    getReducer reducers initialState
```

*Output*    
```
hello
io
monad
(["xxx"],[1,2,3,4,5])
```

----

Now that we have an inner monad, there are two important differences.

- We now need to wrap any `Reducer` oldvalues inside the monad.    
  Hence we must use `return` around `(a,s)`. We cannot omit `return` otherwise
  If we didn't have `return` it implies we don't have a nested monad 
  and the types wouldn't match.

    ```haskell
    --- [1]: We need to wrap the Reducer oldvalue inside the IO monad
    ---      Adding a `return` will wrap it in an IO context.
    ---                   --[1]--
         (ReducerT (\x -> return ("hi", x))) 
    ```
- Conversely, can use the IO monad but we must wrap it around a `Reducer`.

    ```haskell
    --- [1]: We need to wrap the IO newvalue inside the Reducer monad
    ---      The most conventional way to do this is to create a custom
    ---      lifting function for ReducerT
    ---             -------------[1]------------------
                    -----------[2]-----------
         >>= (\_ -> (liftToReducer . putStrLn) "hello")
    ```

So every element in the chain always has  *both* monads.
Sometimes we care just about the Reducer monad, so we use `return` to ignore
the IO monad. Sometimes we care just about the IO monad, so we use `liftToReducer` to
ignore the Reducer monad.

> **Key Point:** Working with two monads is much easier when we use `ReducerT s m a`
compared to `Reducer`.    
This is because
- when we need to use just `ReducerT`, we just need to add `return`. 
- when we need to use just `m`, we use just need to add function like `liftToReducer` to 'lift' it.


We end up with one beautiful *single* lambda-bind chain / a single do-block (no nesting)
that is made up of short one-liner reducers. It's simple to read, yet has the power of two monads!
We can easily use the results from the inner monad to effect the execution of the outer monad.
Let's have a look at how we can use an inner `IO` monad to have an effect on `ReducerT`.

----

**Example**    

```haskell
main = do
    let reducers = ((liftToReducer . putStrLn) "Please give me a number:")
            >>= (\_ -> (liftToReducer getLine)
            >>= (\input -> (ReducerT (\x -> return (x, x+read input)))
            >>= (\_ -> (ReducerT (\x -> return (x, x+1000)))
            >>= (\_ -> ((liftToReducer . putStrLn) "I just added 1000! You're welcome.")
            >>= (\_ -> (ReducerT (\x -> return (x, x))))))))
    let initialState = 0
    getReducer reducers initialState
```

*Output*
```
Please give me a number:
20
I just added 1000! You're welcome.
(1020,1020)
```

----


The implementation of `ReducerT` is:


``` haskell
newtype ReducerT s m a = ReducerT { getReducer :: s -> m (a,s) }
instance (Monad m) => Monad (ReducerT s m) where
  return a           = ReducerT $ \s -> return (a,s)
  (ReducerT x) >>= f = ReducerT $ \s -> do
      (v,s') <- x s
      getReducer (f v) s'
```




----

Now let's rewrite some of our programs involving `StateT` 
so that, rather than using a nested lambda-bind expression, 
they are now using the **do notation**.

**Example**    

Rewrite the following to use lambdas and binds.

```haskell
main = do
    let reducers = ((liftToReducer . putStrLn) "Please give me a number:")
            >>= (\_ -> (liftToReducer getLine)
            >>= (\input -> (ReducerT (\x -> return (x, x+read input)))
            >>= (\_ -> (ReducerT (\x -> return (x, x+1000)))
            >>= (\_ -> ((liftToReducer . putStrLn) "I just added 1000! You're welcome.")
            >>= (\_ -> (ReducerT (\x -> return (x, x))))))))
    let initialState = 0
    getReducer reducers initialState
```

*Solution*    

```haskell
main = do
    let addInput input = ReducerT (\x -> return (x, x+read input))
    let addThousand = ReducerT (\x -> return ((), x+1000))
    let output = ReducerT (\x -> return (x, x))
    let reducers = do
        (liftToReducer . putStrLn) "Please give me a number:"
        input <- (liftToReducer getLine)
        addInput input
        addThousand
        (liftToReducer . putStrLn) "I just added 1000! You're welcome."
        output
    let initialState = 0
    getReducer reducers initialState
```
