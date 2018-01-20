## 3E. Appending I

### Following the Chain
Let's take our work from 4A to write a simpler monad that always appends values to its internal state.
This makes how we manipulate our internal state simpler. 
First, instead of having any arbitrary value as the initial state, let's say for simplicity the initial state is
the empty string, `""`. Also for each reducer, instead of changing the internal state to be *any* next value,
instead the reducer must now append a value each time. Our "reducers" will always take the 
and choose new value to append to the current state.

First we'll definite our Monad. Let's call it `Appender`.

```haskell
newtype Appender w r = Appender { unwrap :: (r, w) }  
```


```haskell
instance Monad (Appender w) where  
    return x = ...
    (Appender (r, w)) >>= f = ...
```

We'll later give the definitions of `>>=` and `return`. But first
let's look at an example of a lambda-bind chain using our `Appender` monad.

```haskell
main = do
  let initialLog = ""
  --                └────────────────────────────────────────────────────────────────────────┐
  let appenders = Appender ((), "hello world ")  --                                          │
        --    ┌──────────────┘          └────────────────────────────────────────────────────│
        --    │                                                                              │
        >>= (\a -> (Appender ((), "this is haskell "))  -- can access a                      │
            --    ┌────────────┘           └─────────────────────────────────────────────────│
            --    │                                                                          │
            >>= (\b -> (Appender (((), "appending to a string ")  -- can access b            │
                --    ┌─────────────┘          └─────────────────────────────────────────────│
                --    │                                                                      │
                >>= (\c -> (Appender (((), "using the Appender monad ")))  -- can access c   │
                )     --      ┌────────┘              └──────────────────────────────────────│
            )         --      │                                                              │
                      --      ☐                                                              ☐
  print $ unwrap appenders
```

*Output*    
```
((), "hello world this is haskell appending to a string using the Appender monad")
```

We saw previously in 4A that each Reducer had are *two* values, `(r,s)`, not just one. For `Reducer` we had:
- **Next State**, `s`. This will *directly* be made available only to the reducer immediately following in the chain.
- **Output / Result**, `r`. This will be made available to all `Reducer`s later in the lambda-bind chain.

Similarly here in 4B, each `Appender` has *two* values, `(r, w)`, not just one. For `Appender` we have:
- **Next Value to Append**, `w`. This will be written to the log. It will not be made available to any subsequent loggers in the chain.
- **Output / Result**, `r`. This will be made available to all `Logger`s later in the lambda-bind chain.

```haskell
-- [1]: Output w as the next value to be logged. No other logger can access this value.
-- [2]: Also give out an output / result value r. All subsequent reducers can access this value.

--      ┌──────────  [2]   
Logger (r, w)
--         └──────  [1]  
```






Again, we can put any arbitary value for `r` and `w` in your logger. And again, we have an output / result value, `r` 
that is available for all the reducers that come later in the chain to use. 
This idea of "scoping" a value `r` for later use should remind you of two things:
- the scoping use for  `<-` in do-notation. Each time we use `a <- ..`, the `a` is available to all later lines.
- the output / result `r` for `Reducer` where the result of one reducer `r` is available to all later reducers
Notice how in all three cases (`<-`, `Reducer`, `Appender`), we are using a nesting  of lambdas and binds
to implement this "scoping rule".

> **Key Point**: There is a key difference between the next log `w` and the output / result value `r`.
This difference is about *scope*. `r` 
is accessible in *all subsequent reducers*, 
whereas `w`, is only not accessible by any other reducer.



**Example**    

```haskell
main = do
    let writers = writer (1, ["Foo"]) >>= (\a -> writer (2, ["Bar"]))
    print $ runWriter writers
```

*Output*    
```haskell
(2,["Foo","Bar"])
```


----

### Using the do-notation


Let's try using the do notation with the Writer monad. 
So far we have been using binds and lambdas but we know from 3A that 
we can alternatively use the do notation.

**Example**     
Rewrite the following so that the line marked with (*) using a do block.

```haskell
main = do
  let w1 = writer(1, ["Foo"])
  let w2 = writer(50, ["Bar"])
  let w3 = writer(75, ["Baz"])
  let writers = w1 >>= (\a -> (w2 >>= (\b -> w3))) -- (*)
  print $ runWriter writers
```

*Solution*    
```haskell
main = do
  let w1 = writer(1, ["Foo"])
  let w2 = writer(50, ["Bar"])
  let w3 = writer(75, ["Baz"])
  let writers = do
      a <- w1
      b <- w2
      w3
  print $ runWriter writers
```

----


### Definition of >>=

Let's work out what the definition of bind should be. Firstly our `Appender`
is a monad.

```haskell
instance Monad (Appender s) where
...
```

For it to be an instance of `Monad`, we are required by the `typeclass` for `Monad`
to give definitions for `return` and `>>=` (which we can compare to to implementing an interface in OOP).


```haskell
instance Monad (Appender s) where
    return x = ...
    (Appender x) >>= y = ...
```

So far so good, but now for the hard part, working out the definitions of `return` and `>>=`.
Let's start with `>>=`. The first thing we know is that it'll be a `Appender`.

```haskell
(Appender x) >>= y    =   Appender ...
```

The first question here is, *what is `y`*? It you look back, the pattern
has always been `(Appender x) >>= (\a -> (...))` So here, `y` must be of the form  `(\a -> (...))`.
Next question: what is inside the `...`?. Well again, let's look back. We see it's a complex 
nested bind structure of all the following appenders -- the following explains why:
```haskell
(Appender x) >>= (\a -> ( 
    ...                                -- the '...' contains ...
))
=    
(Appender x) >>= (\a -> (
    (Appender x1) >>= (\a1 -> (        -- ┐ 
        ...                            -- |> ... the next appender ...
    ))                                 -- ┘
))
=
(Appender x) >>= (\a -> (
    (Appender x1) >>= (\a1 -> (        
        (Appender x2) >>= (\a2 -> (    -- ┐ 
            ...                        -- |> ... which in turn contains the next appender ...
        ))                             -- ┘
    ))
))

--- ... and so on ...

```

Or as a diagram it's:

``` 
            ┌─────────────────┌───────────────────┌─────────────────────────── ... ─────────────────────┐
      [(Appender x)]    [(Appender x1)]    [(Appender x2)]                                              |
a:          └────> x_result ---|------------------|--------------------------- ... -------------------  │
a1:                            └──>  x1_result ---|------------------|-------- ... -------------------  |
a2:                                               └──>  x2_result ---|-------- ... -------------------- |
...
```

But something clever is about to happen. What we're going to do is assume all the subsequent appenders can be 
"merged together" *into one single appender*. This single appender performs the work of all of them.
The bind operator is what performs the "merging together".
What we're doing here is assuming for a moment that our definition of bind works and 
that it'll combine them all the subsequent appenders into one apppender.

The upshot of all of this is that `y` its some Lambda that wraps a *single reducer*, `y = (\a -> (Reducer l)`
and that one reducer, `(Reducer l)` can perform work equivalent all the work done by all the appenders
that follow `Appender x` in our chain in one go. 

``` 
                   ┌──────────────────────────────────────────────────┌──────────────────────────────────────┐
      s ────[(Appender x)]                                       [(Appender l)] ────>   h_value              │
a:                 └─────────────────────> x_result ----------------------└──────────>  l_result ─┐          │
                                                                                                  │          │

```


We want to pass the value of `x_result` to the lambda wrapping  `y = (\a -> (Appender l))`.
This will ensure that the output / result value of our first reducer is available to all the subsequent
reducers "inside" `Reducer h`. So we should perform `y x_result`. This gives us `(Reducer h)`.

We can use pattern-matching to extract the result and value tuples from `x` and `h`.
```
                                    ┌──────────────────────────────────────┌──────────────────────────────────┐
      s ────[(Appender (x_result, x_value)]       [(Appender (l_value, l_result)] ───>   h_value              │
a:                          └────────────> x_result -----------------------└──────────>  l_result ─┐          │
                                                                                                   │          │
                                                                                      result: (h_result, x_value ++ h_value)
```


We need to now create a new appender that can do the same work as both `Appender x` and `Appender h` combined.
This would append the value of `x_value` and then append the `h_value`, but in one go.
That is it would append `x_value ++ h_value`. It also needs to take the `h_result`.
So overall we need to return `Appender (h_result, h_state)`.

```haskell 
                                       --   ┌─────────[2]─────────────┐    ┌─── [1] ───┐
(Appender (x_result, x_value)) >>= f = let (Appender (l_result, l_value)) = f x_result
                                       in Appender (l_result, x_value ++ l_value)   
                                       -- └────────────── [3] ─────────────────┘
                                                                
-- [1]: We first realise that f =  (\a -> ...) where ... is all the subsequent reducers.
--      So we pass x_result to pull them out of the lambda. At the same time, we bind a to x_result
--      and this'll ensure x_result is accessible ("in scope") to all these later appenders.
-- [2]: We make an assumption. If our bind operator works correctly then the subsequent appenders are, 
--      by induction, equivalent to one single appender (`Appender l`). Furthermore this one appender that does 
--      the work of all the appenders in one go.
-- [3]: Finally, we return a new Appender with that can do the same work as both 
--      `Appender x` and `Appender h` combined. The first appender's work is appending x_value.
--      The subsequent appender's work it so append l_value. So to do this all in one step, we
--      must append x_value ++ l_value. We'll also need to take the output value of the last appender in the chain.
--      This is l_result. So overall we return Appender (l_result, x_value ++ l_value).
```
----

Let's go back to the newtype definition.

```haskell
newtype Appender w r = Appender { unwrap :: (r, w) }  
```

Note that the order doesn't matter. I can use either `(r, w)` or `(w, r)`, both give legit monads.
I picked `(r, w)` because it makes the diagrams slightly easier to draw (the lambda parameters appear on the left,
the w . See here for a diagram that uses `(w, r)` instead, you'll see it's slightly clunkier.
Also putting `r` on the left is nice because lambda parameters appear on the left in the chain.

> **Key Point**: The order `(r, w)`, `(w, r)` *doesn't matter*. Both give a monad.

However the order in which `w` and `r` appear on the left hand-side of `=` *does matter*.
That is to say, there are two choices:
```
newtype Appender w r = ...
newtype Appender r w = ...
```
... and here, it's important that I choose `w` as the first type. 
Why? Well we want `Appender w r` to keep the type of `w` *fixed* throughout the chain of appenders,
but we'll allow the output value of each appender in the chain, `r`, to *vary*. 
It's fine for one appender to output a string and for another to output an integer. For example,

```haskell
main = do
  let appenders = do
      r1 <- Appender ("string", "append 1 ")  -- appending a string
      r2 <- Appender (10, "append 2 ")        -- appending a integer
      r3 <- Appender ((), "append 3 ")        -- ...
      r4 <- Appender (True, "append 4 ")
      Appender (27.8, "append 5")
  print $ unwrap appenders
```

**Output**
```
(27.8, "append 1 append 2 append 3 append 4 append 5")
```
Here the first appender outputs a string result then the second outputs an integer result.
Here, our appenders are outputting results of strings, integers, floats, booleans, ... all sorts of types!
But notice that throughout our log that we're appending to `w`, is always a string.
As a result, we want *polymorphism over r*, the result type, but we want to keep `w` *fixed*.
In the example, throughout the whole chain, the Appender is only appending *strings*.

The rule in Haskell when defining newtypes is that you only have polymorphism over one variable.
```haskell
newtype MyNewType t1 t2 t3 t4 t5
-- fixed = t1, t2, t3, t4
-- polymorphic = t5
```

So when we defined `Appender`, we had to order `w` and `r` 
such a way so that `w` is fixed and `r` is polymorphic, and that way is
by putting w first and then r second in the newtype declaration:

```haskell
newtype MyNewType w r
-- fixed = w
-- polymorphic = r
```


> **Key Point** Because we wrote `newtype Appender w r` The output / result values of each 
appender, `w`, can change, however the type of what we are appending, `w`
(and what we're appending to) must remain *fixed*.

----

### Tricks and Tips


We can create a monadic action that doesn't make use of the output values and just writes to the log.
Let's call this function `say`.

```haskell
say :: String -> Writer [String] ()
say x = writer ((), [x]) -- output a don't care, () as the output value

main = do
  let writers = do
      say "Foo"
      say "Bar"
      say "Baz"
  print $ runWriter writers
```

----


```haskell
half :: Int -> Writer String Int
half x = do
  tell ("I just halved " ++ (show x) ++ "!")
  return (x `div` 2)

main = do
  let writers = do
      a <- half 8
      b <- half 10
      return (a*b)
  print $ runWriter writers
```

*Output*     
```
(20,"I just halved 8!I just halved 10!")
```
