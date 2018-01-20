## 5C. _Not_ Defaulting to the Innermost via `lift`

We've previous shown that return and bind always manipulate values in the
innermost monad - so how do we change values for the other monads on the
outer layers? Suppose we have a Writer wrapping an Either wrapping a Maybe.
We can use return and bind to change the value in the maybe, but how do 
we change Writer and Either? Perhaps we want to change Either from being
a `Right` to a `Left`. Or maybe we want to append to the Writer's log.

```haskell
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

type Logged              = Writer [String]
type LoggedFallable      = ExceptT String Logged
type LoggedFallableMaybe = MaybeT LoggedFallable

main = do
   let stack = do
       -- initial stack
       s0 <- (return 10 :: LoggedFallableMaybe Int)

       -- change Maybe, Either, Writer
       s1 <- MaybeT(ExceptT(writer(Right(Just(2)), [])))
       -- change Either, Writer
       s2 <- lift $ (ExceptT(writer(Right(s1), [])))
       -- change Writer
       s3 <- (lift . lift) $ (writer(s2, ["Got number"]))

       return s3
   print stack
```

Normal monad commands

> **Key Point**: ordinary monad “commands” talk to the *innermost* monad by default.
To send commands to the other monads we use the `lift`.
A single lift “sends” commands one layer further out.
Multiple lift calls can be chained together to send commands to layers more further out.



We tend to only use lift with *monadic actions*.

When we need to use lift, 
it can be good style to write wrapper functions that do the lifting for us, as above, and to use those. 
The alternative of sprinkling explicit uses of lift throughout our code tends to look messy. 
Worse, it hard-wires the details of the layout of our monad stack into our code, 
which will complicate any subsequent modifications.

----

**Example 1**

In this example, we'll explore using `lift` to control two independent states. 
Typically we use *tuples* if we have two independent states that we want to keep track of.
If the first state `s` is an integer and the second state `t` is a string,
then we can combine these states using a tuple (`s`, `t`).

```haskell
import Control.Monad.State

combined = do
  (s,t) <- get
  modify (\(x,y) -> (x+1, y++"1"))
  (s',t') <- get
  return ((s,s'),(t,t'))

main = do
  print $ evalState combined (0, "0")
```

*Output*
```
((0,1),("0","01"))
```

However it might become annoying working with tuples.
Another option is to use a monad transformer stack.
One layer of a monad stack
holds one state, another layer holds another state.  To do this, we'll take the state monad
and wrap it inside the state monad. We'll start inside with `StateT`, and then wrap this
with `State`.

```haskell
import Control.Monad.Trans.State.Lazy

type Single = State Int
type Both a = StateT a Single

import Control.Monad.Trans -- lift
import Control.Monad.Trans.State.Lazy -- State, StateT, get, modify

type NumState = State Int
type NumStringState a = StateT a NumState

stacked = do
  -- change integer state
  s <- get
  modify (+1)
  -- change string state
  t <- lift get
  lift $ modify (++ "1")
  -- output final states
  s' <- get
  t' <- lift get
  return (s', t')

main = do
  print $ evalState (evalStateT stacked 0) "0"
```
----


**Example 2**

```haskell
-- IO monad already available from prelude
import Control.Monad.Trans -- lift
import Control.Monad.Trans.State.Lazy -- State, StateT

type StdOut = IO
type StateWithStdOut = StateT Int StdOut

twoLayers :: StateWithStdOut ()
twoLayers = do
  -- print initial state
  s0 <- get
  lift $ print $ "initial state: " ++ show s0
  -- modify state,
  -- print next state
  modify (+1)
  s1 <- get
  lift $ print $ "final state: " ++ show s1

main = do
    evalStateT twoLayers 0
```

*Output*:
```
"initial state: 0"
"final state: 1"
```


----

**Example 3**

In this example we have *three* layers.

```
              Innermost (top)
                    ↓
IO <- WriterT <- StateT
```

By defaut we access the innermost layer which in this case 
is the State monad. To access the Writer layer, we must use lift once, to access the IO layer,
we must use lift twice.

```haskell
import Control.Monad.Trans -- lift
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer

type Output = IO
type OutputLogged = WriterT [Int] Output
type OutputLoggedState = StateT Int OutputLogged

threeLayers :: OutputLoggedState Int
threeLayers = do
    modify (+1)
    s0 <- get
    lift $ tell [s0]        -- Writer layer
    --
    modify (+1)
    s1 <- get
    lift $ lift $ print s1  -- IO layer
    --
    return s1               -- State layer

main = do
  execWriterT (evalStateT threeLayers 0)
```

*Output*

```
2
[1]
```


### Other Kinds of Lifts

There are *three* kinds of lifts, 
`fmap` (`base`), `liftM` (`Control.Monad`) and `lift` (`Control.Monad.Trans`).

- We use `fmap` when using so-called functors (see appendix). 

- We use `liftM` to lift a **non-monadic function to a monadic function**.

    ```haskell
    import Control.Monad (liftM)
    main = do
        list <- liftM words getContents
        putStrLn (show list)
    ```
    
    ```haskell
    import Control.Monad (liftM)
    main = do 
        a <- (liftM read)(getLine) :: IO Int
        print a
    ```

- We use `lift` to lift a **monadic function by one level in a transformer stack**.


If you are not using the monad transformer library, then you won't use `lift`.

----

### Exercises

**6.** Complete the following sentences.    
For each sentence, fill in the `...` with one of: `lift`, `liftM` or `fmap`.

**(a)** `...` elevates a pure function to the level of functors;    
**(b)** `...` takes a pure function to the level of monads;    
**(c)** `...` raises a monadic action from one level beneath in the transformer stack to the current one.
