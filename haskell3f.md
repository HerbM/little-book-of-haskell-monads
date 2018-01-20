## 3F. Handling Errors with Either and Maybe

So far, we have been using the `IO` monad with the `State` and `Writer` monads.
We used an outer do block which was associated to `IO` and an inner block which was 
either `State` or `Writer`.    
Let's practise using `IO` together with these two monads.




----

Sometimes we might want to indicate that something went wrong when using
the `State` monad or the `Writer` monad. That is we want some kind of **error handling**.
Haskell has many techniques for handling errors but the simplest is to use *another monad*.
Here, we'll  two monads that can be used for error handling. 
These two monads are called the **Maybe Monad** (`Maybe`) and the **Either Monad** (`Either`).

----

**Example**    

Let's revisit our stack example. When we perform a pop, we might not have anything on the stack!    

```haskell
main = do -- do block for IO Monad
  let pop = Reducer (\(x:xs) -> (x,xs))
  let push v = Reducer (\xs -> ((),v:xs))
  let set v = Reducer (\_ -> ((),v))
  let reducers = do -- do block for Reducer monad
      push (1::Int)
      push (2::Int)
      push (3::Int)
      a <- pop
      b <- pop
      c <- pop
      d <- pop -- whoops!
      set [a+b+c+d]

  let initialState = []
  print $ getReducer reducers initialState
```

*Output*
```
((),[test.hs: test.hs:2:22-38: Non-exhaustive patterns in lambda
```

Above, when we try to pop to get `d`, we won't have a value and a runtime pattern matching
error will be thrown (non-exhaustive patterns for pop). While we could implementing the case
for popping the empty list, we can instead use the `Maybe` monad. The `Maybe` monad will indicate 
whether or not we have value.


```haskell
import Data.Maybe

main = do
  let pop = Reducer (\vs -> case vs of
                            []     -> (Nothing, [])
                            (x:xs) -> (Just x, xs))
  let push v = Reducer (\xs -> ((),v:xs))
  let set v = Reducer (\_ -> ((),v))
  let reducers = do
      push (1::Int)
      push (2::Int)
      push (3::Int)
      a <- pop
      b <- pop
      c <- pop
      d <- pop -- d is assigned to Nothing
      set $ [fromJust a + fromJust b + fromJust c]

  let initialState = []
  print $ getReducer reducers initialState
```

Here on line `(*)`, we use the fact that Maybe is an *applicative monad*, to use
functions like `<$>`, and `<*>`. We try to add up our value, but if any of them
are `Nothing`, then the overall result is `Nothing`. Indeed, since `d` is `Nothing`,
the output is `Nothing`.


*Output*    
```
((),[6])
```

----

**Example**    

```haskell
import Control.Monad.Writer
import Data.Maybe

list :: [(Int, String)]
list = [(1,"Foo"), (2, "Bar")]

writelookup :: Maybe String -> Writer [String] ()
writelookup (Just x) = writer((), ["Found: " ++ x])
writelookup (Nothing) = writer((), ["Nothing found!"])

main = do
  let m1 = lookup 1 list :: Maybe String
  let m2 = lookup 2 list :: Maybe String
  let m3 = lookup 3 list :: Maybe String
  let w1 = do -- w1 :: Writer [String] ()
      writelookup m1
      writelookup m2
      writelookup m3
  print $ runWriter w1
```

*Output*    
```
((),["Found: Foo","Found: Bar","Nothing found!"])
```

We could rewrite this

```haskell
import Control.Monad.Writer
import Data.Maybe

list :: [(Int, String)]
list = [(1,"Foo"), (2, "Bar")]

writelookup :: Int -> Writer [String] ()
writelookup x = case (lookup x list) of
   (Just x) -> writer((), ["Found: " ++ x])
   (Nothing) -> writer((), ["Nothing found!"])

main = do
  let w1 = do
      writelookup 1
      writelookup 2
      writelookup 3
  print $ runWriter w1
```


```haskell
import Control.Monad.Writer
import Data.Maybe

list :: [(String, String)]
list = [("House", "(n.) a building for human habitation"),
        ("Cat", "(n.) a small domesticated feline animal"),
        ("Lambda", "(n.) the eleventh letter of the Greek alphabet")]

writelookup :: String -> Writer [String] ()
writelookup word = case (lookup word list) of
   (Just def) -> writer((), ["Found: " ++ def])
   (Nothing) -> writer((), ["Nothing found!"])

main = do
  putStrLn "Please enter your dictionary lookup:"
  name <- getLine
  let w1 = do
      writelookup name
  print $ runWriter w1
```

----


----

Is it `IO (Maybe)` or is it `Maybe (IO)`?

`IO (Maybe)`
- Login to locked screen. IO to read password. Just password if is matches. Nothing is password fais.

`Maybe (IO)`
- ... example?

---- 



