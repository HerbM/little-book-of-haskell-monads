## 4E. Iteration in `do` Blocks I

### `do` Block Iteration via Recursion

Now we look at **how do we repeat some code**. In imperative languages,
the answer is normally to use a `for` loop (or to use something fancier like
iterators and generators, etc..). Can we do something similar in Haskell?
Let's suppose we have a program that reads our name and says hello to us.

```haskell
main = do -- IO monad
   putStrLn "What's your name?"
   name <- getLine
   putStrLn $ "Hello " ++ "\n"
```

As it stands right now, this code is only going ask us our name *once*.
What if we want it to ask us our name *several times*.
How we can we repeat this code? 

It turns out, the easiest way is to use
nested do blocks to create an *infinite loop*.
Below is a program that will ask for your name repeatedly and never stop.

```haskell
main = do -- IO monad
  let askName = do -- IO monad
       putStrLn "What's your name?"
       name <- getLine
       putStrLn $ "Hello " ++ "\n"
       askName
  askName
```

```
What is your name?
> James
Hello James

What is your name?
> Mark
Hello Mark

What is your name?
> Susan
Hello Susan

What is your name?
> Jack
Hello Jack
...
```

We have used *recursion* to solve this problem. The do block has a reference to itself. 
We assign the do block to a value called `askName`, then inside the do-block itself we have `askName` 
on the last line. The above pattern is useful if you want to implement a REPL (read-eval-print-loop).
But what if we only wanted to ask your name say, three times? The above isn't going to work
because it'll ask your name indefinitely. We can cheat and hard-code `askName` three times:

```haskell
main = do
  let askName = do
       putStrLn "What's your name?"
       name <- getLine
       putStrLn $ "Hello " ++ "\n"
  askName
  askName
  askName
  putStrLn "Done."
```


That'll work:

```
What is your name?
> James
Hello James

What is your name?
> Mark
Hello Mark

What is your name?
> Susan
Hello Susan

Done.
```

... but it's not great. Indeed, what if somebody came along and wanted our program to ask our name **N** times
where we don't know what **N** is at compile time? We can again use *recursion* to solve this problem. 
This time however, we need a **counter** to stop us looping indefinitely. The counter
will tell us how many more times we need to ask the name. It'll start at **N** and count 
down each time we ask the name until it reaches zero. When it's zero, we need to ask zero times,
that is, we no longer need to ask so we stop.

```haskell
import Control.Monad (liftM)

main = do
  putStrLn $ "How many names?"
  count <- (liftM read)(getLine) :: IO Int
  let askName n = do
       if (n == 0) then do
         putStrLn $ "Done."
       else do
         putStrLn "What's your name?"
         name <- getLine
         putStrLn $ "Hello " ++ name ++ "\n"
         askName (n - 1)
  askName count
```

```
What is your name?
> James
Hello James

What is your name?
> Mark
Hello Mark

What is your name?
> Susan
Hello Susan

Done.
```


While using nested do loops works, it's not very succinct! It would be a lot of boiler-plate
if we had do this kind of recursion every time we want iteration. Each time we have to extract a variable
and introduce a counting argument. It would be nice if we could
write a generic function that could do this for us.

```haskell
import Control.Monad (liftM)

loop n block = do
  let blockCount n = do
      if (n == 1) then do
        block
      else do
        block
        blockCount (n - 1)
  blockCount n
```

We can then use the `loop` function like so:

```haskell
main = do
  putStrLn $ "How many names?"
  count <- (liftM read)(getLine) :: IO Int
  loop count $ do
     putStrLn "What's your name?"
     name <- getLine
     putStrLn $ "Hello " ++ name ++ "\n"
 ```


### `do` Block Iteration via Lists


Another strategy is to uses lists and **list comprehensions**.


```haskell
import Control.Monad (liftM)

loop [] = do
  return ()
loop [b] = do
   b
loop (b:bs) = do
   b
   loop bs

main = do
  putStrLn $ "How many names?"
  count <- (liftM read)(getLine) :: IO Int
  let askName i = do
       putStrLn $ "What's your name? (" ++ (show i) ++ ")"
       name <- getLine
       putStrLn $ "Hello " ++ name ++ "\n"
  let askNames = [ askName i | i <- [1..count] ]
  loop askNames
```

The above relies on us creating a list of do blocks. A simpler approach is to pass the index counting 
list `[1..count]` directly to our `loop` function and let it call `askName` with the appropriate index
when necessary.

```haskell
import Control.Monad (liftM)

loop [] f = do
  return ()
loop [i] f = do
   f i
loop (i:is) f = do
   f i
   loop is f

main = do
  putStrLn $ "How many names?"
  count <- (liftM read)(getLine) :: IO Int
  let askName i = do
       putStrLn $ "What's your name? (" ++ (show i) ++ ")"
       name <- getLine
       putStrLn $ "Hello " ++ name ++ "\n"
  loop [1..count] askName
```

The above approach is much nicer. But what if our do block only has one line. We'd have to write the following:

```haskell
import Control.Monad (liftM)

loop [] f = do
  return ()
loop [i] f = do
   f i
loop (i:is) f = do
   f i
   loop is f

main = do
  putStrLn $ "Say hello many times?"
  count <- (liftM read)(getLine) :: IO Int
  let sayHello i = do
    putStrLn $ "Hello " ++ (show i)
  loop [1..count] sayHello
```

This is slightly long. All we are doing is calling putStrLn repeatedly with different arguments.
Perhaps we could write another version of `loop` that works for do blocks with one 
line that has an action where the argument of the action changes each time.

```haskell
import Control.Monad (liftM)

loopAction [] act = do
  return ()
loopAction [i] act = do
   act i
loopAction (i:is) act = do
   act i
   loopAction is act

main = do
  putStrLn $ "Say hello many times?"
  count <- (liftM read)(getLine) :: IO Int
  loopAction ["Hello " ++ show i | i <- [1..count]] putStrLn

```
