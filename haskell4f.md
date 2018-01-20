## 4F. Iteration in `do` Blocks II

### Storing Results from Iterations

Previously we looked at how we can perform iteration in do blocks.
An important distinction to consider is **whether we care about return values** or not.
The previous section we discarded results from do block being iterated. But we can collect them.

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






### Built-in Functions

Haskell has four functions that help us deal with 'loops' in a more elegant way.
These are functions called `forM`, `mapM`, `sequence`, and `replicate`.
And depending on whether or not we care about the returning results, we can choose between
two versions, the version that includes the result and a version that ignores the results. 
So there are in fact, *eight* functions:

| With Results    | Without Results  |
| ----------------|------------------|
| `forM`          | `forM_`          |
| `mapM`          | `mapM_`          | 
| `sequence`      | `sequence_`      |
| `replicateM`    | `replicateM_`    |

The documentation for [Control.Monad](https://downloads.haskell.org/~ghc/7.0-latest/docs/html/libraries/base-4.3.1.0/Control-Monad.html#g:3) explains that the underscores in the function names are a naming convention to help us distinguish
between the two versions. If there is an underscore, it shows it's the version where we *don't care* about the results.
And conversely, if there is no underscore, we have the version where we *do care* about the results.

Let's take a look at what how eight built-in functions let us do iteration:

- **forM**: `forM xs b` takes a single `do`-block and runs it repeatedly for each value in `xs`
    ```haskell
    --  forM xs b
    do
      r1 <- b x1
      r2 <- b x2
      ...
      rn <- b xn
      return [x1, x2, ..., xn]

    --  forM_ xs b
    do
      _ <- b x1
      _ <- b x2
      ...
      _ <- b xn
      return ()
    ```
- **mapM**: `mapM b xs` does the same as `forM xs b`, except that the arguments are flipped
    ```haskell
    --  mapM b xs
    do
      r1 <- b x1
      r2 <- b x2
      ...
      rn < -b xn  

    --  mapM_ b xs
    do
      _ <- b x1
      _ <- b x2
      ...
      _ < -b xn  
    ```
- **sequence**: `sequence bs` takes a list of `do`-blocks and runs them in order
    ```haskell
    -- sequence bs
    do
      r1 <- b1
      r2 <- b2
      ...
      rn <- bn
      return [r1, r2, ..., rn]

    -- sequence_ fs
    do
      _ <- b1
      _ <- b2
      ...
      _ <- bn
      return ()
     ```
- **replicateM**: `replicateM n b` takes a single `do`-block and runs it `n` times
    ```haskell
    --  replicateM n b
    do
      r1 <- b
      r2 <- b
      ...
      rn <- b
      return [x1, x2, ..., xn]

    --  replicateM_ n b
    do
      _ <- b
      _ <- b
      ...
      _ <- b
      return ()
    ```

The above functions are flexible. Recall that a do-block and a monadic action have the same type.
They can can be used either a single action `act` or for an entire do-block `b`. Neat!

---

**Example 1**

What is the output of the following? Explain why.

```haskell
main = do
  mapM_ print (words "hello\nthere")
```

*Solution*

From beginner Haskell, we know that `words "hello\nthere"` is the same as `["hello", "there"]`. 
And then from the definition of `mapM_` we can dedude that the `mapM_ print ["hello", "there"]` can be written as a do-block
that repeatedly calls print for each word in the list:

```haskell
main = do
  do
    _ <- print "hello"
    _ <- print "there"
    return ()
```

We're using the underscore version of mapM so our results from the print are being discarded.
From our work in 3A on extraneous do blocks, we know this is equivalent to:

```haskell
main = do
  _ <- print "hello"
  _ <- print "there"
  return ()
```

So the result of the `mapM_` is that we print "hello" and then print "there".

*Output*:
```
hello
there
```

----

**Example 2**  

What is the output from the following? Explain why.

```haskell
main = do
  let parsePair = (map read :: [String] -> [Int]) . words
  forM_ ["1 4", "2 3", "6 9"] $ (print . parsePair)
```

*Solution*:

Using the definition of `forM_`, the above is equivalent to the following:

```haskell
main = do
  let parsePair = (map read :: [String] -> [Int]) . words
  do
      _ <- (print . parsePair) "1 4"
      _ <- (print . parsePair) "2 3"
      _ <- (print . parsePair) "6 9"
      return ()
```

And of course, we can simplify this to give:

```haskell
main = do
  let parsePair = (map read :: [String] -> [Int]) . words
  _ <- (print . parsePair) "1 4"
  _ <- (print . parsePair) "2 3"
  _ <- (print . parsePair) "6 9"
  return ()
```

So what actually happens? We're using function composition (`.`) a lot
so it might be a bit hard to see. Let's look at the first assignment (the first `<-`).
We pass "1 4" through `parsePair` and the output of that is passed
through `print`. In full, "1 4" first goes through `words` 
to become `["1", "4"]` then this goes through read to give `[1, 4]`, then finally,
this goes through `print` and see `[1,4]` appearing in the console.

The same thing happens for "2 3" and "6 9". So overall we have the following output. 

*Output*:
```
[1, 4]
[2, 3]
[6, 9]
```

----

**Example 3**  

Simplify the following do block using one of the built-in functions: `forM`, `mapM`, `sequence`, and `replicate`.
```haskell
main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
```

*Solution*   
We're can rewrite the above

```haskell
main = do  
   rs <- do
      a <- getLine  
      b <- getLine  
      c <- getLine 
      return [a,b,c] 
   print rs 
```

this newly added do block is the same as `replicate 3 getLine`, so we can rewrite it as:

```haskell
main = do  
    rs <- replicateM 3 getLine
    print rs  
```

----

**Example 4**  

Simplify the following do block using `forM`.

```haskell
main = do
  putStrLn "Hello 1"
  putStrLn "Hello 2"
  putStrLn "Hello 3"
```

*Solution*   
The easiest way to get started is to having something like this:

```haskell
main = do
  forM_ [1,2,3] $ \x -> do
      putStrLn $ "Hello " ++ (show x)
```

That works! But, it's a little messy because we have two dollars hanging around. Instead, we can build up
the hello strings first. Then, instead of iterating over the indices, we can iterate over the 
hello strings.

```haskell
main = do
  let hellos = ["Hello" ++ show i | i <- [1,2,3]]
  forM_ hellos $ \hello -> do
      putStrLn hello
```

That's a bit better. The last part is bit clever. The do block we're passing into forM_
is exactly the same as putStrLn. We've just wrapped a superfluous function around it.
Our lambda is a function that takes in a string and prints it to the console ... it's exactly
what putStrLn does! This goes back to the point we made earlier: often do-blocks and monadic actions are
interchangable because they have the same type. And indeed here, we can remove this unnecessary lambda:

```haskell
main = do
  let hellos = ["Hello" ++ show i | i <- [1,2,3]]
  forM_ hellos putStrLn
```

Beautiful! And if you like it, you can remove the `hellos` variable and get a one-liner.

```haskell
main = forM_ ["Hello" ++ show i | i <- [1,2,3]] putStrLn 
```

But this might be taking it a bit too far ... it's perhaps less readable than the previous step.
But nonetheless this shows the power of `forM_` and function programming.
We can still write terse  expressions packed rich with behaviour and thanks to `forM_`,
we can still do all this while juggling with an IO side-effect of printing to the console ... amazing!

----

**Example 5**  

Rewrite the ask-our-name example to use the built-in iteration functions from `Control.Monad`.

*Solution*

One way would be the following:

```haskell
askName i = do
   putStrLn $ "What's your name? (" ++ (show i) ++ ")"
   name <- getLine
   putStrLn $ "Hello " ++ name ++ "\n"
   return name

main = forM_ [1,2,3] askName
```



----

### What's the deal with the M's at the end?

If you're wondering why some functions end with an M and some do not, there is a good reason why.
A suffix 'M' denotes that the function that accepts a **monadic function**
(or if you're a Maths geek, recall from 1C that when we say "monadic functions",
we mean functions belonging to the *Kleisli category*). Both `forM` and `mapM` accepts 
a monadic function as their first argument and so have the suffix 'M'. On the other hand,
`sequence` accepts a list as its first argument, not a monadic function
and so does *not* have an M suffix. 

This should remind you of the M in `liftM`. Recall `liftM` takes an ordinary function
and promotes it to a monadic function. In general if you see a capital M in a function
name, it's a hint that it's got something to do with monadic functions.

Generally Haskell function names tend to avoid following
in the footsteps of the Hungarian Notation and don't give type hints, but in this case we needed 
some kind of prefix/suffix to distinguish the `map` in Control.Monad from the regular `map` in the prelude 
to prevent a name conflict. Adding the M prevents `mapM` and `map` having a name conflict.


### File Parsing


```haskell
import Control.Monad (replicateM)

main = do
  n <- readLn :: IO Int
  words <- replicateM numLines getLine
  putStrLn $ show words
```

*names.txt*
```
3
Jack
Jill
John
```

```
$ runhaskell parsefile.hs < names.text
["Jack", "Jill", "John"]
```

This is also useful a competitive programming setting, where
you are often required to first parse a problem's data from a file 
before actually coming up with the algorithm solve the problem.

