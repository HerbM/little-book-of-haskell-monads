## 4A. Conditions in `do` Blocks I


### Using `when`

The `Control.Monad` module exposes a function that help us 
conditionally execute do blocks. This is called `when`.
When we run the code below:

```haskell
import Control.Monad (when)

main = do
 when False (do putStrLn "A")
 when True (do putStrLn "B")
```

The first `putStrLn` is not executed because we pass a false condition,
and the second `putStrLn` *is* executed because the condition is true, 
and we'd see `B` in standard out. 

*Output*

```
B
```


To summarise the behaviour of `when`:

> **when** - `when <condition> <do-block>`, if `<condition>` is true, then `<do-block>` is executed, otherwise the do-block is not executed.

We can for example, read a name from standard-in and print something if the name
matches our codename `007`:

```haskell
import Control.Monad (when)

main = do
 name <- getLine
 when (name == "007") (do putStrLn "Hello, Mr. Bond")
```

*Output*

```
runhaskell foo.js
> 001
runhaskell foo.js
> 007
Hello, Mr. Bond
```

### Using `unless`



In general, if you have `when (not C) b`, you can remove that annoying `not` in the condition
by using `unless`. `unless` is the boolean opposite of `when`. If runs the block when the condition
is *false*. It the condition is *true*, then it does not execute the do block.



### Using `if`-`then`-`else` for handling both cases


What if you want to do something in *both* the true case or in the false case.
We could use a `when` followed by an `unless` like so:


But this is duplicating the condition somewhat. Luckily Haskell has a contruct called 
`if`-`then`-`else`.

```haskell
import Control.Monad (when)

main = do
 putStrLn "Codename?"
 name <- getLine
 if (name == "007") then do
   putStrLn "Hello, Mr. Bond"
   putStrLn "What cocktail would you like sir?"
   cocktail <- getLine
   putStrLn $ "Here it your " ++ cocktail
 else do
   putStrLn "Go away!"
```

*Output*

In the case where you *are* 007:
```
Codename?
> 007
Hello, Mr. Bond
What cocktail would you like sir?
> Vodka Martini, shaken not stirred
Here it your Vodka Martini, shaken not stirred
```

And in the case where you *are not* 007:

```
Codename?
> 001
Go away!
```

Note that `if`-`then`-`else` is to be used when, and only when
you want to do something in *both* the true case
and the false case. Unlike other programming langauges, in Haskell, 
you can't write `if` without the `else` - they come *as a pair*! So the following
would *not* compile:


----

**Example**

What is the output from the following:

```haskell
main = do
  let foo x = do
      if (x == 3)
      then putStrLn "magic"
      else putStrLn "boring"
  foo 1
  foo 2
  foo 3
```

*Solution*: 

```
boring
boring
magic!
```

*Explanation*:
