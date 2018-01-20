## 5D. Receiving Input Commands


### Combining State and IO

It's been a long journey to reach this point but we're finally here.
Now we have Monad Transformers and lift, we can finally read input from stdin and
modify some internal state. Most Haskell beginners start with this problem before 
knowing about Monads and Monad Transformers and are disappointed to find there is a 
lot of work needed to get this to work. But here we are! Enough talking - let's do it!

In this example we have *two* layers.

```
    Innermost (top)
         ↓
IO <- StateT
```

```haskell
import Control.Monad.Trans.Class -- lift
import Control.Monad.Trans.State.Lazy -- State, StateT, get, modify

type Command = IO
type CommandCounter = StateT Int Command
```

Let's create a basic REPL that reads a command and prints it back to us.
We can use a loop similar to the infinite loop we created 3C.

```haskell
repl = do
  command <- lift $ getLine
  lift $ putStrLn $ "Command = " ++ command ++ "\n"
  repl

main = do
  evalStateT repl 1
  return ()
```

Remember that monad transformer stacks default is innermost monad which in this 
case is the *State monad*. Both (i) reading commands and (ii) printing them needs the *IO monad*
which is one level out (it's not the default), so we'll need to use `lift`.
So in our state evalation, we have `lift $ getLine` to read input and to print output
we use `lift $ putStrLn`. Running the above, we have a simple REPL that echos our commands:

*Output*:
```
$ runhaskell foo.hs
> foo
Command = foo

> bar
Command = bar

> baz
Command = baz
...
```

Now let's read these commands and use them to mutate our state. Let's suppose we have the following commands

- `inc` - increments the counter
- `dec` - decrements the counter
- `print` - prints the current value of the counter

We can implement these using our monad transformer stack.
We'll need to read the command and perform a string comparison
check if it equals `inc`, `dec` or `print` respectively.

```haskell
repl = do
  command <- lift $ getLine
  when (command == "inc") $ do
    -- TODO: increment state
    -- ...
  when (command == "dec") $ do
    -- TODO: decrement state
    -- ...
  when (command == "print") $ do
    -- TODO: print current state
    -- ...
  repl
```

Great! Now just need to use the `modify` and `get` helpers of the State monad to modify
and read the state and we're all done!

```haskell
import Control.Monad.Trans.Class -- lift
import Control.Monad.Trans.State.Lazy -- StateT, get, modify
import Control.Monad (when)

type Command = IO
type CommandCounter = StateT Int Command

repl = do
  command <- lift $ getLine
  when (command == "inc") $ do
    modify (\x -> x + 1)
  when (command == "dec") $ do
    modify (\x -> x - 1)
  when (command == "print") $ do
    current <- get
    lift $ putStrLn $ show current
  repl

main = do
  evalStateT repl 0
  return ()
```


### Using Parsec to parse commands

Instead of a simple counter, let's instead have two numbers, or rather
a *vector*. Suppose our commands where not 
simple strings like `inc` and `dec` but could be something like
`inc (10, -20)` or `sub (4, 9)`. It's going to a little tricky parsing these commands.
Luckily, we can use **Parsec**.

Parsec is a library for parsing complicated strings and turning them
into Haskell `data` structures. Parsec has a Monad that holds
the internal state of the parse tree.


```haskell
import Control.Monad.Trans.Class -- lift
import Control.Monad.Trans.State.Lazy -- State, StateT, get, modify
import Text.Parsec
import Text.ParserCombinators.Parsec

data Command = Inc (Int,Int)
  | Dec (Int,Int)
  | Print
  deriving Show

parseWhitespace = many $ oneOf " \n\t"

parseCoordinates :: GenParser Char st (Int, Int)
parseCoordinates = do
  char '('
  xs <- many1 digit
  char ','
  parseWhitespace
  ys <- many1 digit
  char ')'
  return ((read xs), (read ys))

parseIncrement = do
  string "inc"
  parseWhitespace
  (x, y) <- parseCoordinates
  return $ Inc (x,y)

parseDecrement = do
  string "dec"
  parseWhitespace
  (x, y) <- parseCoordinates
  return $ Dec (x,y)

parsePrint = do
  string "print"
  return Print

parseCommand = do
  parseIncrement
    <|> parseDecrement
    <|> parsePrint

repl = do
  command <- lift $ getLine
  let println = lift . putStrLn
  let result = parse parseCommand "" command
  case result of
    Left _ -> println "Parse Fail"
    Right result ->
       case result of
         Inc (dx, dy) -> modify (\(x,y) -> (x + dx, y + dy))
         Dec (dx, dy) -> modify (\(x,y) -> (x - dx, y - dy))
         Print -> do
            current <- get
            println $ show current
  repl

main = do
  evalStateT repl (0, 0)
  return ()
```

*Output*:
```
> print
(0,0)
> inc (20, 10)
> print
(20,10)
> dec (5, 5)
> print
(15,5)
> inc (100, 100)
> print
(115,105)
```
