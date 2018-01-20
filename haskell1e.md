## 1E. Input and Output with the `IO` Monad

```haskell
import Control.Monad (liftM)
main = do 
  -----------[4]-----------
        --------[3]--------
        ----[1]---- --[2]--
  a <- (liftM read)(getLine) :: IO Int
  print a   -- [5]
```

1. Recall that `read` parses a string to an int. `read :: String -> Int`. We use `liftM` to 'lift' the `read` fn to an IO monadic context. Here, `(liftM read) :: IO String -> IO Int`.
2. We use an action to perform a side-effect of getting the next line from stdin. Here, `getLine :: IO String`
3. We use vanilla function application to pass an IO String to a function of type: `IO String -> IO Int`. The result is an IO Int.
4. The do block's arrow `<-` takes the monadic value `IO Int`, pulls out `Int` and "assigns" it to `a`. Or in terms of lambdas and binds, we open a new lambda (`\a -> ... )` with `a` bound to the integer pulled out of the monadic value on the RHS of the `->`. 
5. Finally use `print a` to return a monadic value, print here has type: `Int -> IO Int`.


| IO Function | Description    |
| ----------- |------------| 
| `getLine`   | Reads a line   |
| `readLn`    | Reads a line and uses `read` to parse  | 
| `print`     | Writes the value of `show` to the console   |
| `putStrLn`  | Writes a string to the console with a newline |

```
main = (\val1 -> (\val2 -> (\sum -> (print sum))( val1 + val2 )) =<< readLn ) =<< readLn
```


**Example**    
```haskell
do 
  line <- getLine   
  let line' = reverse line  
  putStrLn $ "You said " ++ line' ++ " backwards!"  
  putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"
```




```haskell
main = do
    a <- liftM (map toUpper) getLine
    putStrLn a
```

```
$ runhaskell example.hs
Hello
"HELLO"
```


----

```
$ touch foo{1,2,3}.txt
```


----

## Exercises

**3.**
Write a program that accepts two integers and returns the first minus the second.  
*Hint: you will need to use readLn and print and one or more let statements). 
Write your program using (a) a do block (b) using lambdas and binds.*
