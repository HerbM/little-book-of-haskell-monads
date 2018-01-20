## 3C. Conditions in `do` blocks II


### Using Actions instead of `do` blocks



### How does `when` "not execute" a do-block?

Haskell if a functional programming language and every function
has an output. 
What exactly do we mean by "not executed". 


the `when` will instead we perform a `return ()`.
So if you (for reason) inspected the value the `when` evaluated to, you'll see it's `()`.

```haskell
import Control.Monad (when)

main = do
 result <- when (True) (putStrLn "hi!")
 print $ result
```

*Output*
```
hi!
()
```


The `<do-block>` can be either a fully-fledged `do`-block, or a simple monadic action like `putStrLn` 
or `getLine`.

