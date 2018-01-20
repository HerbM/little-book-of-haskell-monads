## 3A. Newtypes, Typeclasses, 

```haskell
newtype Foo = Foo { getFoo :: Int }
y = getFoo (Foo 2)
main = (putStrLn . show) y
```


In the world of academia in an area known as **Computability** (closely tied with **Complexity Theory**), 
it has been shown that
the problem of *deciding if one partially applied types and another type are equal*
is equivalent to the problem of *deciding whether two functions are equal*.
The latter problem is classically known to an **undecidable problem** (impossible to implement),
hence the former problem is also an undecidable problem.

> **Key Point** For good reasons, **Haskell doesn't allow partially applied type synonyms**.
It can be mathematically shown that it's impossible to write a compiler 
that can check if a type and a partially applied type synonym are equal.

Haskell needs to know when types are equal.






**Exercises**    

1. Explain why the definition of Y is invalid. 
```haskell
data X a = Foo Int String a
type Y a = Bar [Bool] a
type Z a = X Y a
```

2. Which of the following definitions of Y are valid?
```
type Y = 
type Y a = 
type Y a b = 
type Y a b c = 
```
