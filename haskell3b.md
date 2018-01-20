## 3B. Internal State I

We can use `fold` (reduce) to manipulate an internal state in a functional way.
Here we will store a list of functions or *reducers*.

```haskell
r1 = \s -> s + 5
r2 = \s -> s * 2
r3 = \s -> s - 100
reducers = [r1, r2, r3]

s = 1
f = \s -> \r -> r s
fold f [r1, r2, r3] s
```

> **Key Point** To maintain state in a *functional setting*, we use **reductions**.


As previously mentioned, understanding monads can be decomposed into three upward struggles:    

1. understanding what bind is and why we need it    
2. translating the do blocks 
3. using monads to hold an internal state *(this chapter!)*

In this chapter, we'll begin tackling our third and final struggle and hopefully
you'll begin to understand what monads are all about.

First we'll create a monad called `Reducer`.

```haskell
newtype Reducer s r = ...
instance Monad (Reducer s) where ...
```

Creating a monad is a little tricky, so
we'll skip this creation step for now. It's better to show
you how the monad works first and in the next part we'll properly look into these creation details.





### Reminder: typeclasses

Before we look at creating a Monad, let's have a reminder of what a *typeclass* is.


- A newtype T is allowed to be a *member* of a typeclass C.
- For a newtype T to be a member of a typeclass C, one or more functions `f` have to exist and exist umambiguously.
  Suppose x is an instance of newtype T and we call function `f`.
    ```
    f x
    ```
 Haskell cimplementation

Below shows an example of a typeclass called `Doubleable`.

```haskell
class Doublable t where
  double :: t -> t
```

This says that 



```haskell
instance Monad (Reducer s) where
    return x = Reducer (\s -> s)
    (Reducer g) >>= f = ...
```

What does this mean? 









