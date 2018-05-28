## XC. Kleisli and Function Composition with =<< 

If we have two functions, `f1` and `f2` then we can chain them togehter (`f1(f2 x)`).
But even better, we can create an operator that combines the two functions into one, `f = f1 . f2`.
This operator (`.`) is called the **function composition** operator. All of the ideas from "normal functions" 
can be applied to monadic functions.
So if there is a function composition operator for normal functions, there is also one for 
monadic functions. 

The monadic function composition operation is formally called **Kleisli composition**  (pronounced: *klai-slee*).
The operator is called a *Kleisli Arrow*.
In Haskell, it is written as `<=<`. It is a monadic equivalent of `.`.
It too is an infix operator that two different functions.
```
f =<< x           -- x is passed into f
g <=< f =<< x     -- x is passed through f then g
```


### Kleisli Category

The **Kleisli Category** is a mathematical space where all monadic functions belong.
Instead of having a *Function Category* with regular functions and function composition, we 
instead have monadic functions and the Kleisli Arrow.

- A monadic function is the "object" in the Kleisli Category
- The Kleisli composition operator `<=<` is the "arrow" that connects together monadic functions (the "objects") in the Kleisli Category


The **Kleisli composition** has the following definition: 

```
(m >=> n) x = 
do
  y <- m x
  n y
```

here **m** and **n** are two monadic functions.
The net result of `(m >=> n)` is a brand new 
single monadic function that performs the effects of both m and n with m's effect sequenced before n.
