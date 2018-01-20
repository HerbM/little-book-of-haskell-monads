## XB. Monad, Applicatives and Functors







### Monads as Functors


Every functor has a lift function. Every monad is a functor.
So we can lift every monad.


We can't use `lift` with monads. 
Instead we must use `liftM`.


```haskell
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  
```  

Just as we can `fmap` the `reverse` function over `Just "blah"` to get `Just "halb"`, 
so too can we can `fmap` the `reverse` function over `getLine`.
By exploiting the fact that the IO monad is a functor, we can write shorter programs.

> **Key Point**: Monads are **functors**. As a result, we can use `fmap` to write shorter programs

----

**Example**    
For the following Haskell program
```haskell
import Data.Char (toUpper)
main = do
  let getShout = fmap ((++"!") . map toUpper) getLine
  x <- getShout
  putStrLn x
```
what is sent to STDOUT when `hello` is fed in through STDIN?

*Answer:*
`HELLO!`


----

> **Key Point**: Monads are **applicative functors**. As a result, we can use `<$>` and `<*>` to write shorter programs

```haskell
-- Expression A
main = do
    x <- getLine
    y <- getLine
    return x++y

-- Expression B
main = do  
    (++) <$> getLine <*> getLine  
```    


---- 

Every monad is a functor as so we expect that whenever we create our own monads,
then we can use them in the applicative style. Unfortunately this is not the case: 
```
Warning:
    ‘FooMonad’ is an instance of Monad but not Applicative - this will become an error in GHC 7.10, under the Applicative-Monad Proposal
```

We must explicitly state that our own Monads are an instance of `Applicative`.


### Functions as Functors

```
(+) <$> (+3) <*> (*100)
```

---

What happens if we fmap a function needing an extra parameter like `(+)` over a function?
```
fmap (+) (*2)
```

A functor now holds a function, but besides the usual `x` parameter, it now wants another 
parameter.

It turns out that functions are just functors, they're also *applicative functors* (*applicatives*)!


### Functions as Applicative Functors

You can think of functions as boxes that contain their eventual results, 
so doing k <$> f <*> g creates a function that will call k 
with the eventual results from f and g. 
We can operate on the eventual results of functions as if we already had their results.

In the example above, at no point do we need to mention the results by name. 
There are no explicit parameters or lambdas.
We only care about the operations on the eventual results.
This style of programming is called **point-free arthimetic** (**tacit programming**).

```
(*2) -- Functor
(+) <$> (*2) -- Applicative Functor
```

```haskell
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    F <*> g = \x -> ...
```

The definition of `<*>` is a little tricky, so let's break it down.

First what's F and g here? 
Well both of them are functors.
The difference is that F is an *applicative* functor whereas g is not. 
That means F must be a functor that has had a function already `fmap`-ed over it.
But here, the functors are functions. Relating to the box analogy, 
the box *is* the function.
So we must have a *function* that has had a function `fmap`-ed over it.
Futhermore, this function (`(+)` in our example) requires an extra parameter.

In other words F is of the form `a -> b -> r` as explained below: 
```
        /----- a function b ->r ...
a -> b -> r
|       \---- ...that has is wrapped with another function...
\------ .. and furthermore the wrapper function requires an extra parameter
```

Now we want to work out what functor application means in this context.
We want to do a more grown-up version of "applying values to a function", 
but the "values" here are functors and the "function" in question
is an applicative functor. In this section, the functors in the spotlight are 
*functions* and the applicative
functor is a function that has had a function `fmap`-ed over and futhermore,
that function needs an extra parameter besides `x`.

For us, this new form of application 
is passing x into each functor, `(*2)`, `(+10)`, and taking the results
and applying it to `(+)`.


Passing `x` into `(+10)` is easy, we just call `g x`.
How do pass `x` into `(*2)`? 

```haskell
F  = (+) <$> (*2)
   = fmap (+) (*2)            -- by def of <$>, it's just the infix version of fmap
   = \x -> (+)((*2)(x))(x)    -- by def of fmap c.f. functions as funtors, fmap f g = (\x -> f (g x))  
   = \x -> \r -> (*2)(x) + r  -- changing prefix to infix
```

So to pass x into `(*2)`, we just need to call `F x`.
Finally, we need to pass the two results to `(+)`.

Notice that when we call `F x`, as well as passing `x` into `(*2)`, we get
back the following: `\r -> 2x + r`. We have the result of `(*2)` already in place.
We just need to fill those `r`s with the result of `(10+)` -- which
can be done by applying `x` to `F`: into `F x`.

Hence we arrive at:
```haskell
F <*> g = \x -> (F x)(g x)  
```
This is more conventially written as:

```haskell
F <*> g = \x -> F x (g x)  
```



We don't often use functions as applicatives. It's an intersting example that helps
test our knowledge of applicative functors. But more importantly,
knowing how functions are applicatives work pathes the way for us to understand 
how *functions as monads* (*reader monad*) works.

### Functions as Monads (The Reader Monad)

http://learnyouahaskell.com/for-a-few-monads-more#reader
