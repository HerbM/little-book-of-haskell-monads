## 2C. The Intuition Behind `a -> M b`


### Polymorphism *at each line*


Assume we're translating a standard do-block, 
Suppose we're translating a line of the form `x <- b`.
Remember that each `x <- b` of a standard do-block corresponds to a different `=<<`.
So each time we use `=<<`, its type `m a -> b` is needs to therefore address 
the fact that translating `x <- b` to a lambda and bind that will form
a part of a larger structure of nested lamba binds.

The type signature is **polymorphic**. It's saying that any type can replace `a` and
any type can replace `b` here. And we really mean *any* type! ... String, Integer, boolean, you name it!
But be careful! This polymorphism is "scoped" for this particular function application.
A nested lambda bind may have many binds, 
but the `a` of one bind does not necessarily have to be the same
as the `a` of another bind (same for the `b`s). In other words, we have an instance
of **polymorphism occuring at each line of the block**, not polymorphism occuring once
for the entire block.

When we say `a -> M b`, it does not mean that for the do-block *all* the corresponding
binds have the same type input `a` and the same output type `M b`. What it means is that
out of all these binds, *one* particular bind has an input `a1` and an output type of `M b1`. 
And *another* bind has type `a2` and `M b2`, and so on. And furthermore, we're saying that 
we don't enforce any relation between `a1` and `a2`.
They might be same, they might be different, nobody knows!
The polymorphism occurs seperately for each bind. 
Each bind is for each do-block line so to put it another way, the polymorphism
occurs on a *line-by-line basis*.

> **Key Point**: At each line of a standard do-block `a -> M b`, we have **polymorphism**,
where the `x` can be any type `a` and the `rhs` can use any type 
`b` to formulate a side-effect description `M b`. This polymorphism occurs on a 
**line-by-line basis**. So one line's `a` and `b` types
is not necessarily the same as another line's `a` and `b` types.

If you're familar with logic, we might write this like:
```
for all a, for all b: a -> M b    
for all a, for all b: a -> M b    
```
The for all's are scoped such that a given bind won't necessarily share the same a and b.
Each bind get's its own for all for each variable and keeps it to itself. And so
polymorphism occurs *at each line*.

### Looking Closely at `M b`


To understand this sinature, I think the best way is to pretend we are in the middle of translating a do block,
and are translating a line of the 


```
 a1           ?                   Ma1
(x1 -> <next line translated>) =<< m1
```

What is the type of the `...`? Well if take the type signature: `(a1 -> M b) -> M a1 -> M b`,
the overall result must be M b, and futhermore this must match the result of the lambda.
So we must have:

```
 a1    Mb     Ma1
(x1 -> ...) =<< m1
\----- Mb -------/
```



Furthermore, let's suppose we had another lambda inside `...`.

```     
(x1 -> (x2 -> (...)) =<< m2 ) =<< m1
```

We said that this lambda bind that we just added must have type M b


```     
       -------- M b --------
(x1 -> (x2 -> (...)) =<< m2 ) =<< m1
```

In general, bind has type `(a -> M b) -> M a -> M b`.
If we take the type signature of this newly added lambda bind: `(a2 -> ?) -> M a2 -> M b`,
We see that the ? must be M b.

What we can see if that the M b propagates through.


Overall we have:

```
 a1    Mb      Ma1
(x1 -> ...) =<< m1
             ↑          

         a2    Mb       Ma2
(x1 -> ((x2 -> ...) =<<  m2 ) =<< m1
                     ↑

                a3     Mb       Ma3
(x1 -> ((x2 -> (x3 -> ... ) =<<  m3 ) =<< m2) =<< m1
                             ↑

                      Mb
(x1 -> ((x2 -> (x3 -> e ) =<< m3 ) =<< m2) =<< m1
```

- The types a1 a2, a3, ..., an are the types of the results of side-effects.
- It's up that particular side-effect mi function to determine the type of ai
- A given monad m doesn't know what side effect functions m1, ..., mn are going to be used in
  advance, and hence does know what the types a1, a2, ..., an are going to be.
- Mb is the result of the final side-effect expression e nested at innermost level of the lamba-bind expression.
- The type Mb propogates throughout. The overall type of the lamba-bind expression will be Mb.
  Unlike the `a`s which change at each line, the `b` stays the same. That is,
  we don't have b1, b2, ..., bn – it's the same b throughout.



### `M b` Propogates through all the lines

```
do 
  x1 <- m1  ←      x1 :: a1  m1 :: M a1
  ...              last-line :: M b
  
             do 
line 1:        x1 <- m1    
line 2:        x2 <- m2  ←      x2 :: a2, m2 :: M a2
               ...             
last line:     <last-line>      e :: M b
  
  
             do 
line 1:        x1 <- m1    
line 2:        x2 <- m2
line 3:        x3 <- m3  ←      x3 :: a3,  m3 :: M a3
               ...             
last line:     <last-line>      e :: M b



do 
  x1 <- m1  
  x2 <- m2
  x3 <- m3  ←      x3 :: a3, m3 :: M a3
  ...              last-line :: M b
  
do 
  x1 <- m1  
  x2 <- m2
  x3 <- m3 
  e           the last line has type Mb 
``` 


- In general, for **line i**, we have the statement `xi <- mi`.
    - we are using `<-` to unwrap a constructor so: `mi` has type `m ai` and `xi` has type `ai`  
    - line i accepts that the last line of the *entire* do-block will be of type `M b`
      despite it not even knowing what the other lines of the do-block will be.
      (the last lines of the do-block shown with 
- Mb is the result of the final line of the do-block, e
- The type Mb propogates throughout. The overall type of do-bkock Mb.
  
  we don't have b1, b2, ..., bn – it's the same b throughout.
  
  








> **Key Point** The `M b` propogates throughout the do-block. 
The types of `xi` and `mi` are allowed to change at each line `xi <- mi`, but
what stays the same throughout is `M b`. Every line of a do-block agrees that the last
statement of the do-block must have type `M b`.

The polymorphism of occurs on a line-by-line basis.



by virtue of the nesting of the funcions, `b` types of all the binds *are the same*.
What this means is that a given do block has one and only one return type `M b`.
There's no need to worry about `b1`, `b2`, `b3`, ..., `bn`, it's just the same 
`b` throughout!

And another important result is that the `M` **stays the same**!
What this means is that a given do-block **has one, and only one
particular monad** that it works with.



### Justifying the placement of `m`s in `a -> m b`


Previously in 1B, we complained about the mysterious placement of `m` that we
see for monadic functions. There are four somewhat reasonable
possiblities to consider:

1. `a -> b`
2. `a -> m b`  ← this placement of the `m`s was chosen, ... why?
3. `m a -> b`
4. `m a -> m b`

So why did we choose this second option? After our work in 1C, we can 
give some intution as to why.

- **Why 1. and 3. doesn't work**: The overall result must be a monad. If each bind accepted functions
that had type `a -> b` in a nested lamba-bind strucrure
then the eventual result would *not* be a monad. The same is true 
each bind accepted functions had type `m a -> b`. 
So we need that `m` in the output, that is we need ` <something> -> m b`.
- **Why 4. doesn't work**: Intuitively we're dealing with monads, and we like 
symmetry, so surely `m a -> m b` must work in our nested lamba-bind structure!
Well sadly not. We must have `a -> m b` not `m a -> m b`. Why is this?
Well think about what this left-hand side translates to in our lamba-bind structure.

So by process of elimination we are left with 2.

- **Why 2. `a -> m b` *does* work**
So we're left with only one choice `a -> m b`. It means that our values
in the lamba-bind are normal non-fancy values and it means that our overall
result is a monad. So it's perfect! The reasons go deeper and it's very 
easy to give a complex mathematical reason. One reason is that this is the 
only type signature that obeys the Monad Laws. But unless you're a mathematician
I'd recommended reading up on Monad Laws a bit later
when you have more experience (see Appendix XA if you're *really* intersted). 

### The need for *two* types, `a` *and* `b` in `a -> m b`

1. `m a -> a`
2. `m a -> b` ← we need an extra type `b` here ... why?









