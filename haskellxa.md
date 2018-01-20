## Appendix A: Simplifying `do` blocks and Monad Laws


**Law 1: Right-identity Law**

The *Right-Identity Law* states that the following two expressions are equivalent.
```haskell
-- Expression A
do 
  x <- e
  return x

-- Expression B
e  
```

**Law 2: Left-identity Law**

The *Right-Identity Law* states that the following two expressions are equivalent.
```haskell
-- Expression A
do 
  x <- pure y
  e2

-- Expression B
(\x -> e2)(y)
```
The first `do` block can be simplified by taking `e2` finding all the places where `x` is used in `e2`,
and replacing each `x` with the expression `y` in its entirety.


**Law 3: Associativity Law**

Haskell is a lazy language. With associativity we can choose the order in which 
we evaluate statements more flexibly, which lends itself more to laziness.
It turns out `=<<` is an associative operator. So:
```
we expect that:    f =<< (g =<< input fancy)
to be the same as: (f =<< g) =<< input fancy
```
... well almost!

The problem is that f =<< g, doesn’t really make much sense - 
remember that the right hand side of =<< is always a fancy value, 
it can’t be a function. The correct way to write this is shown below. 

```
      f =<< (g =<< fancy input)
(λx → f =<< g x) =<< fancy input
```

Notice we replace the erroneous (f =<< g) with a lambda. First notice that the lambda takes as input x.
What is x? Well, look at the outermost =<< and recall that bind does. It takes the fancy input, 
strips off the wrapper, and pushes it into the function of the left. 
In other words, x is the input as a normal value.  Now looking at the definition of the lambda (λx → f =<< g x), 
it tells us to take the normal input x, and push it through g first to get a fancy output.
Then take that fancy output and use it as an input for another bind operation that pushes it through f. 


Both expressions, f =<< (g =<< fancy input) and (λx → f =<< g x) =<< fancy input are performing two applications, passing the fancy input first through g, then through f. So they should be functionally equivalent. This is what we mean by associativity of bind.



The last law is the *Associativity Law*. 
It tells us how we can simplify nested do notation blocks. 
It states that the following two expressions are equivalent.

```haskell
-- Expression A
do
  y <- do
    x <- m1
    m2
  m3

-- Expression B
do
  x <- m1
  y <- m2
  m3
```

This is saying that we can flatten the nested do-block in Expression A and still achieve the same results.
To explain this, let's jsut focus on the nested block alone.

```
y <- do
 x <- m1
 x2
```

The overall result, `y` receives the result from
the last line of the do block, which is `m2`. So instead of this whole do-block, let's can just write `y <- m2`, 
and we'll know that `y` still has the same value.
But there's a small catch! `m2` might needs `x` to be in scope for its computation, 

```
-- Attempt 1:
y <- m2
-- hmm ... m2 might need x!
```

So let's add `x <- m1` first. From 2B, we know that this will make `x` in scope for all
subsequent lines, and hence will be in scope for the following line: `y <- m2` hence
`m2` will have access to `x`.

```
-- Attempt 2:
x <- m1
y <- m2
-- m2 can now access x
```

-----

**Example 1**

Adding extraneous `do`s is a classic mistake.
Below are two valid and equivalent ways to print "Hi!".
Using the monad laws we can show why they are equivalent.

```
main = do do do do putStrLn "Hi!"
main = putStrLn "Hi!"
```
