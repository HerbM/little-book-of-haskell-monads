## 1B. Scope and Lambda Nesting

### Scopes

Consider the following do block:

```haskell
do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
``` 

One question is: *how does*  **scoping**  *work*? How is it that we can use `a` and `b` 
like that on that last line with `(a*b)`? Is this like imperative languages? Does the compiler have to 
walk through this do block line-by-line and maintain some sort of '*symbol table*' table to c
heck whether each identifier is contained inside? The answer is: *No*!

The magic behind the scoping these 'variables' (like `a` and `b`) in a do block, 
is simply a result of the **nested structure** of the equivalent lambda-bind expression!
The lambda-bind version of same do-block looks like this:

```haskell
(λa →                                               ) =<< logNumber 3  -- level 2
      (λb →                      ) =<< logNumber 5                     -- level 1
              return (a*b)                                             -- level 0
```
For emphasis, I've broken out each level of the nesting onto a newline.

Because the lambdas are **nested**, we have that:
- Everything on the level 1 has access to `a`.
- Everything on the level 0 has access to both `a` and `b`.

This is true because for any lambda, `(λx → <expr>)`, the value of `x` is available anywhere inside `<expr>`. 
So it is indeed important that lambdas are nested. Suppose we did not have nesting. If had some left arrow (`a <- foo`), 
we'd find that any attempt to use `a` on any later line would fail! 
The rule is that every time we use a left arrow operator, we make that variable accessible to all later lines.


> **Key Point:** The lambda expressions are nested. This means that the body of inner lambdas have access to the parameters of outer lambdas. 

..

> **Key Point:** In a do block, variables are scope such that once you perform an assignment: `x <- RHS`, any line later in the do block has access to x. Lines before this assignment in the do block do not have access this x.

What's remarkable is how similar we can achieve assignment as seen imperative languages yet really 
we are using lambda expressions. Of course this is still functional programming, so this so-called assignment
is *assign-once*. We can't change `a`. 

----

**Example**

For the following expression:
```
(λx1 →  ((λx2 → f3 x1 x2)  <<= f2)) <<= f1
```

**(a)** Highlight the scope of x1, x2    
**(b)** rewrite it using the do notation

*Solution*

**(a)** Below illustrates the highlighted regions that show where x1 and x2 are in scope 
(the scope of x1 and x2):
```
(λx1 →  ((λx2 → f3 x1 x2)  <<= f2)) <<= f1
        |-------------------------|           scope of x1
                |-------|                     scope of x2
```

We can also break the nesting into levels to show the scopes of x1 and x2:
```
(λx1 →                                               ) =<< f1  
      (λx2 →                      ) =<< f2                     -- scope of x1
              f3 x1 x2                                         -- scope of x2
```

**(b)** 
We reverse the algorithm from 1A. Instead of going from a do-block to a lambda-bind expression,
we go in the opposite direction.
```
do
  x1 <- f1
  x2 <- f2
  f3 x1 x2
```

-----

### Variable Shadowing

From imperative programming, you should be familiar with the idea of [variable shadowing](https://en.wikipedia.org/wiki/Variable_shadowing). For example:

```java
int x = 42;

if (y == 1) {
  String x = "hello";
  System.out.println(x); // which x is shown?
}
```

In one scope `x` is an integer equal to 42. But in a nested scope, `x` is a string equal to "hello".
When working inside the if-block, if you access `x`, 
you'll access the *string*, not the integer. This is because the declaration is in some way 
closer and shadows the other definition that is further away.

Similarly in Haskell, we can have variable shadowing in do-blocks.

```haskell
multWithLog = do  
    a <- logNumber 3  
    a <- logNumber 5  
    return a
``` 


Which `a` will be returned above? Will we get 3 or 5. The answer is `5` (the one from `logNumber 5`).
The `a` on the second line of the do block has shadowed the `a` on the first line.    
In terms of lambdas and binds, we have the following:

```haskell
(λa →                                               ) =<< logNumber 3       -- level 2
      (λa →                      ) =<< logNumber 5                          -- level 1
              return a                                                      -- level 0
```

As we can see, the `return a` is nested at the deepest level (level 0). When we go to grab the value of `a`,
we start at the level and go upwards through all the levels, 1 then 2, then 3, etc .. until we hit
the first `λa` – the first lambda that provides a value for `a`. The `a` is bound to *this* lambda's parameter.     
So in the example above, the `a` of `return a` is bound to the `λa` of level 1, and hence
when we perform the bind, the `a` is assigned the unboxed result of `logNumber 5`, say 5. 
So 5 is returned (not 3). The `λa` on level1 is shadowing the value of `λa` of the lambda on level 2.

----

### Exercises


**2** Rewrite the following do block containing lets to a nesting of lambas and bind operators.
Underline the expression to show the scope of val1.  
