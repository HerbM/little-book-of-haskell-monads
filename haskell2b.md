## 2B. `=<<` as a Sequencing Operator

### Pure Programming on top of Side-effects


```
do
  x <- getFoo
  y <- getBar
```


### The Pineal Gland of Haskell

Don Sannella from the University of Glasgow came up with a nice analogy.
Philisophers worried about the connection between
**thoughts** (the mind) and **actions** (the body).
Is there a single point which connects the two.
Descartes said that the Pineal Gland (pine-shaped object in the head)
was the connection between the body and the mind.
Haskell has something like a Pineal Gland. Monads separate out computation
into thinking and acting.

For example:
```
putChar :: Char -> IO ()
```

`putChar 'x'` denotes that, *if ever performed*, this action will
print an x. Writing `putChar 'x'` doesn't actually perform a command (side-effect)
it just yields a description or a thought of how to perform the command.
Producting a description of a command is purely functional. If we actually performed
the command then this would not be functional. 

And when we use this description of a command together with `=<<`, again
we are only describing how to connect together several commands.
So:
```
(\_  -> putChar 'y') =<< putChar 'x'
```
denotes that, *if ever performed*, we should print an x then print a y.
The `=<<` operator yields a description or a **thought** of how to perform the commands in a sequence.
Producing a description of sequencing two or more command is also purely functional. 
If the `=<<` operator actually went ahead and performed
these commands then this would not be functional – because a side-effect would occur.

> **Key Point**: When Haskell evaluates a statement like `putChar 'x'`, instead of immediately
exeucting the action, evaluates to a **description** (thought) that says that *if this ever performed, print an x*. 
This is the opposite to imperative languages where this would evaluate immedatiely
to **action**: *immediately perform this and print an x*

By now you may be desperate to know how is a command ever performed?
Thus `main` is the link from Haskell’s mind to Haskell’s body — the analogue of
Descartes’s pineal gland.

> **Key Point**: We write a description of side-effects happen. These commands are not executed.
The **`main` function** is the link that connects thoughts to actions. The main function
will turn our action descriptions (thoughts) into actions that have an effect on the world.

Thus `main` is the link from Haskell’s mind to Haskell’s body — the analogue of
Descartes’s pineal gland.

----

**Example**

```
done :: IO ()
```

Here `done` doesn’t actually do nothing; it just specifies the idea that, *if
it is ever performed*, it won’t do anything. Compare thinking about doing nothing
to actually doing nothing: they two are distinct.

----


### Using Nested Lambdas and Binds

Let's take our definition of `=<<`. 
Let's suppose our function was a lambda.

```
(\x1 -> (\x2 -> (\x3 -> e) =<< m2 ) =<< m1
```

- Unbox m1 to give x1
- Evaluate first lambda to give `(\x2 -> (\x3 -> e) =<< m2 )`
- Unbox m2 to give x2
- Evaluate second lambda to give `(\x3 -> e)`
- Unbox m3 to give x3
- Evaluate third lambda to give `e`

Let's suppose we have two rules:
- Each time we use `=<<` to do an "unboxing" we are allowed to perform a side-effect,
- m explains how to do the side-effect, the side-effect produces a value x

Then how could this work.

----

**Example 1**

For the do block below,
fill in the blanks to show how the lamba-bind expression is evaluated at each stage.

```
do            
              (λx1 →  ((λx2 → f3 x1 x2)  =<< f2)) =<< f1
  x1 <- f1
              ........................................
  x2 <- f2
              ........................................
  f3 x1 x2    
              ........................................
```

*Solution*

```
do            
              (λx1 →  ((λx2 → f3 x1 x2)  =<< f2)) =<< f1
  x1 <- f1
              ((λx2 → f3 y1 x2)  =<< f2)
  x2 <- f2
              f3 y1 y2
  f3 x1 x2    
              y3
```


----

### Not Imperative

This form of computation is not actually imperative. 
We are just evaluating a bunch of nested functions. 
Although the do notation makes it look like we are doing things in a stepwise manner, 
really, we are just evaluating a single expression full of a bunch of functions. 
The ‘steps’ we take in the do notation correspond to evaluating the big expression according to the bind operators. 
We can instead, think of this form of computation as being *pseudo-imperative*

----

Remember our first explanation of the type signature of `=<<`.
It takes a boxed value, unwraps it and calls a function with it.

- The boxed value is the `<rhs>`
- The function we call is the lambda
in the lambda bind expression.

> **Key Point** The function we pass to `=<<` will be a lambda
in the lambda bind expression. The value we pass to `=<<` is the right hand
side of a `x <- <rhs>`. The paramemter of the lambda is `\x` and
is bound to the unboxed version of `<rhs>`.


----

**Example 1**

Let's look at how nested lambdas and binds can read numbers.

```
main = (\x1 -> (\x2 -> (\x3 -> (x1 ++ x2 ++ x3)) =<< getLine) =<< getLine) =<< getLine
```


- `getLine` from IO reads to give 1, so now x1 is 1.
- Evaluate first lambda to give `(\x2 -> (\x3 -> (x1 + x2 + x3) =<< getLine) =<< getLine)`
- `getLine` from IO reads to give x2, so now x2 is 5.
- Evaluate second lambda to give `(\x3 -> (x1 + x2 + x3) =<< getLine)`
- `getLine` from IO reads to give x3, so now x3 is 10.
- Evaluate third lambda to give `(x1 + x2 + x3)`, which evalutes to 16.

```
do            
  x1 <- getLine
              ((retreives a line from stdin, binds the value to x1))
  x2 <- getLine
              ((retreives a line from stdin, binds the value to x2))
  x3 <- getLine
              ((retreives a line from stdin, binds the value to x3))
  x1 ++ x2 ++ x3    
```

This works and we will concatenate three strings, but we unfortunately can't see the result.
It would be nice if we could print the result.

---



> **Key Point**: We can think of `=<<` of a **sequencing operator** because it
allows us move from one line to the next line in a do-block where:
>    - each line `x <- <rhs>` corresponds to a `=<<`
>    - in order to "execute" `x <- <rhs>`, we must use `=<<`
>    - `=<<` performs a side-effect encapsulated in the declaration of `<rhs>`,
>    - this side-effect (as well as producing some side-effect result outside of our do-block),
       also produces an pure output value take comes back to our do-block and we bind it to `x`
>    - lines below in the do block might use this output `x`. 
       In order to execute those lines, we must execute this line first to get `x`
>    - so overall, when we execute this do block, this line will\* be executed first then
       the line that needs `x` will then be executed
>    - the lines executed in sequence and `=<<` is the glue that holds the sequence together, making it a 
       sequencing operator



So to summarise:
- **A layer of abstraction**: We've abstracted out exactly how the side-effects are performed. We're not looking
at 
- **Sequencing side-effects**: We've got a pairing of side-effect descriptions to side-effect results, that
is: `(m1, x1)`, `(m2, x2)`, `(m3, x3)`, ... and furthermore, these pairs have a vague *order* or *sequence*, so we 
achieve an ordering for the pairs: `[(m1, x1), (m2, x2), ..., (mn, xn)]`. What this order actually means is up to
the monad and its associated side-effect to choose
which usually means the order in which the side-effect should occurs (that is, the order is time).
- **Keeping ahold of results**: When we get a result from a side-effect, `x`, we can use it any time later. 
This is because the lambdas are nested.  Once we get `x` and arrive at `\x -> ...`, 
our setup is that everything else that comes later is nested inside`\x ->`. 
As a result, everything else can see and use `x`. This is great 
because we are not immediately throwing away results from side-effects, 
we can keep them for "later" computations -- once we get `x` the structure of the nested
lambdas keeps it in scope.
- **Producing a single result overall**:

### Where now?

**Can we look inside a Monad?**


**How do we implement our own Monads?**

----

### Exercises

