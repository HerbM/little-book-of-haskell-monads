## 1D. Monadic Actions


### Introducing Actions

Consider the following.

```haskell
do 
  putStr “Hello”
  putStr “  ” 
  putStr “World”
  putStr “\n” 
```

In a functional setting this is strange because
every function has an output, just as it has an input and Haskell programs are formed
by connecting inputs to outputs. But here in our do block we appear to have a different case. Here we 
we have functions producing unimportant output values and connecting functions ignoring these unimportant
output values. 

Why are we doing this? What's different here. 
The difference is that `f1` and `f2` are performing *side-effects*. And furthermore,
we don't care about the output values of the function. 

These functions (`f1` and `f2`) as what we call **actions**. 
So far we have monadic functions that encapsulate computations with side-effects. 
Actions are also monadic functions. But what makes actions different, is they don't produce
a useful output value. Only the side-effect they cause it useful. Monadic functions in general
can produce a useful side-effect and a useful output value, but actions produces useless output 
values. Note that every action is a monadic function.  But not every monadic function is an action.


> **Key Point**: Monadic functions that **ignore their input** 
can encapsulate computations with *side effects*.
These functions that can be thought of as **actions**.


```haskell
do 
  putStr “Hello”
  putStr “  ” 
  putStr “World”
  putStr “\n” 

do 
  _ <- putStr “Hello”
  _ <- putStr “  ” 
  _ <- putStr “World”
  putStr “\n” 
```

In general, when we have a side-effect:

```
  y <- foo x
```

- x is an input to the side-effect
- foo describes how side-effect is done
- y is the output of the side-effect

Actions are special because they only are about foo and x, 
the y part is ignored.


We compute `f1`, and it will perform a computation with a side-effect. 
We don’t care about the value, we just want to move on to compute `f2`.

Each line has an expression, but we are not using `<-` nor are we using `let`. In fact
we are not using the return values from the expression at all. It looks very imperative, 
almost like a sequence of "procedure calls" (or C function calls).
But unsurprisingly, this is again, just a sequence of binds and lambdas.


```haskell
-- Expression A
do 
  f1 x1
  f2 x2
  f3 x3
  f4 x4
  
-- Expression B
do 
  _ <- f1 x1
  _ <- f2 x2
  _ <- f3 x3
  f4 x4

-- Expression C
(\_ -> (\_ -> (\_ -> f4 x4) =<< f3 x3 ) =<< f2 x2) =<< f1 x1
```



Let's look at the first part, 
```
(\_ -> (<other lines translated>) =<< f2 x2) =<< f1 x1
```


After `f1 x1`, none of the subsequent lines need to use this output value from this function.
Similarly, there are no lines after `f2 x1`, that need this function's output value.
These functions are just sort of ‘run’. 
Why would you have a function in Haskell that doesn't 
return a value – or more precisely: doesn't return a useful value?



We can translate these actions. Here is another case.


| Case                                             | Translation                                         |
| -------------------------------------------------| ----------------------------------------------------|
| CASE1. `identifier <- <RHS>` *(not on last line)*    | `(λidentifier -> <next line translated>) =<< <RHS>` |
| CASE2.`let x = <RHS>`  *(not on last line)*        | `(λidentifier -> <next line translated>)(<RHS>)`    |
| **CASE3.** **`<expression>`**  *(not on last line)*  | **`(λ_ -> <next line translated>) =<< <expression>`** |
| CASE4. `<expression>`  *(on last line)*              | `<expression>`                                      |

Let's practice translating actions.

----

**Example 1**

Write the following `do` block in terms of lambdas and binds.
You are given that `f1`, `f2` and `f3` are *monadic actions*.

```haskell
do 
  f1 x1
  f2 x2
  f3 x3
```

----

**Example 2**

Translate the code to use binds and lambas. 
```
main = do 
  line <- getLine  
  putStrLn ('a':line)  
  putStrLn ('b':line)
```

*Solution*

Remember that, in order to translate the first putStrLn, 
the associated lambda that has a don’t-care as it’s input. 

```
(λline -> ( λ_ ->  (putStrLn 'b':line)  ) =<< putStrLn ('a':line) ) =<< getLine
```


*Solution*


### The `<<` operator


We introduce a new operator called the then (`<<`) operator. 

```haskell
-- Expression A
f1 x << f2 y

-- Expression B
(\_ -> f2 y) =<< f1 x
```

As we can see, then (`<<`) is chaining `f1` and `f2` together. 
However, **the output value of f2 is ignored** by the left-hand monadic function. 
This value provided by `f1` isn’t fed to `f2`. We say `f2` *ignores* `f1`. 

> **Key Point:** The `<<` operator is exactly like `=<<` except
that the result from one side-effect is ignored and not passed on 
throughout the nested lambda bind structure as is normally the case. 

The following three expressions are equivalent.

```haskell
-- Expression A  
do 
  putStr “Hello”
  putStr “  ” 
  putStr “World”
  putStr “\n” 
  
-- Expression B
(\_ -> (\_ -> (\_ -> putStr “ World ”) =<< putStr “ ” )) =<< putStr “Hello”

-- Expression C 
putStr “Hello” << putStr “  ” << putStr “World” << putStr “\n”
```

As you can see using `<<` is a little bit easier than using `=<<` because it 
saves you having to write explicitly write `(\_ ->` every time you have a monadic action whose result
you want to ignore. The definition of `<<` will behind-the-scenes introduce the `(\_ ->` for you so 
don't have to worry about it!

----

**Example 1**

Given the following lambdas and binds:
```
(\_ -> (\_ -> (\_ -> f4 x4) =<< f3 x3 ) =<< f2 x2) =<< f1 x1
```

Rewrite the expression to three `<<`'s instead of three `=<<`
and hence remove all of the `(\_ -> ...`'s that 
are causing a mess.



### What about Sequencing, Evaluation order, Execution and Time

The similarly related notions of order of evaluation, execution order and time now come into play.
This is a big deal and will be a source of confusion. These are the first things you learn in imperative programming.
While in Haskell, these are one of the last things you learn about the language. 

Each line here *running a function* that 
doesn’t require an input. We can think of functions as actions. 
Rather than thinking of `=<<` solely as a function application operator
(which we been doing so far), we begin to think of `=<<` as also 
acting as a *sequencing* operator.  We begin to have a concept of time and the syntactic sugar 
of the do block gives a suggestion that one thing happens before another. 
Asking when some Haskell is executed is like asking when Haskell evaluates a particular 
expression - which is a difficult question because Haskell uses *lazy evaluation*.
We'll look at this more in 2B when we'll think of =<< as a **sequencing operator**, that doesn't
really care about when the lines are executed, it just puts them into *some* order –– it's up
to the underlying monad to decide the importance of this order.

----

### Exercises


**1.** Which lines of the do-block below are **monadic actions**? 
```
do 
   num1 <- getNumberFromDatabase "./db1"    -- line 1
   num2 <- getNumberFromDatabase "./db2"    -- line 2
   clearCache                               -- line 3
   num1 + num2                              -- line 4
```

**2.** Which lines of the do-block below are **monadic actions**? 

```
do 
   x1 <- f1
   f2
   x2 <- f3
   f4 x3
   f5 x4
   f6 x5
```


**3.** 
The do-block below has got a lot going on! It's got
monadic assignmnets (`<-`), non-monadic assignments (`let`) and
monadic actions (`xx 1`, `yy 1`)!
```
do 
  xx 1
  a <- f
  yy 2
  b <- g
  let c = a+b
  zz c+3
```
Translate the code to use binds and lambas. 
For each line, make sure
to look at the right case in the translation table.

**4.**
Translate the following to use lambdas and binds.
```haskell
do 
  putStr "What is your first name? "
  first <- getLine
  putStr "And your last name? "
  last <- getLine
  let full = first ++ " " ++ last
  putStrLn ("Pleased to meet you, " ++ full ++ "!")
```


----

**8.** Convert the lamba bind expression below to a do-block
```
main = putStr “Hello” << (putStr “  ” << (putStr “World” << putStr “\n”))
     = putStr “Hello” << (putStr “  ” << ((\_ -> putStr “World”) =<< putStr “\n”))
     = putStr “Hello” << ((\_ -> putStr “  ” ) =<< ((\_ -> putStr “World”) =<< putStr “\n”))
     = (\_ -> putStr “Hello” ) =<< ((\_ -> putStr “  ” ) =<< ((\_ -> putStr “World”) =<< putStr “\n”))
```

**9.** For the bind-lambda expression shown below:
```
main =  
    (\line -> ( (putStrLn (“c”:line) << putStrLn (“b”:line)) << putStrLn (“a”:line))  =<< getLine
```

**(a)** replace the then (<<) operators with lambas and bind operators. *Hint: recall that n << m === (\_ -> n) =<< m.*     
**(b)** Using your answer from (a), rewrite the expression so that it uses the do notation.



**10.** Show that the following two expressions are equal. 

```haskell
-- Expression A
do action1
   action2
   action3 

-- Expression B
(do action2
   action3) << action1 
```
Notice the order changes: action1, action2, action3 changes to action2, action3, action1.    
*Hint: find the bind-lambda expressions for both Expression A and Expression B*
