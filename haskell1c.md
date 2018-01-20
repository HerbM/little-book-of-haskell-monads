## 1C. Using Let in a Do Block

In this chapter, we'll focus on the arrows (`<-`), or ‘assignments’ of a do-block.
In particular, our attention will be drawn to the *right-hand sides* of these assignments.
The first thing to note is that they must be *functions*. 

Consider the assignments in this do-block:
``` haskell
do 
  x1 <- f1
  x2 <- f2
  x3 <- f3
  c x1 x2 x3
```

In order for the above to compile, it is required that
`f1`, `f2`, `f3` are *functions*. Furthermore, and rather crucially, 
they must be **monadic functions** - that is, each function 
must have type `a -> m b`, where `m` is a **monad** [1].    
Why is this?

Well from 3A, we know that at the above do-block is equivalent to:
```haskell
(λx1 → (λx2 → (λx3 → (c x1 x2 x3)) =<< f3) =<< f2) =<< f1
```

We can see that those three 'assignments' correspond to three bind operations. 
And we can also see that the right-hand sides of arrows (<-) are the right-hand sides of a bind! 
From 2A, we know that every right hand side of a bind must be a monadic function.
Hence every right-hand side of an arrow must be a monadic function.

> **Key Point**: Given an arrow of a do-block, `x1 <- <rhs>`, the right-hand side, `<rhs>`, must be a **monadic function**. 

So the following *does not* compile!

``` haskell
do 
  val1 <- 1            -- Wrong! See paragraph below for reason.
  val2 <- 2            -- Wrong! See paragraph below for reason.
  val3 <- (val1+val2)  -- Wrong! See paragraph below for reason.
  print val3
```

The RHSs of `val1`, `val2` and `val3` are not monadic functions -- they’re not even functions! 
This reveals an important difference between the ‘assignments’ in Haskell do blocks,
and actual assignments you see in imperative languages like Python, 
Java and C++. Haskell ‘assignments’ **cannot accept any old right-hand side!**
-- whereas you might see a number, boolean or string on the right-hand side of a Java assignment, you'd
*never* see one on the right-hand side of a Haskell arrow.
Scary! We can’t even put numbers on the right hand side! 
This indeed makes the use of `<-` seem rather limited. 

Thankfully, to solve this problem, the Haskell language designers gave us **new operator** (or more precisely *syntax*)
called `let`. With this new operator, `let`, the above can correctly be written as:

```haskell
main = do
  let val1 = 1
  let val2 = 2
  let val3 = (val1 + val2)
  print val3
```

This program will now correctly compile. 

> **Key Point**: `<-` performs a *monadic 'assignment'*. `let` is a new operator that perfoms ***non-monadic* 'assignment'**

So `let` and `<-` are pretty much the same. 
The key difference concerns that the type of the RHS.
In `let`, the RHS is **not monadic** (a plain old value); in `(<-)` the value on the RHS of <- is *monadic*. 



> **Watch Out!** Before we move on, let me tell you that `let` in the context 
of a do-block is a *completely new operator*. Do not confuse this new operator with the 
familiar (`let` .. `in`) that is used in beginner Haskell. 
Although both features use the keyword `let`, they work in completely different ways!
We'll see that the `let` in a do-block introduces a special kind of lambda in a lambda-bind nesting.
The `let` as seen (`let` .. `in`) merely performs a substitution. Besides sharing
the keyword `let`, the two features **share no relation**! Remember to consider them unrelated. 


----

So, *how does this work*? What exactly is this so-called `let` operator doing?
These two questions are answered by showing what this `let` translates to in terms of lambas and binds. 
A do-block will always translate to a nesting of lambas and binds and the addition of our new operator is not
going to break that rule.


| Case                                           | Translation                                         |
| -----------------------------------------------| ----------------------------------------------------|
| 1. `identifier <- <RHS>` *(not on last line)*  | `(λidentifier -> <next line translated>) =<< <RHS>` |
| **2.** **`let x = <RHS>`**  *(not on last line)*       | **`(λidentifier -> <next line translated>)(<RHS>)`** |
| 3. `<expression>`  *(on last line)*            | `<expression>`                                      |


We introduce a new translation rule (see 2. in the table above).
The translation for `let` is very similar to the rule for `(<-)`. Can you see the difference?
With (`<-`), the RHS is *monadic* so we need to introduce a bind. 
But with `let`, the value is *not monadic* so no **bind is needed**. 
Instead of using `=<<` to pass the value into the lambda, we directly plain-old **function application**.
Besides this difference, the two operators are *exactly the same* in how they both introduce a lambda and how they 
both use the next line of the do statement as the body of this introduced lambda.

> **Key Point**: To translate: `let x = <RHS>` inside a do block, 
write `(λx → ...)(<RHS>)` where the ellipsis, …, is to be filled with the next line of the do block

Let’s look at some more examples and get some practice desugaring the do-block `let` statement. 

----

**Example**    
Consider the original do-block that is correctly written with `let`.
```haskell
main = do
  let val1 = 1
  let val2 = 2
  let val3 = (val1 + val2)
  print val3
```
Translate the block to use binds and lambas. 

*Solution:*    
```haskell
(λval1 → (λval2 → (λval3 → (print val3))(val1 + val2))(2))(1) 
```

----

**Example**     
Rewrite the code below using binds and lambas. 
```haskell
main = do
  n <- readLn
  let m = preprocess n
  process n m
```

*Solution:*    
```haskell
(λn → (λm → (process n m))( preprocess n )) =<< readLn 
```

----

### Exercises


**1.**  You are given the do block.
```
 do
  let n = readLn
  process n
```

You are given that `readLn` is a *monadic function*. Explain why this do block will not compile.
Rewrite it so that it compiles.


**2.** Express as a nesting of lambdas and binds.
```
do
  val1 <- readLn
  val2 <- readLn
  let sum = add val1 val2
  print sum
```


**3.** Below is a do block that contains both monadic assignments `<-` and non-monadic assignments via let.
```
main = do
  putStrLn "Greetings once again.  What is your name?"
  inpStr <- getLine
  let outStr = name2reply inpStr
  putStrLn outStr
```
Find an equivalent expression that is a nesting of lambas and bind operators.

*Note this is real Haskell taken directly from RWHS: http://book.realworldhaskell.org/read/io.html*


**4.**  Express the following do-notation (that includes lines with `let`) in terms of lambdas and binds.
```haskell
nameDo = do 
  putStr "What is your first name? "
  first <- getLine
  putStr "And your last name? "
  last <- getLine
  let full = first ++ " " ++ last
  putStrLn ("Pleased to meet you, " ++ full ++ "!")
```


**5.** A let block is useful for extracting out variables
to help improve the [readability](http://wiki.c2.com/?IntroduceExplainingVariable) of your code.
Below shows a do block that is computing the Body Mass Index (BMI).

```haskell
do
  mass <- readMass
  height <- readHeight
  writeBmi(703 * mass / (height * height))
```

The formala being passed to `writeBmi` is a little long.

**(a)** Complete the code below to use a `let` to 
extract our this formula to a variable called `bmi`. 

```haskell
do
  mass <- readMass
  height <- readHeight
  ...
  writeBmi(bmi)
```
  
**(b)** Take this new `do`-block and rewrite it as a nesting of lambdas and binds.  



**7. [hard]** The following program that uses the IO monad doesn't compile.
```haskell
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   linesOfFiles <- lines content
```
Explain which line is incorrect and give the correct version.
(*Adapted from a [Stack Overflow question](http://stackoverflow.com/questions/11022163/haskell-read-lines-of-file)*)
