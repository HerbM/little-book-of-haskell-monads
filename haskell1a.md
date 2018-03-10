## 1A. What *are* `do`-blocks?

### Introducing `do`-blocks


Now we'll look at so-called **`do` blocks** (**do notation**, **do syntax**).    
This syntax also introduces the so-called **left arrow syntax**, `<-`.    

Below shows an example:

```haskell
do x1 <- f1
   x2 <- f2
   f3 x1 x2
```

The first impression is that the do notation seemingly allows you to write *imperative* programs in Haskell!
When you read this, it seems to be saying: do `x1 <- f1`, then do `x2 <- f2`, then do `f3 x1 x2`... a *sequence of steps*! 
But this is not actually the case. Rather sneakily, the do notation is a big disguise!
The do-notation is merely *syntactic sugar*.
For instance, the following two expressions (Expression A and Expression B) are *equivalent*!

```haskell
-- Expression A
do x1 <- f1
   x2 <- f2
   f3 x1 x2

-- Expression B
(λx1 → ((λx2 → f3 x1 x2) =<< f2)) =<< f1
```

The `(λx1 → ... )` is called a lambda function, you've probably seen this before. The strange
operator `=<<` is called bind. The do notation is syntactic sugar 
for a *series of nested binds and lambas nested*! 
We'll eventually see this these expressions are very powerful as 
they allow us to work with side-effects really nicely.
The theme of this section is that
writing the do-block is no different to writing nesting of lambdas and binds.  

> **Key Point**: The **do notation** is simply *syntactic sugar*. 
Everything written with the do notation can be  written using binds and lambdas. 

So if they're the same, why do we even need `do` blocks?
Well, the answer is simple: `do` blocks are *more convenient to use*.
The Haskell language could have just stuck with pure binds and lambdas but language designers realised 
that programmers would end-up pulling their hair out!
Nested lambdas and binds don't scale well. For as we write longer and longer expressions, we'll use more and more
binds and the whole expression very quickly becomes cumbersome to work with. There are lots of brackets
to work with and figuring out "what's inside what" becomes tricky.

`do` blocks are kinder to our brains and our monitors because they scale *vertically* rather than *horizontally*.

```haskell
-- Expression A
-- Unfortunately, lambdas and binds scale horizontally!
(λx1 -> ( (λx2 -> ( (λx3 -> ( (λx4 ->  ( (λx5 -> ( (λx6 -> y )  =<< f6 ))  =<< f5 )) =<< f4 )) =<<  f3 ))  =<< f2) )) =<< f1

-- Expression B
-- Even if we break them into new lines, we get an indentation triangle of doom!
(λx1 -> ( 
    (λx2 -> (
        (λx3 -> ( 
            (λx4 ->  (
                (λx5 ->  (
                    (λx6 -> y)  =<< f6 
                )) =<< f5
             )) =<< f4
        )) =<<  f3
    ))  =<< f2
)) =<< f1

-- Expression C
-- do blocks are a new syntax that makes it all easier to read!
do x1 <- f1
   x2 <- f2
   x3 <- f3
   x4 <- f4
   x5 <- f5
   x6 <- f6
   y
```
All three of the above expressions (Expressions A, B and C) are equivalent.
Lambas-and-binds tend to scale horizontally ... and even if we try to break them out onto new lines,
we'll still need to ident them to make them clear to read.
If we're going to write serious programs with lambas and binds,
we need something simpler to express what we're doing – almost like a *domain-specific* language (DSL).
Instead of us having to make our own DSL, Haskell has given us some *syntactic sugar* baked into the 
language that sits on top of binds and lambdas. This syntactic sugar is the do-notation.

The keyword `do` and the lines make up a so-called do-*block*.
The syntax of the simplest of do blocks looks like:
```haskell
do
  identifier1 <- rhs1
  identifier2 <- rhs2
  identifier3 <- rhs3
  ...
  identifiern <- rhsn
  expression
```

where:
- `identifier` are any valid identifier (we'll typically stick to words in lowercase)
- `rhs` is any expression
- `expression` is any expression

### Translating do blocks

From the previous discussion, every do block can be written instead using binds and lambdas. 
Understanding monads can be decomposed into three parallel upward struggles:    

1. Translating do blocks into binds and lambdas *(This Chapter)*
2. Understanding what bind is and why we need it *(Chapter 2)*
3. Writing out own monads *(Chapter 3)*  


To get a better feel for how monads work, I can't stress enough that
a good understanding of how do blocks translate to lambdas and binds is *necessary*. 
So let's practice translating! To translate a do block in our heads, 
we look the do block, and translate *one line at a time*. 
For each line, we imagine "pattern-matching" that line to one the following two cases:

| Case                                                   | Translation                                         |
| -------------------------------------------------------| ----------------------------------------------------|
| **CASE 1.** `identifier <- <rhs>` *(must not be on last line)*  | `(λidentifier -> <next line translated>) =<< <RHS>` |
| **CASE 2.** `<expression>`  *(on last line)*                    | `<expression>`                                      |


Let's see how it works.

----
**Example 1**    
Convert the following expression from do notation to **nested binds and lambdas**.
```haskell
do
    stat <- getFileStatus path
    return (fileSize stat)
```

*Answer:*
```haskell
(\stat -> return (fileSize stat)) =<< getFileStatus path
```

*Explanation:*    
We take the do-block line-by-line. There are two lines.
The first line pattern matches case 1, so the result is [A]. 
Now, we need to translate the next line
and put it inside our placeholder. However, the next line is in fact the *last* line, 
and so pattern matches case 2. So the translation is trivial,
is second line translates to itself! The result is [B].

```haskell
(λstat -> <next line translated>) =<< getFileStatus path -- [A]
(λstat -> return (fileSize stat)) =<< getFileStatus path -- [B]
```

---

We can summarise this translation algorithm with the following pseudo-code.

```python
# -- Translates a do-block to a nested lambda-bind expression (pseudo-code)
def translate(line:lines):
   match line:
       case (indenfier <- rhs) => return (λidentifier -> translate(lines))
       case (expression) => return expression
```

----

With the introduction of the do block, 
one question that might to crop up is: *have we been writing* **imperative programs** *all along*? 
The answer is: **No we haven’t**. Let’s see why. If we take that same do block...
```haskell
do x1 <- f1
   x2 <- f2
   f3 x1 x2
```
... and translate it, we get:  `(λx1 →  ((λx2 → f3 x1 x2)  =<< f2)) =<< f1`.
Now, those first two so-called 'assignments' of the do block correspond to 
two bind operations in the lambda-bind translation. This is what Haskell sees
and what Haskell will execute. So we can see that this form of computation 
is not actually imperative. We are not mutating variables in the imperative sense. We are not 
running a program step-by-step. We are just evaluating a bunch of nested functions.

Although the do notation makes it look like we are doing things in a stepwise manner, 
the real ‘steps’ really evaluating each bind in the nesting of lambdas. Instead of working
top-to-bottom, we're working from the outside moving towards the inside. When the Haskell
compiler actually tries to evaluate this nested expressions, because of laziness it's actually
unclear which parts are evaluated when – but in most cases it'll be inside-out.

### What is the `<-` all about?


Notice that we did not give a concrete definition for what `<-` means or does.
You might ask, *is this an operator?*, *is a function?*, *what types does it accept?*
These are always good question to ask in Haskell when you see a funny looking symbol, but here is a rare occation
where these questions are not so applicable. For now, you should for now treat `<-` as just *syntax*
just like `do` keyword is a bit of syntax. The `<-` can only appear in a do block 
and can't be used anywhere else. This is no function definition for `<-` in the Haskell library.
It's only "behaviour" is to separate elements in the sugared abstract syntax tree for a `do` block. 
Think of it as merely a delimeter - like a full-stop in a sentence. It's there to look pretty for us
and to help the GHC's compiler build an AST. 

> **Key Point** There is no function definition for `<-`. `<-` is as merely *syntax* (like the `do` keyword) that helps the compiler build its syntax tree – and to make do blocks nicer for us to read


### Where Now?


#### Recursion and do-blocks

The algorithm to convert to lambda bind is recursive.

#### Any expression, `<expr>`, allowed? 

The `<-` as a delimiter is separating two things:
1. an `<identifier>`, on the LHS, from, ...
2. an `<expr>` on the RHS. 

The way we've written `<expr>` suggests that any *arbitrary* expression, `<expr>` can appear the RHS.
This is fine for understanding the basics, but it turns out that semantically not just anything 
can appear on either side. Later in section 2C (when we introduce the `let` sugar),
we'll see that there are important consequences  for what `<expr>` can actually be. 
More precisely, we'll see that the *type* of `<expr>` 
will have some special rules in order to for the whole do block to correctly type check.


#### You can't use `<-` on the last line

Note that if you try to write a do-block with an `<-` on the last line, this
will result in a compilation error.

```haskell
do x1 <- f1
   x2 <- f2
   x3 <- f3  -- not allowed, can't use <- on the last line of a do-block!
```

Rightly, so as when we try to translate using our translation table, there is no case matching
the last line. If you try to compile this code, the compiler will give you an error message like:

```
The last statement in a 'do' block must be an expression
```

And it you think about it more this makes sense. Our definition is recursive and 
`<-` is like the inductive case of our recursion. Each time we reach a `identifier <- <RHS>` line
we know we're not done yet, because there's needs to be a line after so that we can nest
the translation of that line inside. If there is no next line, then
when we take `(λidentifier -> <next line translated>) =<< <RHS>`, 
we haven't got anything to fill `<next line translated>` with and our recursion breaks. 

----
### Exercises

To get more comfortable with the do notation, I highly recommend practicing the translation between 
lambas and binds and do notation. Let’s get some practice sugaring and 
desugaring using the do notation.

**1.** Convert the following `do`-block to lambdas and binds. 
```haskell
do dog <- foo
   cat <- bar
   chase dog cat
```


**2.** Convert the following `do`-blocks to lambdas and binds. 

```haskell
-- (a)
do x <- ff
   y <- gg
   z <- hh
   x + y + z

-- (b)
do firstName <- readFromStdIn
   lastName <- readFromStdIn
   writeToStdOut (firstName ++ lastName)
   
-- (c)
do 
   connection1 <- connectToDatabase cluster1 config
   connection2 <- connectToDatabase cluster2 config
   documentsAdded1 <- writeDocument(connection1, doc1)
   documentsAdded2 <- writeDocument(connection2, doc2)
   pure (documentsAdded1, documentsAdded2)
```

**3.** Explain why the following *is not* a valid `do`-block.

```haskell
do x <- z
```

**4.** Explain why the following *is* a valid `do`-block.

```haskell
do x
```

**5.** Rewrite the following lambas and binds as a single `do`-block

```haskell
(\x -> (\y -> (\z -> e(x,y,z) ) =<< vv) =<< uu) =<< tt
```

**6.** Rewrite the following lambas and binds as a single `do`-block

```
(\x -> (\y -> (\z -> e(x,y) ) =<< h(y)) =<< g(x)) =<< f
```

**7. [hard]** What's wrong with the `g(z)` in the following expression?
Explain why doesn't `h(y)` doesn't suffer from the same problem.
(Hint: think about variable scoping).
```
(\x -> (\y -> (\z -> e(x,y) ) =<< h(y)) =<< g(z)) =<< f
```


