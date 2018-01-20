## 2E. Nested Do Blocks 

In this chapter we'll explore **do-blocks within do-blocks**.
Wait, hang on. *"Do-blocks inside do-blocks"* ... is that even possible?! 
The answer is: **Yes** it *is* possible ... but *how*?
Well, remember that `do` blocks are merely a nesting of lambdas so do blocks are just an *expression*. 
Literally, do blocks are equivalent do a sequence of lambdas and binds. They have the same type
and when executed, they evaluate to the same value.

I'm sure we're all familiar with expressions,
but to drill the point home, let's take a look at some examples of expressions.


```haskell
-- Expression 1
(1 + 2) * 5

-- Expression 2
\x = x + 1

-- Expression 3
(flip add) 10 20

-- Expression 4
do =
   x <- f
   y <- g
   x + y
```

Just as mathematical operators, functions declarations, function calls are all expressions, 
so too are do-blocks! Remember, anywhere where you can write 1+1, you can write do-block.
This includes (but is not limited to):
- on the right hand side of =
- as an argument to a function
- on the right hand side of a let in a do-block

Don't believe me? See for yourself:

```
-- Right-hand side of an =


-- Argument to a function


-- Right-hand side of a let in a do-block


```

The theme of the Chapter 2 was: *"every do-block is a lambda-bind expression"*.
So what's the theme of this chapter? Well Chapter 3 is all about: *do-blocks are just expressions*.

The result of evaluating a do-block is a monadic value.
So the above case work so long as the compiler expects there to be a monadic 
value in the aforementioned place.


> **Key Point**: A do block can appear anywhere where an ordinary haskell expression can appear.

Let's practice translating *nested* do-blocks.

----

**Exercise 2**

```
do
    match "images/*" (do
       route   idRoute
       compile copyCompiler))
```

*Solution*

Let's work inside-out. We’ll translate the inner do block then translate the outer do block.
```
<inner do block> = 
         (λ_ -> compile copyCompiler) =<< route IdRoute
<outer do block> = 
        match "images/*" ( <inner do block> )
```

Now let's put everything together:
```
match "images/*" ((λ_ -> compile copyCompiler) =<< route IdRoute)
```

### Nested do blocks via `let`



> **Key Point**: you can assign a do block to a let
```haskell
do
  <outer>
  <outer>
  let foo = do
      <inner>
      <inner>
      <inner>
  <outer>
```
We call these *let do blocks*.


### Adding a parameter to `let` do blocks

What if we add a parameter, x to a let do block?

```
main = do
  let talk x = do
      putStrLn x
  talk "hello"
```

Rather interestingly, this compiles.

```
$ runhaskell 
hello
```

What is the resulting lambda/bind expression? 
If we try to translate the outer do block in the way we are used to, we have:

```
main = (λtalk -> talk “world”)(<inner do block translated>)
```

A question arises of how to handle that `x` in the `talk x`.  
That x will be in scope for all expressions inside the inner do block. 
This gives us the hint that we need a λx , 
and that it needs to surround the bind-lambda expression corresponding to that inner do block. 
Indeed, the translation we are looking for is:

```
main = (λtalk -> talk “world”)(λx -> inner do block)
```

Here are using functions as values. We are using to passing in strings, 
numbers as values to a lambda. But there’s nothing stopping us passing in a lambda to a lambda. 
The value of talk will be the function (λx -> inner do block). If we run this code, we get the same result:

```
main = do
  let talk x = do
      putStrLn x
  talk "hello"
```

*Output*
```
hello
```

Now that we have talk, we can ‘call it’ several times. 

```
main = do
  let talk x = do
      putStrLn x
  talk "hello"
  talk "world"
  talk "this"
  talk "is"
  talk "james"
```

*Output*

```
hello
world
this
is
james
```

We have just discovered that do blocks can be parameterized. This is similar to procedural programming languages like Javascript and Ruby which are ‘functional’ in the loose sense of having ‘blocks of imperative code’ that are first class citizens. We can roughly compare Haskell’s do blocks to Javascript’s functions or Ruby’s blocks, the important difference being of course that Haskell do blocks are pure functional; any side-effects are encapsulated in a monad.


> **Key Point** We can add parameters to let do blocks. 
```
do  
  … 
  let foo x = do
      … 
  … 
This allows us to quickly reuse monadic code
```

We can introduce as many parameters as we like - we don’t just have to use one!
To see this, look at example below where we have a let do-block with *two* parameters.

```
main = do
  let log x y = do
      putStrLn ("Welcome to: " ++ x)
      putStrLn ("The time is: " ++ y)
      putStrLn ("---\n")
  log "Home" "8pm"
  log "Work" "12pm"
```
*Output*

```
Welcome to: Home
The time is: 8pm
---
Welcome to: Work
The time is: 12pm
---
```

How does this effect the equivalent lambda bind expression?
We can think of this as translating to a *multi-argument lambda*.

```
main = (λlog -> ((λ_ -> log “Work” “12pm”) =<< log “Home” “8pm”))
       (λx y -> <inner do block>)
```

Of course in Haskell, thanks to Currying, there is no such thing
as multiargument lambdas (or multiargument functions of any kind!).
`(λx y -> <inner do block>)` is the same as 
`(λx -> (λy -> inner do block))`.


> **Key Point** A do block can take multiple parameters 
```
do  
  … 
  let foo x y z = do
      … 
  … 
```



### Scope and nested do blocks


### Nested do blocks via `<-`




### Nested do blocks at the end



### Indentation Rules

When nesting do-blocks, you need to be careful with indentation.

> **Key Point** Use 4 spaces for nested do blocks

https://en.wikibooks.org/wiki/Haskell/Indentation#Layout_in_action


These rules should not be confused with the so-called [*offside rule*](http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html#deftypes.offside).
The offside rule is not mandatory, these identation rules for do blocks
are mandatory. 

### Extraneous nested `do` Blocks


One thing to watch out for is **extraneous do-blocks**.
If you're not careful, you can end up adding a do-block where it is not needed.

```haskell
main = do
  let foo x = do
      if (x == 3)
      then do putStrLn "magic"
      else do putStrLn "boring"
  foo 1
  foo 2
  foo 3
```

```haskell
main = do
  let foo x = do
      if (x == 3)
      then do do do do putStrLn "magic"
      else do do do do putStrLn "boring"
  foo 1
  foo 2
  foo 3
```

These extra do block are trivial in sense that they contain just a *single line*.
A do-block with one line doesn't do anything. Recall the translation table.


When you are working with nested do blocks, you need to be wary of adding unnecessary do blocks. 
If you unsure, try without the do block first. 
- If it works, think: *why does this work?*. 
- It it doesn’t work, try adding a do block and think: *why didn’t it work; why do I need to add this do block?*. 

