## 2A. Using =<< for Function Application

### Thinking about Boxes


- **Boxed Value**



### A High-level Definition of =<<


The =<< operator is called **bind**. This is an operator that only works with so-called **monads**. 
Its type signature is the following. 

```haskell
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

Given that =<< is an *infix* operator, we can imagine the `=<<` sitting the middle
in between its two arguments. In our case, it sits between a special function 
on the left and a special value on the right. To let's pretend for a second that 
we're allowed to rewrite our type signature like:

```haskell
(a -> m b) =<<  m a  =  m b
```

What bind does is take what is on the right operand, `m a` and 
*push into through function on the left*, `(a -> m b)`. 
So bind is a special kind of function application. What is rather
peculiar is the placement of the `m`'s in the above type signature.
The function on the left accepts an `a` as its argument, but our input is a `m a`.
What we need to do is unwrap `m a` first before passing it through the function.
Monads are like newtypes in that they can be unwrapped. 
So the bind operator performs two steps:

1. **Unwrap** Bind takes the RHS: `m a`, strips off the m to get: `a`, 
2. **Apply** It applies the function on the LHS to give m b. 


To summarise,

- `f x`. This is ***normal* function application**.    
   The operator is the space sitting between `f` and `x`.    
   This operation performs function application for normal values.
   ```
   (input as a normal value -> output as a normal value) input as a normal value 
   = output as a normal value
   ```
- `f =<< x`.  This is a ***monadic* function application**.    
    The operator is `=<<` sitting between `f` and `x`.    
    This operation performs function application for 'boxed values'
    ```
    (input as a normal value -> output as a fancy value) =<<  input as a boxed value 
    = output as a boxed value
    ```
    Unlike normal function application, we going to set things up so that a side-effect can 
    occur during this process of unwrapping and applying.

So bind (`=<<`) is nothing more than function application with a bit of unwrapping done before-hand.   
LYAH gives a nice intuitive explanation of the bind operator: 
> If we have a fancy value and a function that takes a normal value but returns a fancy value, 
how do we feed that fancy value into the function?” Answer: use the bind operator.**

----

**Example**

Let's look at harder example.
Here is an example of a real monad taken from a real Haskell library called Hakyll.
`module Hakyll.Core.Compiler.Internal`.
This library defines two functions:
```
loadAll        :: Pattern -> Compiler [Item a]
recentFirst :: [Item a] -> Compiler [Item a]
```

For the monadic function application:
`recentFirst =<< loadAll "posts/*"`
explain, in terms of the LYAH description, what is ...
1. the type of the input is as a fancy value is?
2. the type of the input is as a normal value?
3. the monadic function?
4. the type of the monadic function?
5. the type of output is as a fancy value?

*Solution*

1. The input as a fancy value is `Compiler [Item a]`
2. The input as a normal value is `[Item a]`.
3. The monadic function is `recentFirst`
4. The type of the monadic function is: `[Item a] -> Compiler [Item b]`
5. The output as a fancy value is `Compiler [Item b]`

----

Whenever you see =<< in an expression, try to parse the expression and determine 
which terms correspond to (i) input as fancy value, 
(ii) input are normal value (iii) function in question, and (iv) output as fancy value

> **Key Point** Whenever you see `=<<`, there will definitely be a monadic function next to it.
The monadic function is different because it takes normal value and give out a fancy value. 


----

### Aside: Infix Operators

It just so happens that bind `=<<` is a so-called **infix operator**. It's similar to the likes of
`-`, `+`, `/`, `*`, etc... An infix operator is a special symbol `<symbol>` that sits between
two expressions `<expression1> <symbol> <expression1>` and we use it to compute an output value
where `<expression1>` and `<expression2>` are the input values. 
So it's similar to a function that accepts two parameters, `f : l -> r -> o`,
the left expression has type `l`, the expression argument has type `r` and the output of the operator
has type `o`. 

It turns our that in Haskell you can take any function like `f : l -> r -> o`, specify your
custom symbol and then convert it to an infix operator! Haskell makes it really easy to do this.
Magical! And guess what? This is exactly what happened to `=<<`.
So instead of using `=<<`, we can imagine there is a function called `bind` that when it's called
we use the more common (prefix) way of calling the function: `bind lambda boxed_value`.


### A Lack of Intuition 

We have explained very roughly how `=<<` works via it's type signature, 
but you're probably not feeling any intuition for this at all!
Don't worry this is normal. Before giving an intuition, let's just make sure we're on the same 
page about why our understanding is lacking.
At least for me, when I first discovered this definition,
one the most confusing parts of this definition is the function's signature: `(a -> m b)`.
Why was *this* signiture chosen?!

**Mystery 1: The strange placements of the `m`s:**
It's unclear why this particular setup of boxes and unboxing supposedly "works" for monads (whatever they are!). 
Why is it that the function must return a boxed value? Why is it that the function must 
accept an unboxed value? Both unanswered questions.
In fact our of the four combinations below, only one will work for us:

- `a -> b`
- `a -> m b`  ← this placement of the `m`s was chosen, ... why?
- `m a -> b`
- `m a -> m b`

**Mystery 2: The need for *two* types `a` and `b`:**

A function in general has 

- `m a -> a`
- `m a -> b` ← we need an extra type `b` here ... why?

**Mystery 3: Why do we have boxes in the first place?**



Some things to bear in mind.

- **=<< actual has a body, we've only looked at its type**
- **The body of =<< changes depending on the monad**
- **The body of =<< might call compiler code**
- **The given monadic function (a -> m b) will only ever work with one type of monad**
- **The type system is smart out find the correct definition of =<< right there and then**

### Exercises

**1.** True or false:     
(a) the bind operator (=<<) is a binary operator    
(b) the bind operator (=<<) is symmetric, in the sense that a =<< b is the same as b =<< a    
(c) the bind operator (=<<) must be provided with a function and must apply it to some value    

**2.** Take the monadic function application in its most general form:
```
f =<< x
```
Which is of the following type signatures are result in a valid usage of bind:     
(a) `f :: (FileDescriptor -> Maybe String)`, `x :: Maybe FileDescriptor`      
(b) `f :: (FileDescriptor -> String)`, `x :: Maybe FileDescriptor`       
(c) `f :: (Int -> M Int)`, `x :: Int`       
(d) `f :: (Int -> M Bool)`, `x :: M Int`     
(e) `f :: ((Int,Int) -> M Int)`, `x :: M (Int,Int)` 

**3.** For each, state what (i) the input is as a fancy value is, (ii) the input is as a normal value (iii) the function in question is (iv) the output is as a fancy value:
(a) `(\x -> y) =<< X`
