## 3D. Internal State III


### Output vs Next State

In imperative programming languages, there are operations that mutate objects. For example,

```javascript
var xs = ["Hello", "World"];
var output = x.pop();
console.log(output);  // 1
console.log(xs);      // ["Hello"]
```

The `pop` operation has done two things, it is responsible for (i) changing `xs`; saying what is new 
state should be and (ii) providing a return value; in this case it returned the value of 1, the length
of the new list.

Let's look at another style of reduction. In this style, we will again pass around the state, 
but each type a reducer changes the state, it is also *also allowed to provide output value*.
This output value will be the value that is fed into the corresponding lambda
and hence is exposed to all the reducers coming later on in the chain.

We can do the same in Haskell. We'll continue to use the more elaboration version 
of reduction that uses **both the value and the state**. First, let's define a newtype
for a reducer.

```haskell
newtype Reducer s r = Reducer { getReducer :: s -> (r,s) }
```

**Example**    
```haskell
main = do
  let reducerMonad = Reducer $ \(x:xs) -> ([], x)
  let initialState = [1,2,3]
  let reducer = getReducer reducerMonad
  let reduction = reducer initialState
  print $ reduction
```

*Output*    
```
([], 1)
```

So far all we have done is create a wrapping. 
We wrapped up a function, then unwrapped it and pass it an input of `[1,2,3]` then got the output of `([], 1)`.
This is nothing new, just the basics of `newtype` at work.
Now the magic part of reductions comes with the *chaining*.
Let's try chaining two `Reducer`s with a bind. 

The definition of bind is like so:

```haskell
instance Monad (Reducer s) where
    return x = Reducer (\s -> (x,s))
    (Reducer g) >>= f = ...
```

----

Notice that every reducer outputs *two* values, `(r,s)`, not just one. Below explains why.


```haskell
main = do
  let initialState = 0
  --                 └─────┐
  let reducers = Reducer (\x -> (0, x * 2))
        --    ┌──────────────────┘  │
        --    │               ┌─────┘
        >>= (\a -> (Reducer (\x1 -> (1, x + 5))   -- can access a
            --    ┌──────────────────┘    │        -- can only access x1
            --    │               ┌───────┘
            >>= (\b -> (Reducer (\x2 -> (0, x * 20))  -- can access a,b
                --    ┌──────────────────┘    │        -- can only access x2
                --    │               ┌───────┘
                >>= (\c -> (Reducer (\x3 -> (2, x - 2)))  -- can access a,b,c
                )                                          -- can only access x3
            )
        )
  print $ getReducer reducers initialState
```

As you can see one value is there obviously to update the state, but a second value is needed
to go into the lambda's parameter. This is



- **Next State**, `s`. This will *directly* be made available only to the reducer immediately following in the chain.
The next reducer will access via `\x`. This reducer will in-turn say what 
the next state will be by placing another vlaue in its second tuple entry, `s`. This will arrive
in the parameter of the next reducer that'll determine an even newer state, and so on.
- **Output / Result**, `r`. This will be made available to all reducers later in the chain. 
It'll be bound to a lambas, and given that subsequent reducers are in the body of the lambda, they'll be able to access
this value in the style of "scoping" done via nested lambdas/binds. 

Below is a more visual explanation.

```haskell
-- [1]: Take in current state, x and update it to x'. Only the next reducer can access this value.
-- [2]: Output the value r. All subsequent reducers can access this value.

--        ┌────────┌──────────  [1]   
Reducer (\x -> (r, x'))
--               \─────────────  [2]  
```

Of course, you're free to put any arbitary value for `r`, 'x' and `x'` in your reducer,
but usually you're reducer represents some kind of useful operation in the context of your application.
What's interesting is that the value of `r` is avaiable for all the reducers 
that come later in the chain, not just the immediately next one. This should remind you of the scoping use for  `->` 
and lambas and binds.



The `r` value is a bit like a return value or an output value. For example, a stack pop operation, 
a suitable `r` would be the value popped off the stack. All subsequene reducers can 
read this value that has been popped off and act accordingly. 




Often there isn't a suitable value for `r` and so a reducer will use `()`.


> **Key Point**: Every reducer takes the current state as an input, `\x ->`, and then outputs a tuple `(r,x')`,
where `x'` is the new value of the state and `r` is an optional value used to inform the next reducer



*Output*    
```
((),98)
```

The state makes the following changes
```
0 ──[R1]───> 0 ──[R2]───> 5 ──[R3]──> 100 ──[R4]──> 98
```

We can go into more detail by showing the values outputted values by each reducer,
along with their "lifetime"/"scope".

```
      0 ────[R1]──────> 0 ────[R2]──────> 5 ────[R3]──────> 100 ────[R4]───────> 98
a:           \────────>()------│-----------------│-------------------│------------
b:                             └────────> ()-----│-------------------│------------
c:                                               └─────────> ()------│------------
                                                                     └─────────> ()
```
The dotted lines in the diagram ("`--`") are trying to show 
that the value outputted by the first reducer, `a`, is available to R2, R3 and R4.
Likewise, `b` is available to R3, R4 and `c` is available to R4.

We can add one last detail, that being how we get our final value of `((), 98)` --
detailing where the values of `()` and `98` come from.

```
      0 ────[R1]──────> 0 ────[R2]──────> 5 ────[R3]──────> 100 ────[R4]───────> 98────────────────────┐
a:           \────────>()------│-----------------│-------------------│------------                     │
b:                             └────────> ()-----│-------------------│------------                     │
c:                                               └─────────> ()------│------------                     │
                                                                     └─────────> () ──────────────┐    │
                                                                                         result: ((), 98)
```

The last reducer, R4 determines both values for the overall result.
It's output value output taken and the new state value is returned.


> **Key Point**: There is a key difference between the current state `s` and the output / result value `r`.
This difference is about *scope*. The next reducer will be able to access both `a` and `s`.
However, the crucial difference is that `r` is accessible in *all subsequent reducers*, whereas `s`,
is only accessible in the *immediately following reducer*

----

**Example**    

State the output from the following reducer chain.

```haskell
main = do
  let reducers = Reducer (\x -> (200, x)) 
        >>= (\a -> (Reducer (\x -> (a, x))))
  let initialState = [1,2,3,4,5]
  print $ getReducer reducers initialState
```

*Output*    
```
(200,[1,2,3,4,5])
```

----

**Example**    

State the output from the following reducer chain.

```haskell
main = do
  let reducers = Reducer (\(x:xs) -> (200, xs))
        >>= (\a -> (Reducer (\(x:xs) -> (a, xs))))
  let initialState = [1,2,3,4,5]
  print $ getReducer reducers initialState
```

*Output*    
```
(200,[3,4,5])
```

----

If you hadn't guessed already, we can rewrite these Reducer chains using the do-notation.

```haskell
-- Expression A
main = do
  let initialState = 0
  let reducers = Reducer (\x -> (2, x))
        >>= (\a -> (Reducer (\x1 -> (50, x+a))
            >>= (\b -> (Reducer (\x2 -> (400, x+b))
                >>= (\c -> (Reducer (\x3 -> ((), x + c)))
                )
            )
  print $ getReducer reducers initialState

-- Expression B
main = do
  let initialState = 0
  let reducers = do
        a <- Reducer (\x -> (2, x))
        b <- Reducer (\x1 -> (50, x+a))
        c < -Reducer (\x2 -> (400, x+b)
        Reducer (\x3 -> ((), x + c))
  print $ getReducer reducers initialState
```

Notice that we have *two* do-blocks nested within in each other. 
Recall from 3A that do blocks are independent  corresponds to *one* monad. 
Here, outer do block corresponds to the IO monad, 
the inner do-block corresponds to our Reducer monad.

> **Watch out!** 
We have already been using do in the previous examples of this section,
however these do-blocks were for **the `IO` monad**, not for our `Reducer` monad!

```
main = do -- IO Monad
  ...
  let reducers = do  -- Reducer monad
        ...
  print $ getReducer reducers initialState
```

----


Let's work out what the definition of bind should be. Firstly our Reducer
is a monad.

```haskell
instance Monad (Reducer s) where
...
```

For it to be an instance of `Monad`, we are required by the `typeclass` for `Monad`
to give definitions for `return` and `>>=` (which we can compare to to implementing an interface in OOP).


```haskell
instance Monad (Reducer s) where
    return x = ...
    (Reducer f) >>= g = ...
```

So far so good, but now for the hard part, working out the definitions of `return` and `>>=`.
Let's start with `>>=`. The first thing we know is that it'll be a Reducer.

```haskell
(Reducer f) >>= g    =   Reducer ...
```

The first question here is, *what is `g`*? It you look back the pattern
has always been `R_i >>= (\a >>= ...)` So here, `g` must be of the form  `(\a >>= ...)`.
But what is inside the `...`?. Well initially we see it's a complex 
nested bind structure of all the following reducers, `<reducers Ri+1, Ri+2, ... Rn>`.
```
      s ────[(Reducer f)]──────> f_state ───[Ri+1]──> ... ───[Rn]────>   h_state ──────────────┐
a:                \────────────> f_result ----------- ... ------------   h_result ──┐          │
                                                                                    │          │
                                                                       result: (h_result, h_state)
```

But the clever part is that the complex  is equivalent to a single
reducer that performs the work of all of them.
In other words, the `g` its some Lambda that wraps a *single reducer*.

```
      s ────[(Reducer f)]──────> f_state ──────── [(Reducer h)] ────>   h_state ──────────────┐
a:                \────────────> f_result ---------------------------   h_result ──┐          │
                                                                                   │          │
                                                                       result: (h_result, h_state)
```

Now that we have , we'd like to pass it the state.
So the next question isL *how do we get the current state?*. 
The answer is that it's passed to us from the previous reducer.
So let's setup a function for us to accept the state.
```haskell
                                          /---------- add this lambda to get the current state
(Reducer f) >>= g     =     Reducer $ \s -> ...
```

So now `s` will represent the state current before `f` is applied. We want to take the state
outputted from `f` and feed it into `g`. We perform `f s`, this gives `(f_result, f_state) = f s`.
Then we pass `f_result` to the lambda that wraps all subsequent reducers, `g = (\a >>= ...)`
i.e. we write `g f_result` and gives some single reducer, `(Reducer h)`. This single reducer performs
the same state changes as all the subsequent reducers, but in one go.
Finally, we pass to this reducer the state outputted from f, that is we perform `h f_state`. 
This will return a new tuple `(h_result, h_state)`. This is what
the bind should return.

```
(Reducer f) >>= g = Reducer $ \s -> let (f_result, f_state) = f s
                                                (Reducer h) = g f_result
                                                 in  (h f_state)
```



----

**Example**    
Let's us the `Reducer` monad to walk down a binary tree.

```haskell
data Tree = Node Int Tree Tree | Leaf Int
          deriving (Show)

someTree =
  Node 1
    (Node 2
      (Leaf 3)
      (Leaf 4))
    (Node 5
      (Node 6
        (Leaf 7)
        (Leaf 8))
      (Node 9
        (Leaf 10)
        (Leaf 11)))

left (Node x l r) = l
left x = x

right (Node x l r) = r
right x = x

val (Node x l r) = x
val (Leaf x) = x


main = do
  let goleft  = Reducer (\t -> (val t, left t))
  let goright = Reducer (\t -> (val t, right t))
  let initialState = someTree
  let reducers = do
      goright -- 5
      goleft  -- 6
      goright -- 8
  print $ getReducer reducers initialState
```







Also notice that we now have a definition for `return`.
The simplest thing a Reducer can do it to *not modify the state* and output some value.
This is exactly what `return` does. It creates a new Reducer which states some state `s` and returns
the same state along with a value `(x, s)`.

```haskell
main = do
  let r1 = Reducer (\x -> ((), 200:x))  -- prepend 200, output ()
  let r2 = return 1                     -- dont change state, output 1
  let r3 = Reducer (\x -> ((), 300:x))  -- prepend 200, output ()
  let reducers = r1 >>= (\x1 -> (r2 >>= (\x2 -> r3)))
  let initialState = [1,2,3,4,5]
  print $ getReducer reducers initialState
```

*Output*    
We show the result `(x3, s3)` that comes out of `r3`.
```
((),[300,200,1,2,3,4,5])
```
The state hasn't been changed by reducer, `r2` that given back from `return 1`.
The other two reducers, `r1` and `r3` modified the state by preprending `200` and `300` respectively.

----

Let's try using the `do` notation with the State monad. So far we have been using binds and lambdas
but we know from 3A that we can alternatively use the `do` notation.


```haskell
main = do -- <-- do for IO monad (not for the state monad)
  let reducers = Reducer (\x -> (200, x)) >>= (\a -> (Reducer (\x -> (a, x))))
  let initialState = [1,2,3,4,5]
  print $ getReducer reducers initialState
```
Remember that every do block has *exactly one* monad associated with it.
A single do block cannot switch back and forth between say, the `IO` monad and the the `Reducer` monad - 
the monad *stays fixed*. However the values assigned via `let` can be any old value.
These values are not associated the monad of the `do` block and can be anything, even another monad. Above we
have a do block for the IO monad but we are assigning a Reducer monad via let.

**Example**    
Rewrite the following so that the line marked with (*) using a do block.
```haskell
main = do
  let r1 = Reducer (\(x,y) -> ((), (x+200,y)))       -- add 200 to x
  let r2 = Reducer (\(x,y) -> ((), (x+50, y+50)))    -- add 50 to both x and y
  let r3 = Reducer (\(x,y) -> ((), (x, y+300)))      -- add 300 to y
  let reducers = r1 >>= (\a -> (r2 >>= (\b -> r3)))  -- (*)
  let initialState = (1,5)
  print $ getReducer reducers initialState
```

*Solution*    
```haskell
main = do -- do block for IO Monad
  let r1 = Reducer (\(x,y) -> ((), (x+200,y)))     -- add 200 to x
  let r2 = Reducer (\(x,y) -> ((), (x+50, y+50)))  -- add 50 to both x and y
  let r3 = Reducer (\(x,y) -> ((), (x, y+300)))    -- add 300 to y
  let reducers = do -- (*)
      a <- r1
      b <- r2
      r3
  let initialState = (1,5)
  print $ getReducer reducers initialState
```

Notice that we have two nested `do` blocks, one for the IO monad and one for the Reducer monad.

```haskell
main = do -- do block for IO Monad
  let r1 = Reducer (\(x,y) -> ((), (x+200,y)))
  let r2 = Reducer (\(x,y) -> ((), (x+50, y+50)))
  let r3 = Reducer (\(x,y) -> ((), (x, y+300)))
  let reducers = do -- do block for Reducer monad
      a <- r1
      b <- r2
      r3
  let initialState = (1,5)
  print $ getReducer reducers initialState
```
So long as we use `let` and left-arrow `<-` correctly, the two do blocks work together in peace.

----

> **Watch out!** Make sure you use 4 spaces for the inner do block.
If you use 2 spaces you'll get a compilation error `parse error on input`

----

There have been a lot of cases where a 
Reducer (that isn't the first Reducer) 
*doesn't use the output value returned by the previous Reducer*.

Instead of writing `a <- r1` in a do block, we can just write `r1` and it still works.
Here we are treating `r1` like a **monadic action** (see 3D). 

----

**Example**    
Here we implement a stack using the state monad.    
```haskell
main = do -- do block for IO Monad
  let pop = Reducer (\(x:xs) -> (x, xs)
  let push v = Reducer (\xs -> ((), v:xs))
  let reducers = do -- do block for Reducer monad
      push 1
      push 2
      push 3
      pop
      pop
      pop
  let initialState = (1,5)
  print $ getReducer reducers initialState
```




----


**Example**    
**Tennis.** Let's use the State monad to hold the basic state of a tennis game.

```haskell
addScore :: Int -> Int
addScore 0 = 15
addScore 15 = 30
addScore 30 = 40
addScore _ = 0

main = do
  let scoreP1 = Reducer (\(x,y) -> ((), (addScore x, y)))
  let scoreP2 = Reducer (\(x,y) -> ((), (x, addScore y)))
  let points = (0,0)
  let game = do
      scoreP1
      scoreP1
      scoreP1
      scoreP2
  print $ getReducer game points
```

This can be extended to handle the *advantage scoring system*.
When both players have 40, when the next point is won, the *game is not over*.
Instead, the player has a score is *(adv, 40)* or *(40, adv)*. 
The player who scored *has the advantage*. 
In this situation:    
-  If the player scores again he/she wins.
-  If the other player scores , the score is reset to *(40, 40)*

Below shows an example implementation:

```haskell
type Score = (String, String)
addScoreP1 :: Score -> Score
addScoreP1 ("adv", "40") = ("winner", "loser")
addScoreP1 ("40", "adv") = ("40", "40")
addScoreP1 ("40", "40") = ("adv", "40")
addScoreP1 ("40", x) = ("winner", "loser")
addScoreP1 ("30", x) = ("40", x)
addScoreP1 ("15", x) = ("30", x)
addScoreP1 ("0", x) = ("15", x)
addScoreP1 x = x

addScoreP2 :: Score -> Score
addScoreP2 = swap . addScoreP1 . swap

main = do
  let scoreP1 = Reducer (\score -> ((), addScoreP1 score))
  let scoreP2 = Reducer (\score -> ((), addScoreP2 score))
  let initialScore = ("0", "0")
  let game = do
      scoreP1 -- (15, 0)
      scoreP1 -- (30, 0)
      scoreP1 -- (40, 0)
      scoreP2 -- (40, 15)
      scoreP2 -- (40, 30)
      scoreP2 -- (40, 40)
      scoreP1 -- (adv, 40)
      scoreP2 -- (40, 40)
      scoreP2 -- (40, adv)
      scoreP1 -- (40, 40)
      scoreP1 -- (adv, 40)
      scoreP1 -- (winner, loser)
  print $ getReducer game initialScore
```

And just for fun, here is a classic game between famous tennis players X and Y 
written using our `Reducer` monad!


----

**Exercises**    

1. **Newton-Raphson and sqrt(2)** The [Newton-Raphson algorithm](https://en.wikipedia.org/wiki/Newton%27s_method) can be 
used to evaluate `sqrt(2)`. The technique 
iteratively get's closer and closer to the real answer. Each step
provides an *approximation* and converges closer to the real answer.
In this exercise, you'll explore how `Reducer` to show the stepwise approximations that Newton-Raphson algorithm produce.

```haskell
-- special Newton-Raphson functions for
-- finding the approximation for sqrt(2)
f  x = x*x - 2
f' x = 2*x

-- a single iteration in newton raphson
step = Reducer (\x -> (next x, next x))
       where next x = x - f(x)/f'(x)

main = do
  let initialApprox = 1
  let newtonRaphson = do
      step -- (1.5, 1.5)
      step -- (1.4166666666666667,1.4166666666666667)
      step -- (1.4142156862745099,1.4142156862745099)
      step -- (1.4142135623746899,1.4142135623746899)
  print $ getReducer newtonRaphson initialApprox
```
