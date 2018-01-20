## 5B. Creating Monad Transformer Stacks

### Building Monad Stacks

The available transformers are:

- `Control.Monad.Trans.Maybe` for `MaybeT`
- `Control.Monad.Trans.Reader` for `ReaderT`
- `Control.Monad.Trans.Writer` for `WriterT`
- `Control.Monad.Trans.State` for `StateT`
- `Control.Monad.Trans.Id` for `IdT`
- `Control.Monad.Trans.Except` for `ExceptT` (informally called ErrorT)


Building monad stacks can get a li􏰁ttle confusing un􏰀til you know the pa􏰁tterns. 
The first type parameter to a monad transformer 
is the *outer* monad in the stack — the transformer itself 
provides the *inner* monad. 
For example, the `MaybeIO` type above was built using `MaybeT[IO,a]`
but the result was effec􏰀tively a `IO[Maybe[a]]`. 
In other words, we build monad stacks from the *inside out*.
The stack composes two monads and the bind operation 
cuts through two layers of abstrac􏰀tion, and always manipulates the innermost value.
In our case, bind cuts through `IO[Maybe[a]]` and manipulates the `a`'s.

Many monads and all transformers have at least two type parameters.
It's considerd good practice to define type aliases for intermediate layers in the stack. 

----

**Example 1**

For example, suppose we want to wrap Either around Maybe. 
Maybe is the innermost type so we want to use the MaybeT monad transformer. 
We need to use Either as the first type parameter. 
Either itself has two type parameters and monads only have one
We need a type alias to make everything the correct shape:

```haskell
-- Create a type alias to convert Either to
-- a type constructor, ErrorOr, that has a single parameter, a:
-- "ErrorOr[a] = Either[String,a]"
type ErrorOr = Either String           

-- Use ErrorOr as a type parameter to MaybeT:
-- "ErrorMaybeOr[a] = MaybeT[ErrorOr,a]"
type ErrorMaybeOr a = MaybeT ErrorOr a
```




We have now established `ErrorMaybeOr` as monad stack that combines
the Error and Maybe monads, with Error wrapping Maybe. 
Every monad stack is itself a monad and so we 
can use `return` and `>>=` as usual.
Here we use `return` to create stack instances and then we use `>>=` 
to transform the stack instance to a new instance.

```haskell
main = do
   let stack1 = (return 10 :: ErrorMaybeOr Int)
   let stack2 = stack1 >>= (\x -> return (x + 1) :: ErrorMaybeOr Int)
   print $ stack2   
```

*Output:*
```
[...] Right (Just 11)
```

There is some junk in the `[...]`, but we'll ignore this for now. The important
thing is that we have an Either value `Right` wrapping a Maybe value of `Just`,
wrapping an Int, `11`. When we used bind, we transformed the innermost value in a stack instance.
What does that mean? With any stack instance of `ErrorMaybeOr` stack, 
we have Either wrapping Maybe wrapping an Int - since an Int is the innermost value, 
we can directly manipulate with bind and add one to it. It started off as 10 and then
we used bind to increment it to 11.

----

**Example 2**

Let’s add another monad into our stack. Let’s create a Writer of an Either of Maybe.
Once again we build this from the inside out with an MaybeT of an EitherT of Writer. 


```haskell
type Logged            = Writer [String]
type LoggedEither      = ExceptT String Logged
type LoggedEitherMaybe = MaybeT LoggedEither
```

Our mammoth stack now composes not two but *three* monads.
The `return` and `>>=` (bind) functions now cut through three layers of abstraction.

```haskell
main = do
   let stack1 = (return 10 :: LoggedEitherMaybe Int)
   let stack2 = stack1 >>= (\x -> return (x + 1) :: LoggedEitherMaybe Int)
   print $ stack2
```

*Output*
```
[...] (Right (Just 11),[])
```
This added a writer at the outermost level with a value containing
the same previous stack of `Right (Just 11)` and a log that is empty, `[]`.

---

**Example 3**

Why stop there, let's add a fourth monad. Remember that Lists are monads?
Let's create a List of Writer of an Either of Maybe. We'll need to have a List wrapping 
a Writer wrapping an Either wrapping a Maybe. As always, we start at the inside with
`MaybeT` and say its being wrapped inside Either. Then with Either, we'll use `ExceptT`
and tell it that it's being wrapped inside a Writer. Then with Writer,
we'll use `WriterT` to tell it that it needs to be wrapped inside a List.
List has nothing wrapping it, so we don't need to use `ListT`, we just use
the List type constructor, which is `[]` not "`List`" (which doesn't exist in Haskell).

```haskell
type Many                  = []
type ManyLogged            = WriterT [String] Many
type ManyLoggedEither      = ExceptT String ManyLogged
type ManyLoggedEitherMaybe = MaybeT ManyLoggedEither
```

We'll use bind to cut through *four* layers and increment the innermost integer.

```haskell
main = do
   let stack1 = (return 10 :: ManyLoggedEitherMaybe Int)
   let stack2 = stack1 >>= (\x -> return (x + 1) :: ManyLoggedEitherMaybe Int)
   print $ stack2
```

*Output*
```
[...] [(Right (Just 11),[])]
```

In the output we see that at the outermost level,
we now have square brackets showing we have a list.
And inside the list there is one element which is our previous stack 
consisting of a writer (with an empty log) containing an either (in the `Right` case)
containing a maybe (with a `Just` value) containing an int (11). 
No matter how many layers we have, the increment always targets the innermost integer.


----

**Example 4**

Let's be silly and add the identity monad, `Identity` on top. It will make no difference,
but we'll do it to show off our monad stacking skills.
Let's take the same stack and wrap it. List is now beng wrapped so we'll need to change
the type constructor from `[]` to `ListT` -- this needed because 
we need to tell list that it is now being wrapped in the `Identity` monad.

```haskell
type Same                      = Identity
type SameMany                  = ListT Same
type SameManyLogged            = WriterT [String] SameMany
type SameManyLoggedEither      = ExceptT String SameManyLogged
type SameManyLoggedEitherMaybe = MaybeT SameManyLoggedEither

main = do
   let stack1 = (return 10 :: SameManyLoggedEitherMaybe Int)
   let stack2 = stack1 >>= (\x -> return (x + 1) :: SameManyLoggedEitherMaybe Int)
   print $ stack2
```

*Output*
```
[..] [(Right (Just 11),[])])
```

The identity monad is now there on the stack, but it made no difference
to the values we see on the stack. With out stack of five monads,
our increment still targets the inner most int.

-----

All of these monad transformers follow the same conven􏰀tion: 
For a monad transformer, `FooT m a`, 
the first type parameter `m` is the next immediate *outer monad* in the stack, 
and the transformer itself provides the *inner monad* in the stack. 

If we want to wrap a `Baz` around `Bar` around a `Foo` we'd use

```
               Inside
                 ↓
Baz <- BarT <- FooT
```




> **Key Point**: `MaybeT m` is a new monad type which takes Maybe and wraps it in some monad T.
We've added the power of T to Maybe, or conversely, added the power of Maybe to T.




### The Default is the *Innermost* Monad


> **Key Point**: When we use bind together with a monad transformer stack, 
we transform the value of the *innermost* monad. If we 
have a stack where M1 wraps M2 wraps ... wraps Mn, then bind will change Mn's value.

To create a new monad transformer stack we used `(return 10 :: ErrorMaybeOr Int)`.
This placed 10 inside the innermost monad which is `Maybe`. 
The other outer monads have a default value automatically set. 
In our case, there is only one outer monad of `Either` and it's default is 
to put its value in the `Right` case. Hence we have `(Right (Just 10))`.
The definition of Either's return function is what choose this default.
Every monad has a definition of `return` that picks its default when we construct
a new stack instance.

> **Key Point**: In general for a monad transformer stack where M1 wraps M2 wraps ... wraps Mn,
we can use `return x`, and specified the value `x`
to be placed inside the *innermost monad*, Mn. The other outer monads, M1 ... Mn-1, will automatically
have a default value set to them. For each of these outer monads,
the default is determined by that particular monad's definition of `return`.

The key thing to remember is that we always work with the innermost monad.
For any monad transformer stack, operations such as bind and return automatically
cut through the outer monads and work directly with the innermost value.
By default, all of the other monads are abstracted away.

```haskell
let stack1 = (return 10 :: ErrorMaybeOr Int)
let stack2 = stack1 >>= (\x -> return (x + 1) :: ErrorMaybeOr Int)
   
     
--  use return                           use >>=
--  to create new stack                  to increment
--  setting innermost value              innermost value   
--           |                                 |
--           |            --- stack1 -----     |      ---- stack2 -----
-- 10       =======>      (Right (Just 10))   ===>     (Right (Just 11))
```







### Constucting and Unpacking Instances

As we saw above, we can use `return` to directly inject raw values into a 
fresh monad transformer stack instance. In the injected value goes inside the innermost
monad and the rest of the monads take a "default value.

```haskell
-- Create a new stance instance and inject 10 as the innermost value
let stack1 = (return 10 :: SameManyLoggedEitherMaybe Int)
```

We can also create instances from untransformed stacks 
using monad transformer’s *data constructors*.
The advantage of this later approach is that we can control the values of
all the layers -- the former appoach only allows us to control the innermost value.
The disadvantage is that it is usually more verbose.

```haskell
-- Create a new stack instance by passing in the raw data
-- We pass the raw data through each monad's data constructor
let raw = [(Right (Just 11),[])]
let stack1 = MaybeT (ExceptT (WriterT (ListT (Identity raw))))
```

Recall that stack was formed with type constructors MaybeT being wrapped by 
ExceptT, being wrapped by WriterT, being wrapped by ListT, being wrapped by Identity.
To create a stack from untransformed (raw) data, we pass the data through
the corresponding data constructors corresponding these types constructors, 
but in reverse order. Because we started with MaybeT type constructor first (innermost), we pass
our data through MaybeT data constructor last.


Once we’ve finished with a monad transformer stack, we can unpack it.
Each monad transformer `FooT` conventionally comes with a function called `runFooT`
that let's us retrieve the raw untransformed stack. Once we unpack, we can manipulate
the values in the usual way. Each call to `runFooT` only unpacks a single monad transformer, 
so we may need more than one call, `runFooT`, `runBarT`, `runBazT`, ...
to completely unpack a large stack:

```haskell
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

type Logged              = Writer [String]
type LoggedFallable      = ExceptT String Logged
type LoggedFallableMaybe = MaybeT LoggedFallable

main = do
   let packed = return 123 :: LoggedFallableMaybe Int

   let partiallyUnpacked = runMaybeT packed
   print $ partiallyUnpacked
   -- ExceptT (WriterT (Identity (Right (Just 123),[])))

   let almostUnpacked = runExceptT partial
   print $ almostUnpacked
   -- WriterT (Identity (Right (Just 123),[]))

   let completelyUnpacked = runWriter almost
   print $ completelyUnpacked
   -- Right (Just 123),[])
```


**Example**    
Give the unpacked type of `WriterT u (StateT v (Either x)) y`.

*Solution*    

```haskell
WriterT u (StateT v (Either x)) y
=> (StateT v (Either x)) (y,u)
=> (λa . (v → (Either x) (a,v)))(y,u)
=> (v → (Either x) ((y,u), v))
=> (v → (Either x) ((y,u), v))
=> v → (Either x ((y,u), v))
```


### More Precise Unpacking with `evalFooT` and `execFooT`

Suppose we have transformer for monad `Foo`, called `FooT`.
And suppose it's part of a monad transformer stack and we're about to unpack `FooT` using `runFooT`.
Sometimes `runFooT` will return a tuple `(a,s)`, but we don't want the whole tuple,
we only one just one of the values in the tuple. In this situation, 
the `FooT` newtype will provider two helper functions that let us unpack `FooT` on the stack,
but *only return one of the values in the tuple*.

These two functions are called `evalFooT` and `execFooT`.

- Extract **both**: `runFooT :: FooT s m a -> s -> m (a,s)` returns *both the tuple* `(a,s)`
- Extract **left**: `evalFooT :: FooT s m a -> s -> m a` returns *just the left* `a`
- Extract **right**: `execFooT :: FooT s m a -> s -> m s` returns *just the right* `s`


**Example**    

```haskell
import Control.Monad.Identity
import Control.Monad.State

main = do
  let st = return 1 :: StateT s Identity Int
  print $ runIdentity $  runStateT st ()      -- (value, state)
  print $ runIdentity $ evalStateT st ()      --  value
  print $ runIdentity $ execStateT st ()      --         state
```

In this example, `evalStateT` and `execStateT` are like `runStateT` except that they 
extract just the value and state respectively


### There is no IO Monad Transformer

> **Key Point**: The `IO` monad is a common candidate for the base monad.

It *never* makes sense to have `IO` as an outer monad.

> **Key Point**: The `IO` monad does not have a transformer. There is no `IOT`.

If you ever use the IO monad in a stack it will be at the bottom (the base monad)





### Naming Conventions (what's "top" and what's "bottom"?)

The following naming conventions are used:
The **bottom** is the outermost monad. By default, we don't access this monad when performing bind.
The **top** is the innermost monad. By default, bind cuts through all the monad layers
and manipulates this innermost monad.

This naming convention is intuitive in terms of access. In Data Structures and Algorithms,
with a stack, we always have O(1) access to the top element. Analogously with monad transformer stacks,
bind directly manipulates the "top", which is the innermost monad.
However the naming convention is unintuitive in terms of stacking. When we construct monad stacks,
we start at the "top", and then slot layers underneath, adding the bottom monad last.
The stack is filling top to bottom which is counterintuitive as in real life a stack would 
fill bottom to top.

Two terms that pops up are **base monad** and **precursor monad**.
For a particular transformer, `FooT`, two monads are formed, the inner monad `Foo`
and the outer monad `T`. We refer to the more outer monad as the base monad for the transformer
and the more inner monad as the precursor monad for the transformer. 
I personally don't like these terms (they're a bit too formal) --
for me, it's usually enough to just say *inner* and *outer*.




**Exercises**

**1.** State what type constructors are needed (and in what order, going inside out)
to build a stack where `IO` wraps `Writer` wraps `State` wraps `Maybe`?

*Answer:* Remember that for a monad transformer, `FooT m a`,
the first type parameter is the next immediate *outer* monad in the stack,
and the transformer itself provides the *inner* monad. 
So we need MaybeT, followed by StateT, followed by WriterT and finally IO.


```
                          Inside
                            ↓
IO <- WriterT <- StateT <- MaybeT
```

**2.** Draw the **monad transformer stack** for the following monad tranformer types.

**(a)** `WriterT u (StateT v (Either x)) y`    
**(b)** `WriterT [(String, Int)] IO ()`    
**(c)** `ErrorT u (WriterT v (State x)) y`    


**3.** For the following monad transformer stacks, state which monad is the **base monad**.

**(a)** `WriterT u (StateT v (Either x)) y`    
**(b)** `WriterT [(String, Int)] IO ()`    
**(c)** `ErrorT u (WriterT v (State x)) y`    

**4.** Give the unpacked type for the following monad transformer stacks

**(a)** `WriterT u (StateT v (Either x)) y`    
**(b)** `WriterT [(String, Int)] IO ()`    
**(c)** `ErrorT u (WriterT v (State x)) y`    
