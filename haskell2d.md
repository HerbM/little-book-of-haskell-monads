## 2D. Composing do Blocks

```
do
  x1 <- m1
  __________________
  |                |
  |   t1 -> M t0   |
  |                |
  __________________

_____________________
|                   |
|                   |
|       M t0        |
|                   |
|                   |
_____________________


bind: (t1 -> M t0) -> M t1 -> M t0
                        └───────┐
                              M t1
```




### The Associativity Law

```
do
  x1 <- m1
  x2 <- m2
  __________________
  |                |
  |   t2 -> M t3   |
  |                |
  __________________


do
  x2 <- do
      x1 <- m1
      m2
  __________________
  |                |
  |   t2 -> M t3   |
  |                |
  __________________


do
  x1 <- m1
  do
    x2 <- m2
    do
      __________________
      |                |
      |   t2 -> M t3   |
      |                |
      __________________
```


### do blocks, Recursion and Induction

```
do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  e
```


Apply the associativity law multiple times:

```
do
  x4 <- do
    x3 <- do
      x2 <- do
        x1 <- do
          m1
        m2
      m3
    m4
  e
```

Then we can put emphasis on the types of the corresponding lambda-bind.
There is a mapping that takes each `mi` above in the do-block and
maps it to a `\xi -> mi` in the corresponding lambda-bind expression.
The lambda-bind has type `ti -> M ti+1`.
We're focusing on types so for each `mi`, 
we'll put this type in our picture:

```
do
  x4 <- do
      x3 <- do
          x2 <- do
              x1 <- do
              __________________
              |                |
              |   t1 -> M t2   |
              |                |
              __________________
          __________________
          |                |
          |   t2 -> M t3   |
          |                |
          __________________
      __________________
      |                |
      |   t3 -> M t4   |
      |                |
      _________________
  _________________
  |               |
  |   t -> M tE   |
  |               |
  _________________


IND. CASE (bind):    (t4 -> M tE) -> M t4 -> M tE
                                      └───────┐
IND. CASE (bind):    (t3 -> M t4) -> M t3 -> M t4
                                      └───────┐
IND. CASE (bind):    (t2 -> M t3) -> M t2 -> M t3
                                      └───────┐
IND. CASE (bind):    (t1 -> M t2) -> M t1 -> M t2
                                      └───────┐
BASE CASE (unit):                            M t1
```




```

### Explaining `a -> M b` with Recursion

Mneumonic: a for any, b for base

  
Rough ideas:
```

a = Any types
b = type in the Base case

-- ith line of a do block
Bi+1 = (xi+1 -> Bi ) =<< mi+1

-- last line
B0 = e
````
B0 :: M b
if Bi :: M b, then Bi+1 :: m b
if Mi :: M a, then x ::a 
```
