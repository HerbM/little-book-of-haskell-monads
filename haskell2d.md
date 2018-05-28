## 2D. Composing do Blocks


### The Associativity Law


### do blocks, Recursion and Induction


```
do
  x1 <- do
      x2 <- do
          x3 <- do
              x4 <- return u
              __________________
              |                |
              |   t4 -> M t3   |
              |                |
              __________________
          __________________
          |                |
          |   t3 -> M t2   |
          |                |
          __________________
      __________________
      |                |
      |   t2 -> M t1   |
      |                |
      _________________
  _________________
  |               |
  |   t1 -> M t0  |
  |               |
  _________________


IND. CASE (bind):    (t1 -> M t0) -> M t1 -> M t0
                                      └───────┐
IND. CASE (bind):    (t2 -> M t1) -> M t2 -> M t1
                                      └───────┐
IND. CASE (bind):    (t3 -> M t2) -> M t3 -> M t2
                                      └───────┐
IND. CASE (bind):    (t4 -> M t3) -> M t4 -> M t3
                                      └───────┐
BASE CASE (unit):                     t4 -> M t4
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
