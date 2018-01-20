## 2D. do blocks, Recursion and Induction


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

B0 :: M b
if Bi :: M b, then Bi+1 :: m b
if Mi :: M a, then x ::a 
```
