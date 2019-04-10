sumNat :: Int -> Int
sumNat n = sumNat' n 0
  where sumNat' 0 acc = acc
        sumNat' m acc = sumNat' (m-1) (acc+m)

fact :: Int -> Int
fact n = fact' n 1
  where fact' 0 acc = acc
        fact' m acc = fact' (m-1) (m*acc)

fib :: Int -> Int
fib n = fib' n 0 1
  where fib' 0 a b = a
        fib' 1 a b = b
        fib' m a b = fib' (m-1) b (a+b)
