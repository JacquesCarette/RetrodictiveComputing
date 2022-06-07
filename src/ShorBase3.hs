module ShorBase3 where

import Data.List

------------------------------------------------------------------------------
-- Manually optimized shor 21 using qutrits

-- Gates (defined in: https://arxiv.org/pdf/1512.03824.pdf)
-- 
-- Let delta (x,y) = if x == y then 1 else 0
-- 
--     X (x) = x+1 
--  CX (x,y) = (x , y + delta(x,2))
-- SUM (x,y) = (x, x+y) 
-- 


data Gate = X Int | SUM Int Int | CX Int Int

type State = [Int]

upd :: State -> Int -> (Int -> Int) -> State
upd st i f = st0 ++ (f x : st1)
  where (st0,(x:st1)) = splitAt i st

delta x y = if x == y then 1 else 0

plus :: Int -> Int -> Int
x `plus` y = (x + y) `mod` 3

inc :: Int -> Int
inc x = x `plus` 1

eval :: Gate -> State -> State
eval (X i) st = upd st i inc
eval (SUM i j) st = upd st j (\y -> st !! i `plus` y)
eval (CX i j) st = upd st j (\y -> delta (st !! i) 2 `plus` y)

evalC :: State -> [Gate] -> State
evalC = foldr eval

---

circ :: [Gate]
circ = [ X 4, SUM 1 3, CX 1 2 ]

qst :: [State]
qst = [ [x,y,0,0,0] | x <- [0,1,2], y <- [0,1,2] ]

run = map (\st -> (st, evalC st circ)) qst

------------------------------------------------------------------------------


