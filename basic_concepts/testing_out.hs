
-- 0 1 1 2 3 5 8 13 21 34 55 89
fib :: Int -> Int
fib x
  | x <= 0 = 0
  | x == 1 = x + fib 0
  | otherwise = fib (x - 1) + fib (x - 2)
  
fat :: Int -> Int
fat x
  | x == 1 = 1
  | otherwise = x * fat (x - 1)

sumListRec :: [Int] -> Int
sumListRec [] = 0
sumListRec (head:tail) = head + sumListRec tail


pow :: Int -> Int -> Int
pow _ 0 = 1
pow 0 _ = 0
pow x y |
  y > 0 = x * pow x (y - 1) 

reveseList :: [a] -> [a]
reverseList [] = []
reverseList (head:tail) = reverseAux head tail
  where 
    reverseAux :: [a] -> [a] -> [a]
    reverseAux 


main :: IO ()
main = do
  print $ show (fat 5)
  print $ show (sumListRec [5, 20, 11, 15])
  print $ show (pow 0 0)
