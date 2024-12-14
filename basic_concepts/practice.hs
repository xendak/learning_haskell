
-- 0 1 1 2 3 5 8 13 21 34 55 89
fib :: Int -> Int
fib x
  | x <= 0 = 0
  | x == 1 = x + fib 0
  | otherwise = fib (x - 1) + fib (x - 2)

-- 1.
factorial :: Int -> Int
factorial x
  | x == 1 = 1
  | otherwise = x * factorial (x - 1)

-- 2.
sumListRec :: [Int] -> Int
sumListRec tail = foldr (+) 0 tail

-- 3.
power :: Int -> Int -> Int
power _ 0 = 1
power 0 _ = 0
power x y |
  y > 0 = x * power x (y - 1)


-- 4.
  -- 1 : [5, 3] -> inserts at start -> [1, 5, 3]
  -- reverseAux (head:tail) [] = 
  -- reverse -> [1] [5, 3, 7]
  --                [1, 5] [3, 7]
  --                       [1, 5, 3] [7]
  --                                 [1, 5, 3, 7] []
  --                       [5, 3, 7] [1]
  --                [3, 7] [5, 1]
  --            [3] [7, 5, 1]
  --     []  [3, 7, 5, 1] -> return     
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (head:tail) = reverseAux [head] tail
  where
    reverseAux :: [a] -> [a] -> [a]
    reverseAux [] d = d
    reverseAux (head:tail) [] = head : reverseAux tail []
    reverseAux li (h:t) = reverseAux (h : li) t

-- a bit more simplified version is
simplifiedReverseList :: [a] -> [a]
simplifiedReverseList [] = []
simplifiedReverseList (h:t) = reverseList t ++ [h]

-- 5.
countOccurrences :: Int -> [Int] -> Int
countOccurrences x list = length $ filter (==x) list

-- 6.
maxElement :: [Int] -> Maybe Int
maxElement [] = Nothing
maxElement (h:t) = Just $ getMax h t
  where
    getMax :: Int -> [Int] -> Int
    getMax num [] = num
    getMax num (h:t)
      | h >= num = getMax h t
      | otherwise = getMax num t

-- 7.
pascal :: Int -> Int -> Int
pascal n k
  | n <= 1 = 1
  | k <= 1 || k == n = 1
  | otherwise = pascal (n-1) (k-1) + pascal (n-1) k

-- 8.
mergeLists :: [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists [] x = x
mergeLists x [] = x
mergeLists (x:xs) (z:zs) = fuse [x, z] xs zs
  where
    fuse :: [a] -> [a] -> [a] -> [a]
    fuse d [] [] = d
    fuse li (x:xs) (z:zs) = fuse (li ++ [x, z]) xs zs

-- merge with sort?
sortMergeLists :: (Ord a) => [a] -> [a] -> [a]
sortMergeLists [] ys = ys
sortMergeLists xs [] = xs
sortMergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- 9.
collatz :: Int -> Int
collatz x = helper x 0
  where
    helper :: Int -> Int -> Int
    helper x steps
      | x == 1 = steps
      | even x = helper (x `div` 2) (steps + 1)
      | odd x = helper (x * 3 + 1) (steps + 1)

collatzPrint :: Int -> String
collatzPrint x = "collatz " ++ show x ++ " = " ++ helper (" " ++ show x) x 0
  where
    helper :: String -> Int -> Int -> String
    helper str x steps
      | x == 1 = show steps ++ " =>" ++ str
      | even x = helper (str ++ "->" ++ show (x `div` 2)) (x `div` 2) (steps + 1)
      | odd x  = helper (str ++ "->" ++ show (x * 3 + 1)) (x * 3 + 1) (steps + 1)

-- 10.
-- this is just wrong.. i realized i will need to reverse the end no matter so.. and if so, might as well just reverse everything
-- since complexity is stil the same
-- "hello" -> "he" ===? "lo" when it should be  "he" ===? "ol"
isPalindrome :: String -> Bool
isPalindrome str
  | odd (length str) = helper  (take ((length str - 1) `div` 2) str) (drop (1 + (length str `div` 2)) str)
  | even (length str) = helper  (take (length str `div` 2) str) (drop (length str `div` 2) str)
  where
    helper :: String -> String -> Bool
    helper a b
      | a == b = True
      | otherwise = False

-- fixed
smartIsPalindrome :: (Eq a) => [a] -> Bool
smartIsPalindrome xs = xs == reverse xs

main :: IO ()
main = do
  print $ factorial 5
  print $ sumListRec [5, 20, 11, 15]
  print $ power 0 0
  print $ reverseList [5, 20, 11, 15]
  print $ countOccurrences 20 [5, 20, 20, 2, 10, 20, 11, 15]
  print $ maxElement [5, 20, 20, 77, 10, 20, 11, 15]
  print $ pascal 6 3
  print $ mergeLists [5, 20, 11, 15] [10, 7, 33, 2]
  print $ collatzPrint 6
  print $ isPalindrome "tetet"
