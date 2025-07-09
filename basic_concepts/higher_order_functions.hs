-- 1. 
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 2.
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 3.
cMap :: (a -> b) -> [a] -> [b]
cMap _ [] = []
cMap f (x:xs) = f x : cMap f xs

-- 4.
cFilter :: (a -> Bool) -> [a] -> [a]
cFilter _ [] = []
cFilter f (x:xs)
  | f x = x : cFilter f xs
  | otherwise = cFilter f xs

-- 5.
collatz :: Int -> [Int]
collatz x = takeWhile (/= 1) (iterate helper x) ++ [1]
  where
    helper :: Int -> Int
    helper x
      | even x    = x `div` 2
      | otherwise = x * 3 + 1

-- more exercises
-- 1.
infPowerTwo :: [Int]
infPowerTwo = 1 : iterate (*2) (head infPowerTwo)

-- 2.
repeatSeq :: [Int] -> [Int]
repeatSeq l = l ++ repeatSeq (appendSelf l l)
  where
    appendSelf :: [Int] -> [Int] -> [Int]
    appendSelf _ [] = []
    appendSelf l (x:xs)  = x : appendSelf l xs
-- fix?
repeatSeqCycle :: [a] -> [a]
repeatSeqCycle xs = cycle xs
-- fix2
repeatSeqSelf :: [a] -> [a]
repeatSeqSelf xs = xs ++ repeatSeqSelf xs

-- 3.
nthFib :: Int -> Int
nthFib x = last (take x fibs)

-- 4.
primes :: [Int]
-- try 1
-- primes = [2..] ++ filter (\x -> x `mod` last primes == 0) primes 
-- try 2
primes = sieveOfEratosthenes [2..]
  where
    sieveOfEratosthenes :: [Int] -> [Int]
    sieveOfEratosthenes (head:tail) = head : sieveOfEratosthenes (filter (\x -> x `mod` head /= 0) tail)

-- 5.
collatzLenSeq :: [Int]
collatzLenSeq = callCollz [1..]
  where 
    callCollz :: [Int] -> [Int]
    callCollz (x:xs) = collatz x ++ callCollz xs

-- corrected
collatzLenSeqC :: [Int]
collatzLenSeqC = map (length . collatz) [1..]

-- 6.
mergeInfList :: (Ord a) => [a] -> [a] -> [a]
mergeInfList [] _ = []
mergeInfList _ [] = []
mergeInfList (x:xs) (z:zs)
  | x <= z = x : mergeInfList xs (z:zs)
  | otherwise = z : mergeInfList (x:xs) zs

-- 7.
-- pascalRows :: [[Int]]
-- -- pascalRows = [1] : [1,1] : genpascal [1..]
-- pascalRows = genpascal [[1]]
--   where
--     genpascal :: [[Int]] -> [[Int]]
--     genpascal [] = [1] 
--     genpascal (x:xs) 
    

-- since i wasnt progressing at pascal inf, i tried to split everything
-- into multiple functions to see if my head would work better that way
splitPairs :: [a] -> [[a]]
splitPairs [] = []
splitPairs [_] = []
splitPairs (x:y:xs) = [x, y] : splitPairs (y:xs)

-- using zip instead of recursion
makePairs :: [a] -> [(a,a)]
makePairs xs = zip xs (tail xs)

sumTuple :: Num a => [(a,a)] -> [a]
sumTuple [] = []
sumTuple ((a,b):xs) = (a + b) : sumTuple xs

-- more implementation to learn
sumT2 :: Num a => [(a,a)] -> [a]
sumT2 = map (uncurry (+))

sumPairs :: (Num a) => [[a]] -> [a]
-- original of my take, corrected my take with compiler help.
-- sumPairs [] = []
-- sumPairs (head:tail) = sum head : sumPairs tail
sumPairs = map sum

-- computes the next pascal row, given we have at least one row
nextRow :: (Num a) => [a] -> [a]
nextRow [] = []
nextRow xs = 1 : sumTuple (makePairs xs) ++ [1]

-- then we can iterate ? i thought it was for int only, guess its generic
pascalRows :: (Num a) => [[a]]
pascalRows = iterate nextRow [1]

powerOfTwoSum :: Num a => [a] -> [a]
powerOfTwoSum (x:xs) = (x+x) : xs

-- putting everything in one
pAglomerate :: Num a => [[a]]
pAglomerate = iterate helper [1]
  where
    helper :: Num a => [a] -> [a]
    helper [] = []  -- this is useless since we can stricly once with [1] as input
    helper xs = 1 : sumPair (makePair xs) ++ [1]
    
    sumPair :: Num a => [(a,a)] -> [a]
    sumPair = map (uncurry (+))
    -- uncurry applies the function, to a (pair) // my understanding
    -- so if you have (a, b) and uncurry with *, it will be return a*b
    -- then we map over the entire list so we go back to being a list of a instead of list of pairs of a

    makePair :: [a] -> [(a,a)]
    makePair xs = zip xs (tail xs)
    -- this is basically saying, (x:y:xs) => recursion of [(x,y)] : f (y:xs)

-- 8.
cumulativeSums :: (Num a) => [a] -> [a]
-- consider using scanl
-- cumulativeSums xs = scanl (+) 0 xs
-- it applies a function to the list, but saving the results, as opposed to foldl
cumulativeSums xs = helper xs 0
  where
    helper :: (Num a) => [a] -> a -> [a]
    helper [] _ = []
    helper (x:xs) n = (n+x) : helper xs (n+x) 


main :: IO ()
main = do
  print $ applyTwice (* 10) 5
  print $ cMap (* 10) [5, 20, 1, 3, -1]
  print $ cFilter odd [5, 20, 1, 3, -1]
  print $ collatz 15
  print $ take 20 fibs
  print $ take 20 infPowerTwo
  print $ take 50 (repeatSeq [1, 5, 3, 1])
  print $ take 111 primes
  print $ nthFib 20
  print $ take 20 collatzLenSeq
  print $ take 20 $ mergeInfList primes fibs
  print $ makePairs [1,3,3,1]
  print $ sumTuple $ makePairs [1,3,3,1]
  print $ sumT2 $ makePairs [1,3,3,1]
  print $ nextRow [1,3,3,1]
  print $ take 10 pascalRows
  print $ take 10 (iterate powerOfTwoSum [1])
  print $ take 5 pAglomerate
  print $ cumulativeSums [1, 5, 9, 12, 15]
