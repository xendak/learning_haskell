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
primes = filterOnPosition [2..]
  where
    filterOnPosition :: [Int] -> [Int]
    filterOnPosition (y:ys) = y : filterOnPosition (filter (\x -> x `mod` y /= 0) ys)

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
