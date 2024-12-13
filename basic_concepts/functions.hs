-- 1.
sumList :: [a] -> Int -> Int
sumList [] num
  | num == 0 = 0
  | num > 0  = []!!1 + ( sumList  [] num - 1 )

-- 2.
filterOdd :: [a] -> [b]
filterOdd (x:xs) = filter odd []

-- 3.
factorialFold :: Int -> Int
factorialFold a = foldl (+) 0 $ enumFromTo 0 a

-- 4.
mapAdd3 :: [a] -> [a]
mapAdd3 [a] = map (+3) [a] 


-- corrected
-- 1. We need [Int] since we make a restriction that we are working with numbers?
-- woudl it work if it was any number? like float or double?
-- we also dont need the num referece i used, since (x:xs) splits the list in [head] and [t,a,i,l]
sumListCorrected :: [Int] -> Int
sumListCorrected [] = 0
sumListCorrected (x:xs) = x + sumListCorrected xs
-- or we could use foldr
sumListFoldr :: [Int] -> Int
sumListFoldr xs = foldr (+) 0 xs
-- or we could just use inbuilt sum
sumListSum :: [Int] -> Int
sumListSum xs = sum xs
-- answer to my question
sumListGeneric :: Num a => [a] -> a
sumListGeneric [] = 0
sumListGeneric (x:xs) = x + sumListGeneric xs

-- 2.
-- same as 1., we need [Int] since we are working with numbers
filterOddCorrected :: [Int] -> [Int]
filterOddCorrected xs = filter odd xs

-- 3. Just a little ooppsie, did a sum instead of mul and included 0?
factorialFoldCorrected :: Int -> Int
factorialFoldCorrected x = foldl (*) 1 (enumFromTo 1 x)

-- 4.
mapAdd3Corrected :: [Int] -> [Int]
mapAdd3Corrected xs = map (+3) xs
-- or
mapAdd3Corrected xs = map (\x -> x + 3) xs


main :: IO ()
main = do
  print (show $ filterOddCorrected [1, 5, 332, 2, 11, 4])
  print (show $ mapAdd3Corrected [1, 3, 5])
  -- print (show $ sumListCorrected [1, 5, 9])
