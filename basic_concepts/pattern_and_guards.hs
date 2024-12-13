-- 1.
-- doineedtodefinetype is a pplaceholder, can be any name from what i understood?
listSummary :: [doineedtodefinetype] -> String
listSummary [] = "Empty"
-- does not need to be named, single check from what it seems
listSummary [some_val] = "Single Item"
-- _ matches all, and [placeholder] gives a single?
listSummary _ = "Multiple Items"

-- 2.
isOdd :: Int -> Bool
isOdd a
  | a `mod` 2 == 0 = False
  | otherwise = True

-- 3.
  -- is like this, top to bottom pattern matching
fizzBuzz :: Int -> String
fizzBuzz num
  | num `mod` 3 == 0 && num `mod` 5 == 0 = "FizzBuzz"
  | num `mod` 3 == 0 = "Fizz"
  | num `mod` 5 == 0 = "Buzz"
  | otherwise = show num


-- why this does not work? future me.
main :: IO ()
main = putStrLn $ fizzBuzz 5
