New Exercises

Now, let’s build on these ideas and introduce some additional challenges that focus on higher-order functions, infinite sequences, and functional composition.
Problem 1: Merge Infinite Sequences

Write a function mergeSequences that merges two sorted infinite sequences into one sorted infinite sequence. For example:

mergeSequences [2, 4, 6..] [1, 3, 5..]
-- Result: [1, 2, 3, 4, 5, 6...]

Problem 2: Infinite Powers of a Number

Write a function infPowers that generates an infinite list of powers of a given number. For example:

take 5 (infPowers 3)  -- [1, 3, 9, 27, 81]

Problem 3: nthPrime

Write a function nthPrime that returns the nth prime number. Use your primes list for efficiency. For example:

nthPrime 5  -- 11

Problem 4: Generate Triangle Numbers

Write a function triangleNumbers that generates the sequence of triangle numbers:

1, 3, 6, 10, 15, 21, ...

Formula: The nth triangle number is Tn=n(n+1)2Tn​=2n(n+1)​.
Problem 5: Custom ZipWith

Write your own version of zipWith, which combines two lists using a given function:

cZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cZipWith (+) [1, 2, 3] [4, 5, 6]  -- [5, 7, 9]

Problem 6: Infinite Hailstone Sequences

Write a function infiniteHailstones that generates all hailstone sequences (Collatz sequences) for integers starting from 1:

take 5 (infiniteHailstones !! 2)
-- Result: [3, 10, 5, 16, 8, 4, 2, 1]

Problem 7: Sum of Digits

Write a function sumDigits that calculates the sum of the digits of a number:

sumDigits 12345  -- 15

Problem 8: Pascal's Triangle as Infinite Lists

Generate Pascal’s Triangle as an infinite list of rows. For example:

take 5 pascal
-- [[1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1]]
