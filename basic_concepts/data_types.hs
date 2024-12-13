-- 1.
data Person = Person String Int String

describePerson :: Person -> String
describePerson (Person pName pAge pColor) = "Name: " ++ show pName ++ " Idade: " ++ show pAge ++ " Color: " ++ show pColor

-- 2.
data Point = Point Float Float

pDistance :: (Point, Point) -> Float
pDistance (Point x y, Point a b) = sqrt $ (a - x)**2 - (b - y)**2

-- 3.
data Shape = Circle Float | Rectangle Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi * r**2
area (Rectangle w h) = w * h
area (Square l) = l**2

-- 4.
data List a = Empty | Cons a (List a)

-- gpt created show helper.. idk what 'instace' nor 'where' are yet.
instance Show a => Show (List a) where
  show Empty = "Empty"
  show (Cons x xs) = show x ++ " : " ++ show xs

-- my take
-- listLength :: List a -> Int
-- listLength Empty = 0
-- listLength (x:xs) = 1 + listLength xs
-- answ
listLength :: List a -> Int
listLength (Cons _ tail) = 1 + listLength tail
listLength Empty = 0


listAdd :: a -> List a -> List a
listAdd xs lis = Cons xs lis

listAddSug :: a -> List a -> List a
listAddSug = Cons

listFromBuiltin :: [a] -> List a
listFromBuiltin [] = Empty
listFromBuiltin (x:xs) = Cons x (listFromBuiltin xs)
-- lsp suggested
-- listFromBuiltin lis = foldr Cons Empty lis
-- lsp suggested another
-- listFromBuiltin = foldr Cons Empty

listFromListA :: List a -> [a]
listFromListA Empty = []
listFromListA (Cons x tail) = x : listFromListA tail

-- this is actually prepend
-- listAppend :: List a -> a -> List a
-- listAppend lis added = Cons added lis 
-- corrected
listAppend :: List a -> a -> List a
listAppend Empty to_add = Cons to_add Empty
listAppend (Cons head tail) to_add = Cons head (listAppend tail to_add)

-- my version
-- listMap :: List a -> function -> List a
-- listMap (Cons x xs) f = f x (listMap xs)
-- way to describe a "function" inside a parameter is within ( )
-- corrected
listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap func (Cons head tail) = Cons (func head) (listMap func tail)

-- my version
-- listReverse :: List a -> List a
-- listReverse Empty = Empty
-- listReverse (Cons x xs) = Cons x (listReverse xs)
-- fixed version
listReverse :: List a -> List a
listReverse lis = reverseHelper lis Empty
  where
    reverseHelper :: List a -> List a -> List a
    reverseHelper Empty lis = lis
    reverseHelper (Cons head tail) lis = reverseHelper tail (Cons head lis)

-- my version
-- listFilter :: List a -> something -> List a
-- listFilter (Cons h t) s
--   | h == s = (Cons h (listFilter t s))
--   | h != s = listFilter t s
--   | Empty = Empty
-- corrected
-- filter is a ( ) that takes a and evaluates to Boolean, our list doesnt change its type, just its size
listFilter :: (a -> Bool) -> List a -> List a
listFilter _ Empty = Empty
listFilter func (Cons head tail)
  | func head {-- if func head evaluates as true --} = Cons head (listFilter func tail) 
  | otherwise {-- we just move on                --} = listFilter func tail

main :: IO ()
main = do
  let mylist = Empty

  print $ show (listLength mylist)
  let newList = listAdd 2 mylist
  let newList1 = listAdd 7 newList
  let newList2 = listAdd 5 newList1
  let newList3 = listAppend newList2 5

  print $ show (listAdd 15 newList)

  print $ show newList3
  print $ show  (listAppend newList3 25)
  print $ show (listReverse newList3)

