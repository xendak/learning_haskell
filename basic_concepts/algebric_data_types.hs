import Data.Maybe

data Tree a = Empty
            | Node a (Tree a) (Tree a)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert to_add Empty = Node to_add Empty Empty
treeInsert to_add (Node n l r)
  | to_add >= n = Node n l (treeInsert to_add r)
  | otherwise = Node n (treeInsert to_add l) r


treeContains :: (Ord a) => a -> Tree a -> Bool
treeContains _ Empty = False
treeContains to_find (Node n l r)
  | n == to_find = True
  | to_find < n = treeContains to_find l
  | otherwise = treeContains to_find r

-- Node 5
--   Left: Node 3
--     Left: EmptyTree
--     Right: EmptyTree
--   Right: Node 7
--     Left: EmptyTree
--     Right: EmptyTree
-- GOAL
instance Show a => Show (Tree a) where
  show Empty = "Empty Tree"
  show tr = treeTraverse 0 "Node: " tr
    where
      useSpaces :: Int -> String
      useSpaces num
        | num == 0 = ""
        | otherwise = " " ++ useSpaces (num - 1)

      -- i need to try to understand this a bit better.. was kind lucky i got it done
      treeTraverse :: Int -> String -> Tree a -> String
      treeTraverse depth str Empty = useSpaces (depth * 2) ++ str ++ "Empty Node\n"
      treeTraverse depth str (Node n l r) = 
        useSpaces (depth * 2) ++ str ++ show n ++ "\n" ++ 
        treeTraverse (depth + 1) "Left: " l ++ 
        treeTraverse (depth + 1) "Right: " r


treeMax :: (Ord a) => Tree a -> Maybe a
treeMax Empty = Nothing
treeMax (Node n _ r) = Just (search n r)
  where 
    search :: (Ord a) => a -> Tree a -> a
    search max Empty = max
    search max (Node n _ r)
      | n > max   {-- search with new max --}  = search n r
      | otherwise {-- keep same max       --}  = search max r


exampleTree :: Tree Int
exampleTree = 
  Node 10 
    (Node 5 
      (Node 2 Empty Empty) 
      (Node 8 Empty Empty)) 
    (Node 15 
      (Node 12 Empty Empty) 
      (Node 20 Empty Empty))


main :: IO ()
main = do
  print exampleTree
  let res = treeMax exampleTree
  print (fromMaybe 0 res)
  -- print (treeInsert 3 exampleTree)
