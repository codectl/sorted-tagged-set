module SortedTaggedSet where

data SortedTaggedSet a = STS [(a,[String])]

-- Returns empty set
empty :: SortedTaggedSet a
empty = STS []

-- Checks whether set is empty
nullSet :: SortedTaggedSet a -> Bool
nullSet (STS []) = True
nullSet _ = False

-- Checks whether element belongs to set
belongs :: Ord a => a -> SortedTaggedSet a -> Bool
belongs _ (STS []) = False
belongs x (STS ((y,_):ys)) = x==y || (x>y && belongs x (STS ys))

-- Returns the number of the elements in the set
lengthSet :: SortedTaggedSet a -> Int
lengthSet (STS []) = 0
lengthSet (STS (_:xs)) = 1 + lengthSet (STS xs)

-- Returns set with single element
singleton :: a -> SortedTaggedSet a
singleton x = STS [(x,[])]

-- Returns tags from an element
peek :: (Ord a) => a -> SortedTaggedSet a -> [String]
peek _ (STS []) = []
peek e (STS ((x,xs):ys)) = if e == x then xs else peek e (STS ys)

-- Inserts new element in set, if not present, returning sorted set
insertSet :: Ord a => a -> SortedTaggedSet a -> SortedTaggedSet a
insertSet x (STS sts) = STS (ins x sts)
  where
    ins e [] = [(x,[])]
    ins e ((x,xs):ys)
      | e < x     = (e,[]):(x,xs):ys
      | e == x    = (x,xs):ys
      | otherwise = (x,xs) : ins e ys

-- Removes element from set
removeSet :: Ord a => a -> SortedTaggedSet a -> SortedTaggedSet a
removeSet x (STS sts) = STS (rem x sts)
  where
    rem _ [] = []
    rem e ((x,xs):ys)
      | e < x     = (x,xs) : rem e ys
      | e == x    = ys
      | otherwise = (x,xs):ys

-- Inserts new element with tag to the set
insertTag :: Ord a => String -> a -> SortedTaggedSet a -> SortedTaggedSet a
insertTag tag x (STS sts) = STS (insTag tag x sts)
  where
    insTag _ _ [] = []
    insTag tag e ((x,xs):ys)
      | e < x     = (x,xs):ys
      | e == x    = (x,insElem tag xs):ys
      | otherwise = (x,xs) : insTag tag e ys
      where
        insElem x [] = [x]
        insElem x (y:ys)
          | x < y    = x:y:ys
          | x == y   = y:ys
          | otherwise = y : insElem x ys

-- Merges two sets
merge :: Ord a => SortedTaggedSet a -> SortedTaggedSet a -> SortedTaggedSet a
merge (STS sts1) (STS sts2) = STS (merge' sts1 sts2)
  where
    merge' [] xs = xs
    merge' xs [] = xs
    merge' ((x1,xs1):ys1) ((x2,xs2):ys2)
      | x1 < x2   = (x1,xs1) : merge' ys1 ((x2,xs2):ys2)
      | x1 > x2   = (x2,xs2) : merge' ((x1,xs1):ys1) ys2
      | otherwise = (x1,(mergeSet xs1 xs2)) : merge' ys1 ys2
      where
        mergeSet xs [] = xs
        mergeSet [] xs = xs
        mergeSet (x:xs) (y:ys)
          | x < y     = x : mergeSet xs (y:ys)
          | x == y    = x : mergeSet xs ys
          | otherwise = y : mergeSet (x:xs) ys

instance Show a => Show (SortedTaggedSet a) where
  show (STS ts) = "{" ++ printTaggedSet ts ++ "}"

printTaggedSet :: Show a => [(a,[String])] -> String
printTaggedSet [] = " "
printTaggedSet [(elem,tags)] = show elem ++ "#" ++ show tags
printTaggedSet ((elem,tags):elems) = show elem ++ "#" ++ show tags ++ "," ++ printTaggedSet elems