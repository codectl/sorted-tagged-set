module SortedTaggedSet (
  SortedTaggedSet,
  empty,
  nullSet,
  belongs,
  lengthSet,
  singleton,
  peek,
  insertSet,
  removeSet,
  insertTag,
  merge,
  show
) where

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
belongs x (STS (y:ys)) = x==fst y || (x>fst y && belongs x (STS ys))

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
peek e (STS (x:xs)) = if e == fst x then snd x else peek e (STS xs)

-- Inserts new element in set, if not present
insertSet :: Ord a => a -> SortedTaggedSet a -> SortedTaggedSet a
insertSet x (STS sts) = STS (ins x sts)
  where
    ins e [] = [(e,[])]
    ins e (x:xs) = case compare e (fst x) of
      LT -> (e,[]):(x:xs)
      EQ -> x:xs
      GT -> x : ins e xs

-- Removes element from set
removeSet :: Ord a => a -> SortedTaggedSet a -> SortedTaggedSet a
removeSet x (STS sts) = STS (rem x sts)
  where
    rem _ [] = []
    rem e (x:xs) = case compare e (fst x) of
      LT -> x:xs
      EQ -> xs
      GT -> x : rem e xs

-- Inserts new element with tag to the set
insertTag :: Ord a => String -> a -> SortedTaggedSet a -> SortedTaggedSet a
insertTag tag x (STS sts) = STS (insTag tag x sts)
  where
    insTag _ _ [] = []
    insTag tag e (x:xs) = case compare e (fst x) of
      LT -> x:xs
      EQ -> (fst x, insElem tag (snd x)):xs
      GT -> x : insTag tag e xs
      where
        insElem x [] = [x]
        insElem x (y:ys) = case compare x y of
          LT -> x:y:ys
          EQ -> y:ys
          GT -> y : insElem x ys

-- Merges two sets
merge :: Ord a => SortedTaggedSet a -> SortedTaggedSet a -> SortedTaggedSet a
merge (STS sts1) (STS sts2) = STS (merge' sts1 sts2)
  where
    merge' [] xs = xs
    merge' xs [] = xs
    merge' (x:xs) (y:ys) = case compare (fst x) (fst y) of
      LT -> x : merge' xs (y:ys)
      GT -> y : merge' (x:xs) ys
      EQ -> (fst x, (mergeSet (snd x) (snd y))) : merge' xs ys
      where
        mergeSet xs [] = xs
        mergeSet [] xs = xs
        mergeSet (x:xs) (y:ys) = case compare x y of
          LT -> x : mergeSet xs (y:ys)
          EQ -> x : mergeSet xs ys
          GT -> y : mergeSet (x:xs) ys

-- Textual representation for the sorted tagged set
instance Show a => Show (SortedTaggedSet a) where
  show (STS ts) = "{" ++ printTaggedSet (STS ts) ++ "}"

printTaggedSet :: Show a => SortedTaggedSet a -> String
printTaggedSet (STS []) = " "
printTaggedSet (STS [(elem,tags)]) = show elem ++ "#" ++ show tags
printTaggedSet (STS ((elem,tags):elems)) = show elem ++ "#" ++ show tags ++ "," ++ printTaggedSet (STS elems)