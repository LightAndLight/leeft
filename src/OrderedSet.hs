{-# language BangPatterns #-}
module OrderedSet
  (Set, empty, toList, size, insert, pos, posInsert, posInsertMay)
where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- | Ordered Set
data Set a = Set !Int [a]
  deriving Show

empty :: Set a
empty = Set 0 []

size :: Set a -> Int
size (Set s _) = s

toList :: Set a -> [a]
toList (Set _ s) = s

-- | Insert an element
insert :: Eq a => a -> Set a -> Set a
insert a (Set n s) = go n s
  where
    go n s =
      case s of
        [] -> Set (n+1) [a]
        x:xs
          | x == a -> Set n s
          | otherwise ->
            case go n xs of
              Set n' xs' -> Set n' $ x : xs'

-- | Get the position of an element
pos :: Eq a => a -> Set a -> Maybe Int
pos a (Set _ s) = elemIndex a s

-- | Insert the element, then get its position
posInsert :: Eq a => a -> Set a -> (Int, Set a)
posInsert a s = fromMaybe s <$> posInsertMay a s

-- | Insert the element, then get its position. Returns 'Just' if the set was updated,
-- 'Nothing' if the set didn't change and the item was already inside
posInsertMay :: Eq a => a -> Set a -> (Int, Maybe (Set a))
posInsertMay a (Set sz s) = go 0 sz s
  where
    go !n sz [] = (n, Just $ Set (sz+1) [a])
    go !n sz (x:xs)
      | x == a = (n, Nothing)
      | otherwise =
        case go (n+1) sz xs of
          (n', res) ->
            case res of
              Nothing -> (n', Nothing)
              Just (Set sz' xs') -> (n', Just $ Set sz' (x:xs'))