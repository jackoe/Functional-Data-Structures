{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe

data Color = R | B deriving (Show, Eq)

data Tree a =  Node Color (Tree a) (Tree a) a | Empty
    deriving (Show, Eq)


null :: Ord a => Tree a -> Bool
null = (==) empty

empty :: Tree a
empty = Empty

singleton :: Ord a => a -> Tree a
singleton = Node B Empty Empty

member :: forall a. Ord a =>  a -> Tree a -> Bool
member elem = member'
    where
        member' :: Tree a -> Bool
        member' Empty = False
        member' (Node _ left right curr)
                | elem == curr = True
                | elem < curr = member' left
                | otherwise   = member' right


insert :: forall a. Ord a => a -> Tree a -> Tree a
insert elem tree = fromMaybe tree . insert' . turnBlack $ tree
    where

        insert' :: Tree a -> Maybe (Tree a)
        insert' Empty = Just $ Node R Empty Empty elem
        insert' (Node col left right curr)
          | elem < curr = recurse (\left'  -> Node col left' right curr) left
          | elem > curr = recurse (\right' -> Node col left right' curr) right
          | otherwise   = Nothing

        recurse :: (Tree a -> Tree a) -> Tree a -> Maybe (Tree a)
        recurse makeNode below = fmap (balance . makeNode) . insert' $ below

balance :: Tree a -> Tree a
balance (Node B (Node R (Node R a b x) c y) d z) = (Node R (Node B a b x) (Node B c d z) y)
balance (Node B a (Node R b (Node R c d x) y) z) = (Node R (Node B a b x) (Node B c d z) y)
balance (Node B (Node R a (Node R b c x) y) d z) = (Node R (Node B a b x) (Node B c d z) y)
balance (Node B a (Node R (Node R b c x) d y) z) = (Node R (Node B a b x) (Node B c d z) y)
balance a = a

turnBlack :: Tree a -> Tree a
turnBlack (Node _ left right elem) = Node B left right elem
turnBlack Empty = Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node _ left right elem) = (toList left) ++ elem : (toList right)

fromList :: (Foldable t, Ord a) => t a -> Tree a
fromList = foldl (flip insert) empty
