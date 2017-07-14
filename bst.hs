{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe

data Tree a =  Node (Tree a) (Tree a) a | Empty
    deriving (Show, Eq)

null :: Ord a => Tree a -> Bool
null = (==) empty

empty :: Tree a
empty = Empty

singleton :: Ord a => a -> Tree a
singleton = Node Empty Empty

member :: forall a. Ord a =>  a -> Tree a -> Bool
member elem = member'
    where

        member' :: Tree a -> Bool
        member' Empty = False
        member' (Node left right curr)
                | elem == curr = True
                | elem < curr = member' left
                | otherwise   = member' right


insert :: forall a. Ord a => a -> Tree a -> Tree a
insert elem tree = fromMaybe tree . insert' $ tree
    where

        insert' :: Tree a -> Maybe (Tree a)
        insert' Empty = Just $ Node Empty Empty elem
        insert' (Node left right curr)
          | elem < curr = fmap (\left'  -> Node left' right curr)
                        . insert'
                        $ left
          | elem > curr = fmap (\right' -> Node left right' curr)
                        . insert'
                        $ right
          | otherwise = Nothing

fromList :: (Foldable t, Ord a) => t a -> Tree a
fromList = foldl (flip insert) empty
