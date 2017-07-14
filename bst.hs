{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe

data Tree a =  Node (Tree a) (Tree a) a | Empty
    deriving (Show, Eq)

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
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
insert elem tree = if insert'' == Empty then tree else insert''
    where

        insert'' :: Tree a
        insert'' = fromMaybe Empty . insert' $ tree

        insert' :: Tree a -> Maybe (Tree a)
        insert' Empty = Node Empty Empty elem
        insert' (Node left right curr)
          | elem < curr = Just
                        . fmap (\left'  -> Node left' right  curr)
                        . insert'
                        $ left
          | elem > curr = Just
                        . fmap (\right' -> Node left  right' curr)
                        . insert'
                        $ right
          | otherwise = Nothing

fromList :: (Foldable t, Ord a) => t a -> Tree a
fromList = foldl (flip insert) empty
