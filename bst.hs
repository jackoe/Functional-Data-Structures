data Tree a =  Node (Tree a) (Tree a) a | Empty
    deriving (Show, Eq)

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
singleton = Node Empty Empty

member :: Ord a => a -> Tree a -> Bool
member elem = member'
    where
--        member' :: Ord a => Tree a -> Bool
        member' Empty = False
        member' (Node left right curr)
            | elem == curr = True
            | elem < curr = member' left
            | otherwise   = member' right


insert :: Ord a => a -> Tree a -> Tree a
insert elem tree = if insert'' == Empty then tree else insert''
    where
        insert'' = insert' tree

        insert' Empty = Node Empty Empty elem
        insert' (Node left right curr)
          | elem < curr = nodeLeft (insert' left) right  curr
          | elem > curr = nodeRight left (insert' right) curr
          | otherwise = Empty

        nodeRight left Empty curr = Empty
        nodeRight left right curr = Node left right curr

        nodeLeft Empty right curr = Empty
        nodeLeft left  right curr = Node left right curr


fromList :: (Foldable t, Ord a) => t a -> Tree a
fromList = foldl (flip insert) empty
