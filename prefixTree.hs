import qualified Data.Map.Strict as Map

data Tree a = Node Bool (Map.Map a (Tree a)) deriving Show


addStr :: Ord a => [a] -> Tree a -> Tree a
addStr [] (Node _ treeNode) = Node True treeNode
addStr (c : rest) (Node end treeNode)
    = Node end
    . Map.insert c childToInsert
    $ treeNode
        where
            childToInsert = addStr rest child

            child = Map.findWithDefault empty c $ treeNode

extractMap :: Ord a => Tree a -> Map.Map a (Tree a)
extractMap (Node _ map) = map

empty = Node False Map.empty

member :: Ord a => [a] -> Tree a -> Bool
member [] (Node end treeNode) = end
member (c : rest) (Node _ treeNode)
  = c `Map.member` treeNode && member rest (treeNode Map.! c)

fromList :: (Ord a, Foldable t) => t [a] -> Tree a
fromList = foldl (flip addStr) empty
