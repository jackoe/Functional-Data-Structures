import qualified Data.Map.Strict as Map

data Tree = Node Bool (Map.Map Char Tree)


addStr :: String -> Tree -> Tree
addStr "" (Node _ treeNode) = Node True treeNode
addStr (c : rest) (Node end treeNode)
    = Node end
    . Map.insert c childToInsert
    $ treeNode
        where
            childToInsert :: Tree
            childToInsert = addStr rest child

            child :: Tree
            child = Map.findWithDefault empty c $ treeNode

extractMap :: Tree -> Map.Map Char Tree
extractMap (Node _ map) = map

empty = Node False Map.empty

member :: String -> Tree -> Bool
member "" (Node end treeNode) = end
member (c : rest) (Node _ treeNode)
  = c `Map.member` treeNode && member rest (treeNode Map.! c)

showTree :: Tree -> String
showTree (Node _ treeNode)
    = tail
    . init
    . show
    . map (showTree . snd)
    . Map.toList
    $ treeNode

fromList :: [String] -> Tree
fromList = foldl (flip addStr) empty
