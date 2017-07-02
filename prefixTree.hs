import qualified Data.Map.Strict as Map

data Tree = Node (Map.Map Char Tree)


addStr :: String -> Tree -> Tree
addStr "" tree = tree
addStr (c : rest) (Node treeNode)
    = Node
    . Map.insert c childToInsert
    $ treeNode
        where
            childToInsert :: Tree
            childToInsert = addStr rest child

            child :: Tree
            child = Map.findWithDefault empty c $ treeNode

extractMap :: Tree -> Map.Map Char Tree
extractMap (Node map) = map

empty = Node Map.empty

member :: String -> Tree -> Bool
member "" _ = True
member (c : rest) (Node treeNode)
  = c `Map.member` treeNode && member rest (treeNode Map.! c)

showTree :: Tree -> String
showTree (Node treeNode)
    = tail
    . init
    . show
    . map (showTree . snd)
    . Map.toList
    $ treeNode
