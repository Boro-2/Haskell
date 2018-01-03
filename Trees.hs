(/>):: a -> (a-> b) -> b
x />f = f x

data Tree = Node Integer Tree Tree | Stub

-- hlavni funkce show je realizovana funkci showTree
instance Show Tree where 
        show x = showTree x

instance Eq Tree where
         (==) = eqTree

t1 = (Node 5 (Node 1 Stub Stub) (Node 3 (Node 42 Stub Stub) Stub))
t2 = (Node 5 (Node 1 Stub Stub) (Node 3 (Node 41 Stub Stub) Stub))

treeSize :: Tree -> Int
treeSize Stub = 0
treeSize (Node _ lb rb) = 1 + (treeSize lb) + (treeSize rb)

showTree :: Tree -> String
showTree Stub = ""
showTree (Node i lb rb) = "("++(showTree lb) ++ (show i) ++(showTree rb)++ ")"

eqTree:: Tree -> Tree -> Bool
eqTree Stub Stub = True
eqTree (Node v1 l1 r1) (Node v2 l2 r2) = (v1 == v2) && (eqTree l1 l2) && (eqTree r1 r2)
eqTree _ _ = False

insertInT :: Integer -> Tree -> Tree
insertInT n Stub = (Node n Stub Stub)
insertInT n (Node v lb rb) | n <= v = (Node v (insertInT n lb) rb)
                        | otherwise = (Node v lb (insertInT n rb))
                        
listToTree :: [Integer] -> Tree
listToTree values = foldr insertInT Stub values


--druhy  paramatr je akumulator
treeToList' ::  Tree -> [Integer] ->  [Integer]
treeToList' Stub s  = s
treeToList' (Node value lb rb) s= s /> treeToList' lb /> (value:) /> treeToList' rb

treeToList :: Tree -> [Integer]
treeToList t = t /> (flip treeToList') [] /> reverse

