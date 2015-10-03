-- vim:set foldmethod=marker:

-- {{{ Pile structure
data Pile a = EmptyPile | RecurPile a (Pile a)

pile_init :: Pile a
pile_init = EmptyPile

pile_push :: Pile a -> a -> Pile a
pile_push p e = RecurPile e p

pile_pop :: Pile a -> (Maybe a, Pile a)
pile_pop  EmptyPile      = (Nothing, EmptyPile)
pile_pop (RecurPile e p) = (Just e, p)

pile_empty :: Pile a -> Bool
pile_empty EmptyPile = True
pile_empty _         = False

pile_show :: (Show a) => Pile a -> String
pile_show p = "[" ++ pile_show_elem p ++ "]"
 where pile_show_elem :: (Show a) => Pile a -> String
       pile_show_elem EmptyPile = ""
       pile_show_elem (RecurPile e EmptyPile) = show e
       pile_show_elem (RecurPile e p)         = show e ++ ", " ++ pile_show_elem p
instance Show a => Show (Pile a) where
    show = pile_show
-- }}}

-- {{{ Graph structure
data Graph a = Graph { nodes    :: [Maybe a],
                       links    :: [[Int]],
                       capacity :: Int,
                       size     :: Int
                     }

graph_init :: Graph a
graph_init = Graph [] [] 0 0

graph_add :: Graph a -> a -> Graph a
graph_add g e = if capacity g == size g then Graph (Just e : nodes g)
                                                   ([] : links g)
                                                   (1 + capacity g)
                                                   (1 + size g)
                                        else let (nds, lks) = insert e (nodes g) (links g) in
                                             Graph nds lks (capacity g) (1 + size g)
 where insert :: a -> [Maybe a] -> [[Int]] -> ([Maybe a], [[Int]])
       insert e [] _ = ([], [])
       insert e (Nothing:tl) (hd:lks) = (Just e : tl, [] : lks)
       insert e (hd1:tl1) (hd2:tl2)   = (hd1 : itl1, hd2 : itl2)
        where (itl1, itl2) = insert e tl1 tl2

graph_map :: (a -> b) -> Graph a -> Graph b
graph_map f (Graph nodes ls c s) = Graph (map f2 nodes) ls c s
 where f2 Nothing  = Nothing
       f2 (Just e) = Just $ f e

graph_acc :: (a -> b -> b) -> b -> Graph a -> b
graph_acc f acc (Graph _ _ _ 0)                    = acc
graph_acc f acc (Graph (Nothing:nds) (_:lks)  c s) = graph_acc f
                                                               acc
                                                               (Graph nds lks (c-1) s)
graph_acc f acc (Graph (Just e:nds)  (lk:lks) c s) = graph_acc f
                                                               acc2
                                                               (Graph nds lks (c-1) (s-1))
 where acc2 = f e acc
-- }}}



