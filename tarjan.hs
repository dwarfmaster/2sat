-- vim:set foldmethod=marker:

import Data.Array
import Data.Maybe

-- {{{ Pile structure
data Pile a = EmptyPile | a :< (Pile a)

pile_init :: Pile a
pile_init = EmptyPile

pile_push :: Pile a -> a -> Pile a
pile_push p e = e :< p

pile_pop :: Pile a -> (Maybe a, Pile a)
pile_pop  EmptyPile = (Nothing, EmptyPile)
pile_pop (e :< p)   = (Just e, p)

pile_empty :: Pile a -> Bool
pile_empty EmptyPile = True
pile_empty _         = False

pile_show :: Show a => Pile a -> String
pile_show p = "[" ++ pile_show_elem p ++ "]"
 where pile_show_elem :: (Show a) => Pile a -> String
       pile_show_elem EmptyPile = ""
       pile_show_elem (e :< EmptyPile) = show e
       pile_show_elem (e :< p)         = show e ++ ", " ++ pile_show_elem p
instance Show a => Show (Pile a) where
    show = pile_show

pile_has :: Eq a => Pile a -> a -> Bool
pile_has EmptyPile _ = False
pile_has (h :< t)  e = if h == e then True else pile_has t e

pile_get :: (b -> a -> Bool) -> b -> Pile a -> Maybe a
pile_get _ _ EmptyPile = Nothing
pile_get e m (h :< t)  = if e m h then Just h else pile_get e m t
-- }}}

-- {{{ Graph structure
-- Graph elements to be build with a tying the knot method
data EGraph a = EGraph a [EGraph a]
-- Real Graph structure, a list of all nodes
data Graph a  = Empty
              | EGraph a :& Graph a

-- TODO : make it really cyclic
build :: Eq a => [a] -> [(a,a)] -> Graph a
build []    _   = Empty
build (h:t) lks = ebuild h lks :& build t lks
 where ebuild :: Eq a => a -> [(a,a)] -> EGraph a
       ebuild e lks = EGraph e lgs
        where lgs = [ebuild b lks | (a,b) <- lks, a == e]

mapg :: (a -> b) -> Graph a -> Graph b
mapg _ Empty     = Empty
mapg f (eg :& g) = mg f eg :& mapg f g
 where mg :: (a -> b) -> EGraph a -> EGraph b
       mg f (EGraph e sg) = EGraph (f e) $ map (mg f) sg

showg :: Show a => Graph a -> String
showg Empty = ""
showg (EGraph e egs :& g) = show e ++ " -> " ++ showegs egs ++ "\n" ++ showg g
 where showegs :: Show a => [EGraph a] -> String
       showegs lst = foldl fld "[ " lst ++ "]"
        where fld :: Show a => String -> EGraph a -> String
              fld str (EGraph e _) = str ++ show e ++ " "
instance Show a => Show (Graph a) where
    show = showg

eqegraph :: Eq a => EGraph a -> EGraph a -> Bool
eqegraph (EGraph e1 _) (EGraph e2 _) = e1 == e2
instance Eq a => Eq (EGraph a) where
    (==) = eqegraph

-- }}}

-- {{{ Tarjan algorithm
tar_mtch :: Eq a => EGraph a -> (EGraph a, Int) -> Bool
tar_mtch (EGraph e1 _) ((EGraph e2 _),_) = e1 == e2

-- The first argument is the graph
-- The second is the list of seen nodes
-- The third argument is the number for parcours
-- It return the strongly connected components
tar_iter :: Eq a => Graph a
                 -> Pile (EGraph a, Int)
                 -> Int
                 -> [[a]]
tar_iter Empty     _ _ = []
tar_iter (eg :& g) p n = if isJust $ pile_get tar_mtch eg p
                           then tar_iter g p n
                           else scc : tar_iter g np nn
 where (scc, np, nn) = parcours eg p n

parcours :: Eq a => EGraph a
                 -> Pile (EGraph a, Int)
                 -> Int
                 -> ([a], Pile (EGraph a, Int), Int)
parcours (EGraph v sgs) p n = ([], EmptyPile, 0) -- TODO
-- }}}

