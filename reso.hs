-- vim:set foldmethod=marker:

import Data.Array

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
-- Graph to be build with a tying the knot method, can't handle unconnected components
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

-- }}}


