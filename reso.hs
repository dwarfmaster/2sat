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
-- Inductive graph structre, plainly a list of concepts
data Node a    = Node { key :: Int, value :: a }
type Edge      = Int
data Context a = Context [Edge] (Node a) [Edge]
data Graph a   = Empty
               | Context a :& Graph a

-- Text the emptiness of the graph O(1)
is_empty :: Graph a -> Bool
is_empty Empty = True
is_empty _     = False

-- Extract a context O(1)
match_any :: Graph a -> (Context a, Graph a)
match_any Empty    = error "matchAny : empty graph"
match_any (c :& g) = (c, g)

-- Extract a particular context matching f O(N)
match :: (Context a -> Bool) -> Graph a -> (Maybe (Context a), Graph a)
match f g = match_rec g Empty
 where -- match_rec :: Graph a -> Graph a -> (Maybe (Context a), Graph a)
       match_rec Empty ac = (Nothing, ac)
       match_rec (c :& g2) ac = if f c
                                  then (Just c, concatg g2 ac)
                                  else match_rec g2 (c:&ac)

match_by_key :: Int -> Graph a -> (Maybe (Context a), Graph a)
match_by_key i = match (mkey i)
 where mkey :: Int -> Context a -> Bool
       mkey i (Context _ n _) = key n == i

match_by_value :: Eq a => a -> Graph a -> (Maybe (Context a), Graph a)
match_by_value a = match (mval a)
 where -- mval :: a -> Context a -> Bool
       mval v (Context _ n _) = value n == a

-- Join two graphs, for internal use only O(N1)
concatg :: Graph a -> Graph a -> Graph a
concatg Empty   g2    = g2
concatg g1      Empty = g1
concatg (c:&g1) g2    = concatg g1 (c:&g2)

-- Map a value against all node O(N)
mapg :: (a -> b) -> Graph a -> Graph b
mapg _ Empty  = Empty
mapg f (c:&g) = Context lk1 (Node (key nd) (f (value nd))) lk2 :& mapg f g
 where Context lk1 nd lk2 = c

-- Build a list from a hash function, an array of nodes and an array of edges
build :: Eq a => (a -> Int) -> [a] -> [(a, a)] -> Graph a
build _ []     _  = Empty
build h (n:ns) ls = Context lk1 (Node (h n) n) lk2 :& build h ns ls
 where lk1 = [h e | (e, f) <- ls, f == n]
       lk2 = [h e | (f, e) <- ls, f == n]

-- }}}


