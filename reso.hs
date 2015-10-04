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
data Vertex a = Vertex { key   :: a,
                         links :: [Int]
                       }
instance Show a => Show (Vertex a) where
    show v = "(" ++ show (key v) ++ ":" ++ show (links v) ++ ")"
type Graph a = Array Int (Vertex a)

graph_build :: [(a, [Int])] -> Graph a
graph_build l = listArray (1,length l) $ map (\(e, t) -> Vertex e t) l

graph_convert :: (a -> b) -> Graph a -> Graph b
graph_convert f g = fmap cvt g
 where cvt (Vertex k lks) = Vertex (f k) lks
-- }}}

-- {{{ Tarjan algorithm
data TrVx a = TrVx { num   :: Int,
                     numa  :: Int,
                     inP   :: Bool,
                     value :: a
                   }
type TrPile a  = Pile (Vertex (TrVx a))
type TrGraph a = Graph (TrVx a)

trj_cvt :: a -> TrVx a
trj_cvt e = TrVx (-1) (-1) True e

tarjan :: Graph a -> [[a]]
tarjan g = [[]] -- TODO

parcours :: TrPile a -> [[a]] -> TrGraph a -> Int -> (TrPile a, [[a]], TrGraph a)
parcours p part gr vx = 

-- }}}


