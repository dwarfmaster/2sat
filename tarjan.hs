-- vim:set foldmethod=marker:
{-# LANGUAGE ViewPatterns #-}

import Data.Array
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Example

-- {{{ Tree structure
data Tree a = Empty | Branch a [Tree a]
tshow :: Show a => Tree a -> String
tshow Empty = "Empty"
tshow (Branch a l) = "(Branch " ++ show a ++ " " ++ show l ++ ")"
instance Show a => Show (Tree a) where
    show = tshow

tempty :: Tree a -> Bool
tempty Empty = True
tempty _     = False

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Branch v ts) = concatMap postOrder ts ++ [v]

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Empty         = Empty
tmap f (Branch v ts) = Branch (f v) $ map (tmap f) ts

-- }}}

-- {{{ Misc function
-- Return all nodes and their labels
nds :: Graph gr => gr a b -> [(Int,a)]
nds (isEmpty -> True)            = []
nds (matchAny -> ((_,n,l,_), g)) = (n,l) : nds g

-- Get the succesors of a context
msuc :: Context a b -> [Int]
msuc (_,_,_,scs) = map snd scs

-- A special kind of mapping with an accumulator and excluding values
rd ::    (a -> b -> (b,c)) -- The mapping function, b is the accumulator
      -> b                 -- The first value of the accumulator
      -> (c -> Bool)       -- The excluding function : any recognised mapped value is excluded
      -> [a]               -- The list of values to map on
      -> ([c],b)           -- The map of values and the la value of the accumulator
rd _ g e []    = ([],g)
rd f g e (h:t) = if e r then (rc, rrg) else (r : rc, rrg)
 where (rg,r)   = f h g
       (rc,rrg) = rd f rg e t

-- Mapping on graph
mgmap :: DynGraph gr => (Context a b -> Context c d) -> gr a b -> gr c d
mgmap _ (isEmpty -> True)   = empty
mgmap f (matchAny -> (c,g)) = (f c) & mgmap f g

-- Swapping froms and tos of a context
swap :: Context a b -> Context a b
swap (f,n,l,t) = (t,n,l,f)

-- Reversing an oriented graph
mgrev :: DynGraph gr => gr a b -> gr a b
mgrev = mgmap swap
-- }}}

-- {{{ Deep-first algorithm
-- DFS from the node n
dfs :: Graph gr => Int -> gr a b -> (Tree (Int,a), gr a b)
dfs n g@(match n -> (Nothing, _)) = (Empty, g)
dfs n (match n -> (Just c, g))    = (Branch (n,l) ar, rg)
 where (_,n,l,_) = c
       rdf i g   = let (rt,rg) = dfs i g in (rg,rt)
       (ar,rg)   = rd rdf g tempty $ msuc c

-- DFS forest
dff :: Graph gr => gr a b -> [Tree (Int,a)]
dff (isEmpty -> True)              = []
dff g@(matchAny -> ((_,n,_,_), _)) = t : dff rg
 where (t,rg) = dfs n g
-- }}}

-- {{{ Topological sorting
toposort :: Graph gr => gr a b -> [(Int,a)]
toposort = reverse . concatMap postOrder . dff
-- }}}

