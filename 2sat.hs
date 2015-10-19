-- vim:set foldmethod=marker:

import Data.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Text.Lazy (unpack)

-- Data types
data Prob = Prob1 Int
          | Prob2 Int Int

sprob :: Prob -> String
sprob (Prob1 i)     = "(p " ++ show i ++ ")"
sprob (Prob2 i1 i2) = "(p " ++ show i1 ++ " || " ++ show i2 ++ ")"
instance Show Prob where
    show = sprob

-- Building the graph
tograph :: [Prob] -> [(Int, Int)]
tograph []                = []
tograph (Prob1 i : t)     = (-i, i) : tograph t
tograph (Prob2 i1 i2 : t) = (-i1, i2) : (-i2, i1) : tograph t

adapt :: [Int] -> [(Int,Int)] -> [(Int, Int, [Int])]
adapt []       _  = []
adapt (n : t) lks = (n, n, tlks) : adapt t lks
 where tlks = [b | (a,b) <- lks, a == n]

readprob :: [String] -> [Prob]
readprob []    = []
readprob (s:t) = case words s of
                   (i:[])     -> Prob1 (read i)            : readprob t
                   (i1:i2:[]) -> Prob2 (read i1) (read i2) : readprob t
                   _          ->                             readprob t

lmax :: Ord a => [a] -> a
lmax []     = error "max /O has no sense"
lmax (h:[]) = h
lmax (h:t)  = max h $ lmax t

range :: (Enum a, Eq a) => a -> a -> a -> [a]
range b e x | b == e = [b]
range b e x | b == x = range (succ b) e b
range b e x          = b : range (succ b) e x

buildg :: [String] -> ([(Int, Int, [Int])], Int)
buildg ss = (adapt (range (-n) n 0) (tograph pbs), n)
 where pbs = readprob ss
       n = lmax $ map nbfy pbs
       nbfy (Prob1 i)     = abs i
       nbfy (Prob2 i1 i2) = max (abs i1) (abs i2)

mscc :: [(Int, Int, [Int])] -> [[Int]]
mscc g = map unnode nodes
 where nodes = stronglyConnComp g
       unnode :: SCC Int -> [Int]
       unnode (AcyclicSCC v) = [v]
       unnode (CyclicSCC vs) = vs

-- Checking the components
checkSCC :: [Int] -> Bool
checkSCC a = check $ map abs a
 where check :: [Int] -> Bool
       check [] = True
       check (h:[]) = True
       check (h:t)  = if length eqq >= 1 then False
                                         else check sub && check upp
        where sub = [x | x <- t, x < h]
              upp = [x | x <- t, x > h]
              eqq = [x | x <- t, x == h]

checkSCCs :: [[Int]] -> Bool
checkSCCs p = foldr chck True p
 where chck :: [Int] -> Bool -> Bool
       chck a p = p && checkSCC a

sat :: [String] -> (Bool, String)
sat strs = (checkSCCs $ mscc $ g, dotGraph g)
 where (g,_) = buildg strs

-- Reading file
linesof :: FilePath -> IO [String]
linesof fp = do ct <- readFile fp
                return $ lines ct

-- Graphviz
has :: Int -> [Int] -> Bool
has _ []    = False
has i (h:t) = if i == h then True else has i t

graphvizgraph :: [(Int, Int, [Int])]
              -> Data.Graph.Inductive.PatriciaTree.Gr Int ()
graphvizgraph g = mkGraph nds links
 where nodes = [i | (i,_,_) <- g]
       nds   = [(i,i) | i <- nodes]
       links = [(i,j,()) | (i,_,d) <- g, j <- nodes, has j d]

dotGraph :: [(Int, Int, [Int])] -> String
dotGraph g = unpack $ renderDot $ toDot $ graphToDot nonClusteredParams gg
 where gg = graphvizgraph g

-- Output
result :: Bool -> String
result True  = "Problem solvable"
result False = "Problem unsolvalble"

main = do lns <- linesof "pbsat"
          let (r, g) = sat lns
          writeFile "outputGraph" g
          putStrLn $ result r

