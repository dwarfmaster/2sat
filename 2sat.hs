-- vim:set foldmethod=marker:

import Data.Graph

data Prob = Prob1 Int
          | Prob2 Int Int

sprob :: Prob -> String
sprob (Prob1 i)     = "(p " ++ show i ++ ")"
sprob (Prob2 i1 i2) = "(p " ++ show i1 ++ " || " ++ show i2 ++ ")"
instance Show Prob where
    show = sprob

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

sat :: [String] -> Bool
sat strs = checkSCCs $ mscc $ g
 where (g,_) = buildg strs

result :: Bool -> String
result True  = "Problem solvable"
result False = "Problem unsolvalble"

linesof :: FilePath -> IO [String]
linesof fp = do ct <- readFile fp
                return $ lines ct

main = do lns <- linesof "pbsat"
          putStrLn $ result $ sat lns

