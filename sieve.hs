-- adapted from the https://www.garrisonjensen.com/2015/05/13/haskell-programs-are-lies.html
import qualified Data.Map as Map
import qualified Data.Set as PQ

-- Not elegant, but short sieve
primes = sieve [2..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
-- Matter of fact is a trial division

-- trial division method explicit
primesT    = 2 : [x | x <- [3..], isprimeT x]
isprimeT x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where
    factorsToTry x = takeWhile (\p -> p*p <= x) primesT

-- True Erastosthenes sieve
-- With Map data structure
-- Map.empty = fromList []
-- Map.insertWith fn key el
primesE = sieveM [2..]
sieveM xs = sieve' xs Map.empty
  where
    sieve' [] table     = []
    sieve' (x:xs) table =
      case Map.lookup x table of
      -- Table receives the square of the key x 
      Nothing      -> x : sieve' xs (Map.insert (x*x) [x] table)
      Just factors -> sieve' xs (foldl reinsert (Map.delete x table) factors)
        where
          reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

primesQ = 2:sieve [3,5..]
  where
    sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)
    sieve' (x:xs) table
         | nextComposite == x = sieve' xs (adjust x table)
         | otherwise          = x : sieve' xs (insertprime x xs table)
           where 
             (nextComposite,_) = PQ.findMin table

    adjust x table
         | n == x    = adjust x (PQ.insert (n', ns) newPQ)
         | otherwise = table
           where
             Just ((n, n':ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p*p, map (*p) xs)
