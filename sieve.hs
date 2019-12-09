import Data.Map (Map)
import qualified Data.Map as Map

-- Not elegant, but short sieve
primes = sieve [2..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
-- Matter of fact is a trial division

-- True Erastothenes sieve
-- With Map data structure
-- Map.empty = fromList []
-- Map.insertWith fn key el
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
              

