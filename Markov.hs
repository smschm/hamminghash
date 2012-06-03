module Markov where

import qualified Data.Map as M
import System.Random
import Data.List ( foldl' )
import Data.IORef
import System.IO.Unsafe

type ProbMap k = M.Map k Int
type ChainMap k1 k2 = M.Map k1 (ProbMap k2)

emptyMap = M.empty

pcRef = unsafePerformIO $ newIORef 0

addPairDebug = do
	x <- readIORef pcRef
	let x' = x + 1
	if (mod x' 10000 == 0) then putStrLn ("added pair " ++ show x') else return ()
	--putStrLn ("added pair " ++ show x')
	writeIORef pcRef x'

addPair :: (Ord a, Ord b) => (a,b) -> ChainMap a b -> ChainMap a b
addPair (k,v) m = seq (unsafePerformIO addPairDebug) (M.alter (Just . addPair') k m)
--addPair (k,v) m = seq (unsafePerformIO (print "hello")) (M.alter (Just . addPair') k m)
	where
	addPair' Nothing = M.singleton v 1
	addPair' (Just pm) = M.alter (Just . addPair'') v pm
	addPair'' Nothing = 1
	addPair'' (Just n) = n + 1
addPairs ps m = foldl' (flip addPair) m ps

pmSum :: ProbMap k -> Int
pmSum = M.fold (+) 0

selectFromPM :: ProbMap k -> Int -> k
selectFromPM p n = selectFromList (M.toList p) n
selectFromList :: [(k,Int)] -> Int -> k
selectFromList [] _ = error "selectFromList on empty list"
selectFromList ((k,v):kvs) n
	| n < v = k
	| otherwise = selectFromList kvs (n-v)
randInPM :: ProbMap k -> IO k
randInPM p = do
	let s = pmSum p
	v <- randomRIO (0,s-1)
	return $ selectFromPM p v
randInCM :: (Ord a, Ord b) => ChainMap a b -> a -> IO b
randInCM cm v = randInPM (cm M.! v)

overlapList l n = overlapList' (length l) l n
overlapList' len l n
	| len < (n+1) = []
	| otherwise = let
		dummy = unsafePerformIO (print "overlapping list")
		px = take (n) l 
		nx = head $ drop n l
	in (px,nx): overlapList' (len - 1) (tail l) n

overlapPairs (a:b:c) = (a,b) : overlapPairs (b:c)
overlapPairs _ = []
