-- Haskell hamming code library

module Hamming where --( BinTree, makeHamming, printCode, decodeStream, decodeOne, octToStream ) where

import Data.List ( span )
import Data.Binary

data BinTree a = Fork !(BinTree a) !(BinTree a) | Leaf !a
	deriving (Show, Eq)


instance (Binary a) => Binary (BinTree a) where
	put (Leaf x) = putWord8 1 >> put x
	put (Fork l r) = putWord8 0 >> put l >> put r
	get = do
		t <- get :: Get Word8
		case t of
			0 -> do
				l <- get
				r <- get
				return $ Fork l r
			1 -> do
				x <- get
				return $ Leaf x



makeHamming :: (Num n, Ord n) => [(a, n)] -> BinTree a
makeHamming x = let
	leafize z = (Leaf (fst z), snd z)
	leaves = map leafize x
	steps = iterate hammingStep leaves
	in fst $ head $ steps !! (length x - 1)

decodeStream :: BinTree a -> [Bool] -> [a]
decodeStream x l = let
	ds' x (Leaf k) l = k : ds' x x l
	ds' x _ [] = []
	ds' x (Fork tl tr) (l:ls) = ds' x (if l then tl else tr) ls
	in ds' x x l

decodeOne :: BinTree a -> [Bool] -> (Maybe a, [Bool])
decodeOne (Leaf k) l = (Just k, l)
decodeOne _ [] = (Nothing, [])
decodeOne (Fork tl tr) (l:ls) = decodeOne (if l then tl else tr) ls

octToStream x = x >>= doct where
	doct l = case l of
		'0' -> [False, False, False]
		'1' -> [False, False, True]
		'2' -> [False, True, False]
		'3' -> [False, True, True]
		'4' -> [True, False, False]
		'5' -> [True, False, True]
		'6' -> [True, True, False]
		'7' -> [True, True, True]
		_ -> error "balls"

hexToStream x = x >>= dhex where
	dhex l = case l of
		'0' -> [False, False, False, False]
		'1' -> [False, False, False, True]
		'2' -> [False, False, True, False]
		'3' -> [False, False, True, True]
		'4' -> [False, True, False, False]
		'5' -> [False, True, False, True]
		'6' -> [False, True, True, False]
		'7' -> [False, True, True, True]
		'8' -> [True, False, False, False]
		'9' -> [True, False, False, True]
		'a' -> [True, False, True, False]
		'b' -> [True, False, True, True]
		'c' -> [True, True, False, False]
		'd' -> [True, True, False, True]
		'e' -> [True, True, True, False]
		'f' -> [True, True, True, True]
		_ -> error "balls"

printCode :: (Show a) => BinTree a -> String
printCode = unlines . (printCode' "")
	where printCode' p (Leaf l) = [p ++ ": " ++ show l]
	      printCode' p (Fork l r) = printCode' ('0':p) l ++ printCode' ('1':p) r

-- start out with all leaves, and in steps combine the least-two into a new tree until there is
-- only one tree:

hammingStep :: (Num n, Ord n) => [(BinTree a, n)] -> [(BinTree a, n)]
hammingStep x = let
	(s0, f0, b0) = splitOnSmallest x
	(s1, f1, b1) = splitOnSmallest (f0 ++ b0)
	in (Fork (fst s0) (fst s1), snd s0 + snd s1) : (f1 ++ b1)

-- split a list into (smallest element, [elements to left of smallest], [elements to right])
splitOnSmallest :: (Ord n) => [(a, n)] -> ((a, n), [(a,n)], [(a,n)])
splitOnSmallest [] = error "splitOnSmallest []"
splitOnSmallest [x] = (x,[],[])
splitOnSmallest x = let
	m = minimum (map snd x)
	(front, back) = span ((/= m) . snd) x
	in (head back, front, tail back)

testDoc = zip ['a'..'z']
	[816,149,278,425,1270,
	 223,201,609,697,15,
	 77,403,241,675,751,
	 193,10,599,633,906,
	 276,98,236,15,197,7]
testCode = makeHamming testDoc
