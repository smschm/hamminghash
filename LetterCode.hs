import Hamming
import Markov
import qualified Data.Map as M 
import Data.Maybe ( fromJust, catMaybes )
import Data.Binary
import System.IO.Unsafe
import Data.Char ( toLower )
import System.Environment ( getArgs )
import qualified Data.ByteString.Lazy as BS


-- character-based

data Atom = Begin | Word Char | End
	deriving (Ord, Eq, Show)

unAtom Begin = ' '
unAtom End = ' '
unAtom (Word w) = w

renderAtoms = map unAtom

{-
-- word-based
data Atom = Begin | Word String | End
	deriving (Ord, Eq, Show)

unAtom Begin = ""
unAtom End = ""
unAtom (Word w) = w

renderAtoms = unwords . (map unAtom)
-}

instance Binary Atom where
	put Begin = putWord8 0
	put End = putWord8 255
	put (Word w) = putWord8 1 >> put w
	get = do
		t <- get :: Get Word8
		case t of
			0 -> return Begin
			255 -> return End
			1 -> do
				s <- get
				return $ Word s

makeChainFromFile fn order = do
	c <- readFile fn
	print (take 20 c)
	--let w = map (Word . (map toLower)) (words c)
	let w = map (Word . toLower) c
	print $ head w
	let l = overlapList ((replicate order Begin) ++ w ++ [End]) order
	print $ length l
	let m' = addPairs l emptyMap
	print "ha"
	return m'

makeCodeFromChain c = fmap (makeHamming . M.assocs) c

decodeOneWithChain :: (Ord a) => [a] -> -- current state
	M.Map [a] (BinTree a) -> -- chain
	[Bool] -> -- stream
	(Maybe a, [Bool], [a]) -- (result, remainder of stream, new state)
decodeOneWithChain st ch stream = let
	curCode = ch M.! st
	(p, rem) = decodeOne curCode stream
	in (p, rem, (tail st) ++ [fromJust p])

decodeStreamWithChain st ch stream
	| stream == [] = []
	| otherwise = let (p, rem, st') = decodeOneWithChain st ch stream in
		p : decodeStreamWithChain st' ch rem

test ifn ofn order = do
	ch <- makeChainFromFile ifn order
	putStrLn $ seq ch "made chain"
	--return $ decodeOneWithChain [Begin] (makeCodeFromChain ch) stream
	let code = makeCodeFromChain ch
        let x = code
	putStrLn $ seq code "made code"
	--a <- getArgs
	--let stream = hexToStream $ head a
	let stream = replicate 40 False
	BS.writeFile ofn $ encode code
	--print code
	return $ decodeStreamWithChain (replicate order Begin) code stream

testBinary fn hex = do
	c <- BS.readFile fn
        let order = (read [last fn]) :: Int
	-- a <- getArgs
	let code = decode c
	let stream = hexToStream hex
	return $ decodeStreamWithChain (replicate (fromIntegral order) Begin) code stream

main = do
     a <- getArgs
     x <- case a of
       (('c':[]):ifn:ofn:ord:[]) -> test ifn ofn (read ord :: Int)
       (path:hex:[]) -> testBinary path hex
       _ -> putStrLn "usage:\n c <ifn> <ofn> <order>\n <path> <hex>\n" >> return []
     putStrLn $ renderAtoms $ catMaybes x
