
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
test1 = "writers"
test2 = "vintner"
--optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]

type AlignmentType = (String,String)

--similarityScore :: String -> String -> Int
--similarityScore string1 string2


-- This method takes two values h1 and h2 and attaches them first in 2 lists in the list tuple aList
--attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
--attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = [c | c <- xs, valueFcn c == maximum (map valueFcn xs)]




--optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]


--outputOptAlignments string1 string2 
score :: Eq a => a -> a-> Int
score a b 
	| a == b = scoreMatch
	| otherwise =  scoreMismatch

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
	where
	mcsLen i j = mcsTable!!i!!j
	mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
			 
	mcsEntry :: Int -> Int -> Int
	mcsEntry x 0 = scoreSpace * x
	mcsEntry 0 y = scoreSpace * y
	mcsEntry i j = maximum [score x y + mcsLen (i-1) (j-1), mcsLen i (j-1) + scoreSpace,mcsLen (i-1) j + scoreSpace]
		where
		x = xs!!(i-1)
		y = ys!!(j-1)