scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)

score :: Eq a => a -> a-> Int
score a b 
	| a == b = scoreMatch
	| otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore xs ys = len (length xs) (length ys)
	where
	len i j = table!!i!!j
	table = [[ entry i j | j<-[0..]] | i<-[0..] ]
			 
	entry :: Int -> Int -> Int
	entry x 0 = scoreSpace * x
	entry 0 y = scoreSpace * y
	entry i j = maximum [a,b,c]
		where
		x = xs!!(i-1)
		y = ys!!(j-1)
		a = score x y + len (i-1) (j-1)
		b = len i (j-1) + scoreSpace
		c = len (i-1) j + scoreSpace


--This method takes two values h1 and h2 and attaches them first in 2 lists in the list tuple aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [c | c <- xs, valueFcn c== maximum (map valueFcn xs)]

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = snd (len (length xs) (length ys))
	where
	len i j = table!!i!!j
	table = [[ entry i j | j<-[0..]] | i<-[0..] ]
	
	reduceX :: Int -> (Int, [AlignmentType])
	reduceX 0 = (0,[("","")])
	reduceX x = (scoreSpace + fst (len (x-1) 0), attachTails (xs!!(x-1)) '-' (snd (entry (x-1) 0)))
	reduceY :: Int -> (Int, [AlignmentType])
	reduceY 0 = (0,[("","")])
	reduceY y = (scoreSpace + fst (len 0 (y-1)), attachTails '-' (ys!!(y-1)) (snd (entry 0 (y-1) )))
	
	entry :: Int -> Int -> (Int, [AlignmentType])
	entry x 0 = reduceX x
	entry 0 y = reduceY y
	entry i j = (fst (head maxList), concat (map snd maxList))
		where
		maxList = (maximaBy fst [a,b,c])
		x = xs!!(i-1)
		y = ys!!(j-1)
		a = (score x y + fst (len (i-1) (j-1)), attachTails x y (snd  (entry (i-1) (j-1))))
		b = (fst (len i (j-1)) + scoreSpace, attachTails '-' y (snd (entry i (j-1))))
		c = (fst (len (i-1) j) + scoreSpace, attachTails x '-' (snd (entry (i-1) j)))

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
	putStr ("There are " ++ show (length xs) ++ " optimal alignments.\n\n")
	mapM_ printString xs
	where
	xs = optAlignments string1 string2
	printString :: AlignmentType ->IO ()
	printString al = putStr ((fst al) ++ "\n" ++ (snd al) ++ "\n---------------------------\n")