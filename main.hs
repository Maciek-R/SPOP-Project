import System.IO
import System.Exit

printWords [] = return ()
printWords (x:xs) = do
			putStrLn x
			printWords (xs)
			
printBoards [] [] = return ()	
printBoards (x:xs) (y:ys) = do
							putStrLn $ x ++ "   " ++ y
							printBoards xs ys
		
nthElem (x:_) 0 = x
nthElem (_:xs) n = nthElem xs (n-1)		

--TODO
rowsWithoutUsedWords rows usedWords = rows
--

getLetterFromBoard rows y x = nthElem (nthElem rows x) y

crossLetterFromLine [] _ = []
crossLetterFromLine (x:xs) 0 = '-':(crossLetterFromLine xs (-1))
crossLetterFromLine (x:xs) n = x:(crossLetterFromLine xs (n-1))

crossLetterOnBoard [] _ _ = []
crossLetterOnBoard (row:rows) x y | y==0 = (crossLetterFromLine row x):(crossLetterOnBoard rows (y-1) x)
								  | otherwise = row:(crossLetterOnBoard rows (y-1) x)
								  
exampleCrossedBoard board = 
	(crossLetterOnBoard (crossLetterOnBoard (crossLetterOnBoard board 3 0) 2 0) 1 0)
							
boardFileName nr |  nr=="1" = "board_1.txt"
				 |  nr=="2" = "board_2.txt"
				 |  nr=="3" = "board_3.txt"
				 
wordsFileName nr |  nr=="1" = "words_1.txt"
				 |  nr=="2" = "words_2.txt"
				 |  nr=="3" = "words_3.txt"
	
mainPuzzle nr = do
				boardIO <- readFile (boardFileName nr) 
				wordsIO <- readFile (wordsFileName nr) 
				--checking files TODO
				--algorithm TODO
				let board = lines boardIO
				let words = lines wordsIO
				let usedWords = [nthElem words 0, nthElem words 5, nthElem words 7]
				printBoards board (rowsWithoutUsedWords board usedWords)
				putStrLn ""
				putStrLn "All words:"
				printWords words
				putStrLn ""
				putStrLn "Used words"
				printWords usedWords
				putStrLn ""
				putStrLn "Example:"
				printBoards (exampleCrossedBoard board) board
		
main =	do
		putStrLn "1. Puzzle1"
		putStrLn "2. Puzzle2"
		putStrLn "3. Puzzle3"
		putStrLn "0. Wyjscie"
		putStrLn "Wybierz zagadke: "
		nr <- getLine
		if nr == "0" 
			then return ()
		else if not (nr == "1" || nr == "2" || nr == "3")
			then main
		else 
			mainPuzzle nr
				