import System.IO
import qualified Data.Set as Set
import qualified Data.List as List

main =  do
        putStrLn "1. Puzzle1"
        putStrLn "2. Puzzle2"
        putStrLn "3. Puzzle3"
        putStrLn "Wybierz zagadke: "
        nr <- getLine
        boardIO <- readFile (boardFileName nr) 
        wordsIO <- readFile (wordsFileName nr) 
        --checking files TODO
        let the_board = lines boardIO
        let the_words = lines wordsIO
        printWords the_words
        putStrLn $ " "
        printBoards the_board (crossIndicesFromBoard the_board (findIndicesToCross the_words the_board))

printWords [] = return ()
printWords (x:xs) = do
            putStrLn x
            printWords (xs)
            
printBoards [] [] = return ()    
printBoards (x:xs) (y:ys) = do
                            putStrLn $ x ++ "   " ++ y
                            printBoards xs ys

boardFileName nr |  nr=="1" = "board_1.txt"
                 |  nr=="2" = "board_2.txt"
                 |  nr=="3" = "board_3.txt"
                 
wordsFileName nr |  nr=="1" = "words_1.txt"
                 |  nr=="2" = "words_2.txt"
                 |  nr=="3" = "words_3.txt"
                 
diagonals :: [[a]] -> [[a]]
diagonals []         = []
diagonals ([]:board) = board
diagonals board      = zipWith (++) ([[x] | x <- (map head board)] ++ repeat [])
                                    ([]:(diagonals (map tail board)))

convertTransposedIndices :: [(Int,Int)] -> [(Int,Int)]
convertTransposedIndices indices = [(y,x) | (x,y) <- indices]

convertDiagonalIndices :: Int -> [(Int,Int)] -> [(Int,Int)]
convertDiagonalIndices _ [] = []
convertDiagonalIndices n (index:indices) = (convertDiagonalIndex n index : (convertDiagonalIndices n indices))

convertDiagonalIndex :: Int -> (Int,Int) -> (Int,Int)
convertDiagonalIndex n (i,j) = if (j <= n) then (i,(j-i))
                               else ((j-n+i),(n-i))

convertDiagonalTransposedIndices :: Int -> [(Int,Int)] -> [(Int,Int)]
convertDiagonalTransposedIndices _ [] = []
convertDiagonalTransposedIndices n (index:indices) = (convertDiagonalTransposedIndex n index : (convertDiagonalTransposedIndices n indices))

convertDiagonalTransposedIndex :: Int -> (Int,Int) -> (Int,Int)
convertDiagonalTransposedIndex n (i,j) = if (j <= n) then ((n-j+i),i)
                                         else (i,(j-n+i))

findIndicesToCross :: [String] -> [[Char]] -> [(Int,Int)]
findIndicesToCross words board = Set.toList (Set.fromList (findWordsInAllDirections words board))

findWordsInAllDirections :: [String] -> [[Char]] -> [(Int,Int)]
findWordsInAllDirections words board = findWordsInBoard words board 
                                       ++ (convertTransposedIndices (findWordsInBoard words transposed_board)) 
                                       ++ (convertDiagonalIndices m (findWordsInBoard words diagonals_up_board))
                                       ++ (convertDiagonalTransposedIndices n (findWordsInBoard words diagonals_down_board))
                                       where 
                                         n = (min (length board) (length (head board)))-1
                                         m = (max (length board) (length (head board)))-1
                                         transposed_board = List.transpose board
                                         diagonals_up_board = diagonals board
                                         diagonals_down_board = diagonals (reverse transposed_board)

crossIndicesFromBoard :: [[Char]] -> [(Int,Int)] -> [[Char]]
crossIndicesFromBoard board [] = board
crossIndicesFromBoard board (index:indicesToCross) = crossIndicesFromBoard (crossIndexFromBoard board index) indicesToCross

crossIndexFromBoard :: [[Char]] -> (Int,Int) -> [[Char]]
crossIndexFromBoard board (x,y) = crossLetterOnBoard board x y

crossLetterOnBoard :: [[Char]] -> Int -> Int -> [[Char]]
crossLetterOnBoard [] _ _ = []
crossLetterOnBoard (line:lines) colNr lineNr | lineNr==0 = (crossLetterFromLine line colNr):(crossLetterOnBoard lines colNr (lineNr-1))
                                    | otherwise = (line:(crossLetterOnBoard lines colNr (lineNr-1)))

crossLetterFromLine :: [Char] -> Int -> [Char]
crossLetterFromLine [] _ = []
crossLetterFromLine (letter:restLine) 0 = '-':(crossLetterFromLine restLine (-1))
crossLetterFromLine (letter:restLine) n = letter:(crossLetterFromLine restLine (n-1))

findWordsInBoard :: [String] -> [[Char]] -> [(Int, Int)]
findWordsInBoard [] _ = []
findWordsInBoard _ []= []
findWordsInBoard (word:restWords) board = findWordInBoard word board 0 ++ (findWordsInBoard restWords board)

findWordInBoard :: String -> [[Char]] -> Int -> [(Int,Int)] 
findWordInBoard [] _ _ = []
findWordInBoard _ [] _ = []
findWordInBoard word (line:restBoard) lineNr = [(x,lineNr) | x <- findWordInLine word line] ++ findWordInBoard word restBoard (lineNr+1)

findWordInLine :: String -> [Char] -> [Int]
findWordInLine _ [] = []
findWordInLine [] _ = []
findWordInLine word line = if _findWordInLine word line word line 0 >= 0 then [(x + (_findWordInLine word line word line 0)) | x <- [0..((length word)-1)]]
                           else []

_findWordInLine :: String -> [Char] -> String -> [Char] -> Int -> Int
_findWordInLine _ _ [] _ firstLetterColNr = firstLetterColNr
_findWordInLine _ _ _ [] _ = -1
_findWordInLine fullWord (letterOuter:restLineOuter) (letterToFind:restWord) (letter:restLine) firstLetterColNr | (letterToFind == letter) = (_findWordInLine fullWord (letterOuter:restLineOuter) restWord restLine firstLetterColNr)
                                                                                                                | otherwise = _findWordInLine fullWord restLineOuter fullWord restLineOuter (firstLetterColNr+1)
