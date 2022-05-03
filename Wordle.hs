module Wordle where

import Data.List as L (map, delete, sort, elemIndices)
import Data.List.Split as S (splitOn)
import Debug.Trace (trace)

--efficiency key
-- W = length of word list
-- S = length of solutions list
-- L = number of letter in each word

--function to be called from other modules
wordle :: IO ()
wordle = do
    putStrLn "Enter 0 to play Solve or 1 to play Live Play"
    g <- getLine
    let entry = read g
    a <- readAnswerFile
    w <- readWordsFile
    s <- readSolutionsFile
    let answer = head $ lines a
    let words = lines w
    let solutions = lines s
    if entry == 0 then 
        solve answer words solutions 1 
    else if entry == 1 then 
        livePlay startingWord words solutions 1 
    else error "Invalid input"
    where 
        --efficiency of bestWord + delete + updateList, which is O(S^3*L^2*W)
        --recursively uses bestWord to make a guess, then updates the list of possible solutions until the guess is correct
        --solve :: targetWord -> word list -> solutions -> guess number
        solve :: String -> [String] -> [String] -> Int -> IO ()
        solve answer words solutions 1 = do
            putStrLn startingWord
            if startingWord == answer then 
                putStrLn ("Word guessed in " ++ show 1 ++ " guesses") 
            else solve answer (delete startingWord words) (updateList startingWord answer solutions) 2
        solve answer words solutions 7 = print "Could not guess the word in time"
        solve answer words solutions round = do
            let guess = bestWord words solutions in if guess == answer then 
                putStrLn guess <> putStrLn ("Word guessed in " ++ show round ++ " guesses") 
            else trace guess solve answer (delete guess words) (updateList guess answer solutions) $ round + 1

        --efficiency of convert + delete + correct + eliminate + bestWord, which is O(S^3*L^2*W)
        --recursively receives the grade for its guess, uses eliminate to get the new list of possible solutions, then calls bestWord on the new list to make its next guess
        --livePlay :: guess -> word list -> solutions -> guess number
        livePlay :: String -> [String] -> [String] -> Int -> IO ()
        livePlay guess words solutions 1 = do
            putStrLn "No Color = 0, Yellow = 1, Green = 2"
            putStrLn "Example grade input: 0, 2, 0, 1, 0"
            putStrLn guess
            putStrLn "Grade my guess please"
            rawResults <- getLine
            let results = convert $ splitOn ", " rawResults
            let updatedWords = delete guess words
            if correct results then 
                putStrLn ("I win in " ++ show 1 ++ " guesses!") 
            else let updatedSolutions = eliminate guess results solutions in livePlay (bestWord updatedWords updatedSolutions) updatedWords updatedSolutions 2
        livePlay guess words solutions 7 = putStrLn "Could not guess the word in time"
        livePlay guess words solutions round = do
            putStrLn guess
            putStrLn "Grade my guess please"
            rawResults <- getLine
            let results = convert $ splitOn ", " rawResults
            let updatedWords = delete guess words
            if correct results then 
                putStrLn ("I win in " ++ show round ++ " guesses!") 
            else let updatedSolutions = eliminate guess results solutions in livePlay (bestWord updatedWords updatedSolutions) updatedWords updatedSolutions $ round + 1

        --efficiency of O(W * log W ) + tupleList , which effectively is O(S^3*L^2*W)
        --used to get more than just the single bestWord
        startingWordList :: Int -> [String] -> [String] -> [(Int, String)]
        startingWordList num list solutions = take num $ sort $ tupleList list solutions

        --efficiency of O(W * log W ) + tupleList , which effectively is O(S^3*L^2*W)
        --determines the best word to guess by finding the lowest score of all possible words
        --same as startingWordList 1 ...
        bestWord :: [String] -> [String] -> String
        bestWord list solutions = do case minimum $ tupleList list solutions of { (n, s) -> s }

        --efficiency of W * score, which is O(S^3*L^2*W)
        --returns a list of tuples with all possible words and their score
        --tupleList :: word list -> solutions -> [(score of word, word)]
        tupleList :: [String] -> [String] -> [(Int, String)]
        tupleList [] _ = []
        tupleList [x] solutions = [(score x solutions solutions, x)]
        tupleList (x:xs) solutions = (score x solutions solutions, x) : tupleList xs solutions

        --efficiency of S*(S*updateList), which is O(S^3*L^2)
        --grades each possible word against all solutions and sums the number of possible solutions remaining
        --score :: guess -> solutions -> solutions -> int
        score :: String -> [String] -> [String] -> Int
        score guess [] solutions = error "Grading input was incorrect"
        score guess [l] solutions = length $ updateList guess l solutions
        score guess (l:ls) solutions = length (updateList guess l solutions) + score guess ls solutions

        --efficiency of + eliminate grade + correct, which is O(S*L^2)
        --returns the new list of possible solutions after grading the guess against answer
        updateList :: String -> String -> [String] -> [String]
        updateList guess answer solutions = let results = grade guess guess answer in 
            if correct results then 
                [] 
            else eliminate guess results solutions

        --efficiency of O(S*L^2)
        --iterates through solutions and uses the keep helper function to determine if it should be included in the updated solutions
        --eliminate :: guess -> results of grading -> solutions -> new list of possible solutions
        eliminate :: String -> [Int] -> [String] -> [String]
        eliminate _ _ [] = []
        eliminate guess results (l:ls)
            | keep guess guess results l = l : eliminate guess results ls
            | otherwise = eliminate guess results ls
            where
                --checks the green results, then the yellows, then the nothings
                --when a green or yellow letter finds its match in answer, replace that letter in answer with a *, so it cannot be used again
                --keep :: guess -> guess -> grades -> word from solutions -> Bool 
                keep :: String -> String -> [Int] -> String -> Bool
                keep (x:xs) guess results word = do
                    let indices2 = 2 `elemIndices` results
                    let indices1 = 1 `elemIndices` results
                    let indices0 = 0 `elemIndices` results
                    if not (null indices2) then
                        greenCheck guess word indices2 && let claimed = replaceGreen results word indices2 in uncurry (keep xs guess) claimed
                    else if not (null indices1) then
                        let x = yellowCheck guess word results indices1 in extractFirst x && keep xs guess (extractSecond x) (extractThird x)
                    else nothingCheck guess word indices0
                    where
                        --returns true if all the green letters match the letters at that index in answer
                        greenCheck :: String -> String -> [Int] -> Bool
                        greenCheck guess word [] = True
                        greenCheck guess word (i:is) = guess !! i == word !! i && greenCheck guess word is

                        --returns true if all the yellow letters find themselves in the answer at a different index
                        yellowCheck :: String -> String -> [Int] -> [Int] -> (Bool, [Int], String)
                        yellowCheck guess word results [] = (True, results, word)
                        yellowCheck guess word results (i:is) = 
                            if guess !! i `elem` word && guess !! i /= word !! i then 
                                let replaced = replaceYellow results word i (head (guess !! i `elemIndices` word)) in yellowCheck guess (snd replaced) (fst replaced) is 
                            else (False, [], "")

                        --returns true if all the nothing letters are not found in the answer
                        nothingCheck :: String -> String -> [Int] -> Bool
                        nothingCheck guess word [] = True
                        nothingCheck guess word (i:is) = guess !! i `notElem` word && nothingCheck guess word is

                        --replace the letters in answer at the given indices with a *
                        replaceGreen :: [Int] -> String -> [Int] -> ([Int], String)
                        replaceGreen results word [] = (results, word)
                        replaceGreen results word (i:is) = do
                            let x = splitAt i results
                            let y = splitAt i word
                            replaceGreen (fst x ++ [-1] ++ tail (snd x)) (fst y ++ "*" ++ tail (snd y)) is

                        --replace the letters in answer at the given indices with a *, different than replaceGreen because replaceYellow is called within yellowCheck
                        replaceYellow :: [Int] -> String -> Int -> Int -> ([Int], String)
                        replaceYellow results word ri wi = do
                            let x = splitAt ri results
                            let y = splitAt wi word
                            (fst x ++ [-1] ++ tail (snd x), fst y ++ "*" ++ tail (snd y))

                        --the following three functions are needed to get values from a triple
                        extractFirst :: (a, b, c) -> a
                        extractFirst (a, _, _) = a

                        extractSecond :: (a, b, c) -> b
                        extractSecond (_, b, _) = b

                        extractThird :: (a, b, c) -> c
                        extractThird (_, _, c) = c

        --efficiency of O(L^2)
        --0 means no color, 1 means yellow, 2 means green
        --iterates through the chars in a string and checks if it is an element of answer, then if that char is at the same index as it is in answer
        --grade :: guess -> guess -> answer -> list of results represented by ints
        grade :: String -> String -> String -> [Int]
        grade [] guess answer = []
        grade (x:xs) guess answer = do
            let myOccur = occurrences x guess
            let answerOccur = occurrences x answer
            if myOccur == 1 || (answerOccur - myOccur == 0) then
                if x `elem` answer then
                    if x == (answer !! (4 - length xs)) then
                        2 : grade xs guess answer
                    else 1 : grade xs guess answer
                else 0 : grade xs guess answer
            else if answerOccur > myOccur then 
                if x == (answer !! (4 - length xs)) then 
                    2 : grade xs guess (replace answer (4 - length xs))
                else 
                    1 : grade xs guess (replace answer $ indexOfClaimedLetter x guess answer) 
            else
                if answerOccur == 0 then
                    0 : grade xs guess answer
                else if x == (answer !! (4 - length xs)) then
                    2 : grade xs guess (replace answer (4 - length xs))
                else if (myOccur == 3 && answerOccur == 1 && anyDuplicateExact x guess answer) || duplicatesExact x guess answer then
                    0 : grade xs guess answer
                else 1 : grade xs guess (replace answer $ indexOfClaimedLetter x guess answer)
                where
                    --replaces the letter at the given index with *
                    replace :: String -> Int -> String
                    replace [] _ = error "Index out of bounds"
                    replace (x:xs) 0 = "*" ++ xs
                    replace (x:xs) index = x : replace xs (index - 1)

                    --finds the index of the letter in answer that satisfied a green or yellow condition
                    indexOfClaimedLetter :: Char -> String -> String -> Int
                    indexOfClaimedLetter x _ [] = error "Never found claimed letter"
                    indexOfClaimedLetter x guess (a:as)
                        | x == a && a /= (guess !! (4 - length as)) = 4 - length as
                        | otherwise = indexOfClaimedLetter x guess as

                    --returns true if all duplicate letters are green
                    duplicatesExact :: Char -> String -> String -> Bool
                    duplicatesExact x [] answer = False
                    duplicatesExact x (g:gs) answer
                            | x == g = helper x gs answer
                            | otherwise = duplicatesExact x gs answer
                        where
                            helper :: Char -> String -> String -> Bool
                            helper x [] answer = True
                            helper x (g:gs) answer
                                | x == g = x == (answer !! (4 - length gs)) && helper x gs answer
                                | otherwise = helper x gs answer
                    
                    --returns true if any duplicate letters are green
                    anyDuplicateExact :: Char -> String -> String -> Bool
                    anyDuplicateExact x [] answer = False
                    anyDuplicateExact x (g:gs) answer
                        | x == g = helper x gs answer
                        | otherwise = anyDuplicateExact x gs answer
                        where
                            helper :: Char -> String -> String -> Bool
                            helper x [] answer = False
                            helper x (g:gs) answer
                                | x == g = x == (answer !! (4 - length gs)) || helper x gs answer
                                | otherwise = helper x gs answer

                    --counts the number of occurrences of a letter in a word
                    occurrences :: Char -> String -> Int
                    occurrences x [] = 0
                    occurrences x (g:gs)
                        | x == g = 1 + occurrences x gs
                        | otherwise = occurrences x gs

        --efficiecy of O(L)
        --returns true if every letter was graded green, false if not
        correct :: [Int] -> Bool
        correct = foldr (\ r -> (&&) (r == 2)) True

        --efficiency of O(L)
        --converts the inputed grades from type string to int
        convert :: [String] -> [Int]
        convert [] = []
        convert (x:xs)
            | x == "0" = 0 : convert xs
            | x == "1" = 1 : convert xs
            | x == "2" = 2 : convert xs
            | otherwise = error "Invalid input"

        --efficiency of O(1)
        --used to store the best opening word, so you don't have to figure that out each time the code executes
        --can be found using startingWordList 1 word list solutions
        startingWord :: String
        startingWord = "roate"

        --efficiency of O(1)
        --reads the word in the answer.txt file
        --used in the solve game, where the user defines the target word
        readAnswerFile :: IO String
        readAnswerFile = readFile "answer.txt"

        --efficiency of O(1)
        --reads the list of solution words in the solutions.txt file
        readSolutionsFile :: IO String
        readSolutionsFile = readFile "solutions.txt"

        --efficiency of O(1)
        --reads the list of all words in the word_list.txt file
        readWordsFile :: IO String
        readWordsFile = readFile "word_list.txt"
