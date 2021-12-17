import Data.List(transpose)
import Data.Maybe

main = do
    contents <- readFile "4.txt"

    let rawLines = lines contents :: [String]
        rawLinesNoEmpty = filter (/= "") rawLines
        calledNumbers = map (\x -> read x :: Int) $ split (head rawLinesNoEmpty) ','
        rawCards = tail rawLinesNoEmpty
        bingoCards = createBingoCards rawCards
        (winningNumber, summedRest) = play bingoCards calledNumbers
    print (winningNumber, summedRest)
    print $ winningNumber * summedRest
    return ()


createBingoCards :: [String] -> [[[(Int, Bool)]]]
createBingoCards [] = []
createBingoCards cards = createBingoCard (take 5 cards) : createBingoCards (drop 5 cards)

createBingoCard :: [String] -> [[(Int, Bool)]]
createBingoCard = map (createBingoCardRow . words)

createBingoCardRow :: [String] -> [(Int, Bool)]
createBingoCardRow = map (\x -> (read x :: Int, False))


play :: [[[(Int, Bool)]]] -> [Int] -> (Int, Int)
play _ [] = (0, 0)
play boards numbers =
    let (winningBoard, winningNumber) = playTillExhaustion boards numbers
    in (winningNumber, sumBoard $ winningBoard)

playTillExhaustion :: [[[(Int, Bool)]]] -> [Int] -> ([[(Int, Bool)]], Int)
playTillExhaustion [] _ = ([], -1)
playTillExhaustion _ [] = ([], -2)
playTillExhaustion boards (number : rest) =
    let newBoards = map (\board -> applyBoard board number) boards
        ours = fromJust (first checkBoard newBoards)
        restBoards = filter (not . checkBoard) newBoards
        theirs = playTillExhaustion restBoards rest
    in if (fst theirs) /= [] then theirs else (ours, number)


sumBoard :: [[(Int, Bool)]] -> Int
sumBoard board =
    let fields = concat board
        relevant = filter (not . snd) fields
        numbers = map fst relevant
    in sum numbers

first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first f (head:tail) = if f head then Just head else first f tail

checkBoard :: [[(Int, Bool)]] -> Bool
checkBoard board =
    let rows = board
        columns = transpose board
        slices = rows ++ columns
    in any validateSlice slices

validateSlice :: [(Int, Bool)] -> Bool
validateSlice = all snd

applyBoard :: [[(Int, Bool)]] -> Int -> [[(Int, Bool)]]
applyBoard board number = map (map (\(field, called) -> (field, called || number == field))) board

split :: String -> Char -> [String]
split [] _ = []
split str sep =
    let (word, rest) = break (== sep) str
    in word : split (dropWhile (== sep) rest) sep
