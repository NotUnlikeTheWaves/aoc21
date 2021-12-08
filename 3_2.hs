import Data.Char(digitToInt)
import Data.Bits(shiftL)

main = do
    contents <- getContents

    let contentList = lines contents

    let useOnes = (\ones zeros -> if ones >= zeros then 1 else 0) :: Int -> Int -> Int
    let useZero = (\ones zeros -> if ones >= zeros then 0 else 1) :: Int -> Int -> Int

    let bitStrings = map getBits contentList
        oxygen = findStuff bitStrings 0 useOnes
        co2 = findStuff bitStrings 0 useZero
    
    print oxygen
    print co2

    let oxyInt = bitsToInt oxygen
        co2Int = bitsToInt co2
    
    print oxyInt
    print co2Int
    print (oxyInt * co2Int)
    return ()

findStuff :: [[Int]] -> Int -> (Int -> Int -> Int) -> [Int]
findStuff a 13 _ = error ("hey " ++ show a) -- a keeps being empty list [] here, so probably too much is filtered?
findStuff [a] _ _ = a
findStuff bitStrings pos decider =
    let amountOfOnes = foldl (\acc l -> acc + (l !! pos)) 0 bitStrings
        amountOfZeros = length bitStrings - amountOfOnes
        digit = decider amountOfOnes amountOfZeros

        filtered = filter (\x -> (x !! pos) == digit) bitStrings
    in findStuff filtered (pos + 1) decider

bitsToInt :: [Int] -> Int
bitsToInt = foldl (\acc v -> acc `shiftL` 1 + v) 0

getBits :: [Char] -> [Int]
getBits = map digitToInt

zipBits = zipWith (+)
