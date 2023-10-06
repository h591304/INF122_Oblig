module Oblig0 where

-- Imports
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Ord as Ord
import qualified Data.ByteString.Char8 as BS

-- Type definitions
type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

-- Exercise1
-- a) implement encode. This method encrypts a string  using a key
encode :: Key -> String -> String
encode key str = map (\c -> fromMaybe c $ lookup c key) str

-- b) implement decode. Invert is a helper function used to invert the key
invert :: Key -> Key
invert key = [(a,b) | (b,a) <- key]

-- decode decrypts a string by using the encode and invert function 
decode :: Key -> String -> String
decode key str = encode (invert key) str

-- c) implement caesar. This function ouputs a rotationkey based on an alphabet(String) and rotationsteps(Integer)
caesar :: Alphabet -> Integer -> Key
caesar alphabet steps = zip alphabet $ drop (fromIntegral $ mod steps $ fromIntegral (length alphabet)) (cycle alphabet)

-- Exercise2
-- a) implement count. This function calculates the frequency/ how many times a symbol appears in a string
count :: String -> FrequencyTable
count str =
    let charCounts = Map.fromListWith (+) [(c, 1) | c <- str] -- Counts each symbol in a string and makes a list of tuples for each character and frequency
        totalCount = fromIntegral (length str) -- Total amount of symbols in a string
        charFrequencies = Map.toList $ fmap (\count -> count/totalCount) charCounts  -- charFrequency is defined by count/totalcount
    in charFrequencies

-- b) implement loadFrequencyTable. This function reads a file (corpus.txt) and counts its symbols.
loadFrequencyTable :: FilePath -> IO FrequencyTable 
loadFrequencyTable file = do 
    contents <- BS.readFile file -- reads a file and turns it into binary 
    let frequency = count (BS.unpack contents) -- turns binary into normal text. i found that one symbol were missing if I skipped the binary step
    return frequency

-- c) import initialGuess. This function inputs two frequencytables and returns a list of pairs representing an initial guess for a key.
initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess frequency1 frequency2 = zip (sortedChars frequency1) (sortedChars frequency2)
    where
        sortedChars freqTab = map fst $ List.sortBy (Ord.comparing (negate . snd)) freqTab

-- d) import chiSquared. This function calculates chisquared using a formula given for this exercise.
chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observation =
    sum [((oFreq - eFreq) ^ 2) / eFreq | (oChar, oFreq) <- observation, 
    let eFreq = fromMaybe (1/10000) (lookup oChar model)] -- if the expected frequency of a symbol is 0, we use a factor 1/10000.

-- Exercise3
-- a/b) implement swapEntries. This funciton takes two pairs and swaps two substitutions within a key
swapEntries :: Eq a => (a, b) -> (a, b) -> [(a, b)] -> [(a, b)]
swapEntries (c1, e1) (c2, e2) = map (swapIfNeeded (c1, e1) (c2, e2)) 
    where
        swapIfNeeded (c1, e1) (c2, e2) (c3, e3) -- Helper function that swaps the pairs.
            | c1 == c3 = (c1, e2) -- If c1 matches c3, e1 and e2 changes
            | c2 == c3 = (c2, e1) -- If c2 matches c3, e2 and e1 changes
            | otherwise = (c3, e3)  -- Else it stays unchanged.

-- c) implement neighbourKeys. This function generates all possible keys from a given key.
neighbourKeys :: Key -> [Key]
neighbourKeys key = [swapEntries tuple1 tuple2 key | tuple1 <- key, tuple2 <- key, fst tuple1 < fst tuple2]

-- d) implement greedy. This greedy function uses all functions up untill now to iteratively updates the key based on chi-squared values until a local minimum is reached.
greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey
    | currentMin <= snd newMin = initKey
    | otherwise = greedy model cipherText (fst newMin)
    where
        candidates = [(k, chiSquared model (count (decode k cipherText))) | k <- neighbourKeys initKey]
        newMin = List.minimumBy (Ord.comparing snd) candidates
        currentMin = chiSquared model (count (decode initKey cipherText))

-- Exercise4
-- a) implement loadDictionary. This function reads a file reads a file (corpus.txt) and finds all unique words within the file.
loadDictionary :: FilePath -> IO Dictionary
loadDictionary file = do 
    content <- readFile file
    let wordsList = words content -- words function makes a wordlist of all content from the file
    return $ Set.fromList wordsList

-- b) implement countValidWords. This function counts how many words from a string appearing in the dictionary.
countValidWords :: Dictionary -> String -> Integer
countValidWords dict str = fromIntegral $ length validWords
    where
        listOfWords = words str
        validWords = filter (`Set.member` dict) listOfWords

-- c) implement greedyDict. This greedy function maximize the amount of valid words in the decrypted text.
greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText key =
    let neighbours = [(k, countValidWords dict (decode k cipherText)) | k <- neighbourKeys key] --creates a list of pairs where each pair consists of a key k.
        maxNeighbour = fst $ List.maximumBy (Ord.comparing snd) neighbours
    in if countValidWords dict (decode key cipherText) < countValidWords dict (decode maxNeighbour cipherText)
        then greedyDict dict cipherText maxNeighbour else key -- Recursively calls itself until with the maxNeighour as the key








