import Control.Monad (replicateM)
import Data.List (concatMap)
import Data.Char (ord, chr)
import Moo.GeneticAlgorithm.Random (shuffle, randomSample)
import Moo.GeneticAlgorithm.Binary

--The goal is to evolve the string "hello"
--This is just a demonstration of a genetic
--algorithm

type Letter = Bool
type Word = Genome Letter
type Wordscore = Objective

lettersInWord = 5
rangeOfLetters = (ord 'a', ord 'z')
bitsPerLetter = bitsNeeded rangeOfLetters
bitsPerWord = bitsPerLetter * lettersInWord
targetWord = concatMap (encodeBinary rangeOfLetters . ord) "hello"

wordFitness :: Word -> Wordscore
wordFitness w = hammingDistance w targetWord

randomWord :: Rand Word
randomWord = do
    rndstring <- do
        s <- randomSample lettersInWord "abcdefghijklmnopqrstuvwxyz"
        shuffle s
    return $ concatMap ((encodeBinary rangeOfLetters) . ord) rndstring

randomWordPopulation :: Int -> Rand [Word]
randomWordPopulation n = replicateM n randomWord

--Checking out the library
inbuiltInitialize = do
    mt <- newPureMT
    let pop = getRandomBinaryGenomes 10 bitsPerWord
        bits = evalRandom pop mt
    mapM_ putStrLn $ map (map (chr. decodeBinary rangeOfLetters) . splitEvery bitsPerLetter) bits

manualInitialize = do
    mt <- newPureMT
    let pop = replicateM 10 randomWord
        bits = evalRandom pop mt
    mapM_ putStrLn $ map (map (chr. decodeBinary rangeOfLetters) . splitEvery bitsPerLetter) bits

