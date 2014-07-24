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

targetWord = encodeWord "hello"


encodeWord :: String -> Word
encodeWord = concatMap (encodeBinary rangeOfLetters . ord)

decodeWord :: Word -> String
decodeWord = map (chr. decodeBinary rangeOfLetters) . splitEvery bitsPerLetter

wordFitness :: Word -> Wordscore
wordFitness = flip hammingDistance targetWord

randomWord :: Rand Word
randomWord = do
    rndstring <- do s <- randomSample lettersInWord "abcdefghijklmnopqrstuvwxyz"
                    shuffle s
    return $ encodeWord rndstring

randomWordPopulation :: Int -> Rand [Word]
randomWordPopulation n = replicateM n randomWord

population = 30
elitecount = 1
initialpopulation = randomWordPopulation population
selection = tournamentSelect Minimizing 2 (population - elitecount)
crossover = onePointCrossover 0.7
mutation = pointMutate 0.03
nextgen = nextGeneration Minimizing wordFitness selection elitecount crossover mutation
stop = (IfObjective ((==0) . minimum))

ioact = DoEvery 1 helper where
    helper _ p = do
        let prstr = map (\ (g, f) -> decodeWord g ++ " " ++ (show f)) p
        mapM_ putStrLn prstr

main = runIO initialpopulation (loopIO [ioact] stop nextgen) >> return ()

--Checking out the library
{-fitnessTest = do                                                       -}
{-    mt <- newPureMT                                                    -}
{-    let pop = replicateM 10 randomWord                                 -}
{-        bits = evalRandom pop mt                                       -}
{-    mapM_ putStrLn $ map (\b -> show (decodeWord b, wordFitness b))bits-}

{-inbuiltInitialize = do                                                 -}
{-    mt <- newPureMT                                                    -}
{-    let pop = getRandomBinaryGenomes 10 bitsPerWord                    -}
{-        bits = evalRandom pop mt                                       -}
{-    mapM_ putStrLn $ map decodeWord bits                               -}

{-manualInitialize = do                                                  -}
{-    mt <- newPureMT                                                    -}
{-    let pop = replicateM 10 randomWord                                 -}
{-        bits = evalRandom pop mt                                       -}
{-    mapM_ putStrLn $ map decodeWord bits                               -}

