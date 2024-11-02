module Main (main) where

-- standard libraries imports
import System.IO
-- import System.Console.ANSI

-- external libraries imports
import Statistics.Distribution
import Statistics.Distribution.StudentT

-- project imports
import Analysis
import LabParameters


colorRed = "\ESC[31m" :: String
colorGreen = "\ESC[32m" :: String
colorDefault = "\ESC[0m" :: String


-- Restituisce la coppia (avg,err).
-- 1. l'argomento l e' una lista di coppie (x_i, e_i) (valore, errore).
-- 2. (xs,ws) e' una coppia di liste, nella prima ci sono gli x_i e nella seconda w_i = 1/(e_i)^2
-- Da wikipedia:
-- 3. avg e' la somma di x_i * w_i
-- 4. err e' la radice di 1 fratto la somma di w_i
weightedAverage :: [(Float,Float)] -> (Float,Float)
weightedAverage l = ( avg, err )
  where (xs,ws) = unzip $ map (\(x,y) -> (x, 1 / (y**2))) l
        avg = (sum (zipWith (*) xs ws)) / (sum ws)
        err = sqrt (1 / (sum ws))


-- restituisce la coppia (avg, sqrt( varianza/N ))
-- dove (xs,_) e' una coppia di liste in cui la seconda (quella degli errori) e' ignorata
-- 1. avg e' la somma dei dati x_i divisa per il numero di dati N
-- 2. var e' la somma degli scarti quadratici divisa per N-1
stdAverage :: [(Float,Float)] -> (Float,Float)
stdAverage l = ( avg, sqrt $ var / (fromIntegral $ length xs) )
  where (xs,_) = unzip l
        avg = (sum xs) / (fromIntegral $ length xs)
        var = (sum $ map (\x -> (x - avg)**2) xs) / ((fromIntegral $ length xs) - 1)


-- quantile e' il valore del parametro della distribuzione passata come parametro (in questo caso la densita'
-- di probabilita' della t di student con parametro df (degrees of freedom) tale che l'integrale da -infinito a quel valore
-- faccia una certa probabilita' (cioe' nel nostro caso deve essere uguale a significance)
singleTailCriticalTValue :: Float -> Int -> Float
singleTailCriticalTValue significance df = realToFrac $ quantile (studentT $ fromIntegral df) (1 - realToFrac significance)

-- restituisce la differenza tra due quantili (cioe' tra due valori del parametro t della t di student che vengono associati
-- a una certa probabilita', in particolare q1 -> significance /2 mentre q2 -> (1-significance/2))
doubleTailCriticalTValue :: Float -> Int -> Float
doubleTailCriticalTValue significance df = realToFrac $ q2 - q1
  where alpha = significance / 2
        q1 = quantile (studentT $ fromIntegral df) (realToFrac alpha)
        q2 = quantile (studentT $ fromIntegral df) (1 - realToFrac alpha)


-- 1. funzione piu' interna (lines):
--    separa le righe,
-- 2. poi (map words) in ogni riga (che ora e' un elemento di una lista) separa le parole (cioe' usa
--    lo spazio come un separatore)
-- 3. poi (map (map read)) ogni parola di ogni riga viene letta come un numero
-- 4. poi (filter (not . null)) dalla lista totale creata (lista di liste di numeri) vengono eliminati gli elementi che sono
--    liste vuote (cioe' righe in cui non c'erano parole e per cui non e' stata prodotta una lista di numeri)
simpleParser :: String -> [[Float]]
simpleParser = filter (not . null) . map (map read) . (map words) . lines



-- definisce un "algebraic data type", cioe' un tipo di dato che puo' essere costruito in due modi:
-- 1. ConfidenceInterval, con un parametro (significance)
-- 2. SignificanceTest, con due parametri (valore atteso e significance)
data TTest = ConfidenceInterval Float | SignificanceTest Float Float | NoTest

-- i due costruttori potrebbero essere scritti in modo piu' chiaro dando nomi
-- ai parametri con la record synatx ma non ho voglia di cambiarlo. Anyway:
-- ConidenceInterval <confidence level>
-- SignificanceTest <expected value> <significance>


-- gli argomenti sono scritti a fianco come commenti. Nota due cose:
-- 1. TTest e' il tipo definito qui sopra, e rappresenta una variabile che puo' essere o un ConfidenceInterval, o un SignificanceTest
--    e quando uno di questi due valori viene passato, l'argomento di tipo TTest porta' essere "decostruito" nel suo rispettivo
--    caso in cui si legano i suoi parametri a delle "variabili"
-- 2. il tipo di ritorno IO (...) indica che la funzione main e' in realta' una funzione "non puramente funzionale", perche' IO
--    e' una monade particolare che permette di gestire l'idea di fallimento di una routine a causa di elementi esterni al programma.
--    Ogni volta che vedi l'operatore <- pensalo come un operatore di assegnazione ad una variabile (non lo e', ma in questo caso
--    funziona vederlo cosi'), che prende il risultato di una "IO action" (che potrebbe andare male) e lo mette in una "variabile"
processFile :: String ->                               -- filename
               (String -> [[Float]]) ->                -- parser
               ([[Float]] -> [Float]) ->               -- operation on dataset
               ([[Float]] -> [Float]) ->               -- dataset errors
               ([(Float,Float)] -> (Float,Float)) ->   -- avg+-err statistical estimator
               String ->                               -- measurement units
               Float ->                                -- scale factor
               TTest ->                                -- t-test
               IO (Float, Float)                       -- return (avg,err)
processFile path parseContents processData calcErrors bestEstimate units scaleFactor ttest = do
  putStrLn $ colorGreen ++ "\n[*] Processing " ++ path ++ colorDefault

  withFile path ReadMode (\handle -> do
    -- read file, get the data and calculate stuff
    contents <- hGetContents handle
    let rawData = parseContents contents
        processedData = processData rawData
        errors = calcErrors rawData
        (avg, err) = bestEstimate $ zip processedData errors
        degOfFreedom = (length processedData) - 1 -- 1 because average is the only constraint

    putStrLn "\nResults:"
    _ <- sequence $ map
      (\(x,e) -> putStrLn $ (show $ scaleFactor * x) ++ " +- " ++ (show $ scaleFactor * e) ++ " " ++ units)
      $ zip processedData errors

    putStrLn "\nFinal Measure:"
    putStrLn $ (show $ scaleFactor * avg) ++ " +- " ++ (show $ scaleFactor * err) ++ " " ++ units

    case ttest of
      -- consider implementing a way of testing against multiple significance levels
      SignificanceTest expectedValue significance -> do
        let t0 = (abs $ avg - expectedValue) / err
            tc = singleTailCriticalTValue significance degOfFreedom
            p0 = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -t0)
            pc = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -tc)
        putStrLn $ "\nStatistical Significance at " ++ (show $ 100 * significance) ++ "% for expected value of " ++ (show $ scaleFactor * expectedValue) ++ ":"
        putStrLn $ "t0:  P(-inf <= -" ++ (show t0) ++ ") = " ++ (show $ 100 * p0) ++ "%"
        putStrLn $ "t_c: P(-inf <= -" ++ (show tc) ++ ") = " ++ (show $ 100 * pc) ++ "%" -- just to check: P(<=tc) should be equal to significance
      ConfidenceInterval confidence -> do
        let tc = doubleTailCriticalTValue confidence degOfFreedom
            lowerBound = avg - tc * err
            upperBound = avg + tc * err
        putStrLn $ "\nMargins of error for confidence level of " ++ (show $ 100 * confidence) ++ "%:"
        putStrLn $ "tc = " ++ (show tc) ++ ": (" ++ (show $ scaleFactor * lowerBound) ++ ", " ++ (show $ scaleFactor * upperBound) ++ ")"
      NoTest -> do putStrLn "\nNo test to see here..."

    -- return results of calculations
    return (avg, err))


processParameterFile :: String ->                -- filename
                        (String -> [[Float]]) -> -- parser
                        ([[Float]] -> Float) ->  -- operation on dataset
                        ([[Float]] -> Float) ->    -- error estimate
                        IO (Float, Float)        -- return (avg,err)
processParameterFile path processFile calcParam calcParamError = do
  putStrLn $ "[*] Processing '" ++ path ++ "':"
  withFile path ReadMode (\handle -> do
    contents <- hGetContents handle
    let rawData = processFile contents
        param = calcParam rawData
        paramError = calcParamError rawData
    putStrLn $ (show param) ++ " +- " ++ (show paramError)
    return (param,paramError))



-- ..:: Entry Point ::..

main :: IO ()
main = do
  (theta0,theta0Err) <- processFile "./data/misure_orto.csv"    (simpleParser) (calcTheta0)              (calcTheta0Error)              (stdAverage) "rad" (1) (NoTest)
  (step,stepError)   <- processFile "./data/misure_passo.csv"   (simpleParser) (calcLatticeStep theta0)  (calcLatticeStepError theta0)  (stdAverage) ""    (1) (NoTest)
  (_,_)              <- processFile "./data/misure_viola1.csv"  (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_viola2.csv"  (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_blu.csv"     (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_verde.csv"   (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_giallo1.csv" (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_giallo2.csv" (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  (_,_)              <- processFile "./data/misure_rosso.csv"   (simpleParser) (calcLambdas step theta0) (calcLambdaErrors step theta0) (stdAverage) "A"   (1) (NoTest)
  return ()
