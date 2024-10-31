module Main (main) where

-- standard libraries imports
import System.IO
-- import System.Console.ANSI

-- external libraries imports
import Statistics.Distribution
import Statistics.Distribution.StudentT

-- project imports
import Lambda
import RefractionIndex
import WhiteLight
import Sodium



{- ..:: NOTES ::..
 -
 -
 -
 - funzioni utili:
 -
 - 1. zip prende due liste [x_i] e [y_i] e produce una lista i cui elementi sono le coppie di elementi
 -    cioe' da' [(x_i, y_i)]
 -
 - 2. zipWith fa prende le due liste e le unisce con una funzione => da' [f(x_i,y_i)]
 -
 - 3. map applica una funzione a ciascun elemento di una lista, restituendo la lista con gli output:
 -    da' [f(x_i)]
 -
 - 4. ++ concatena due liste (anche le stringhe sono liste)
 -
 - 5. putStrLn, putStr, print sono tutte funzioni per printare sul terminale
 -
 - 6. show trasforma un dato che non ha tipo string in una stringa (se il tipo lo permette)
 -
 -
 -
 - notazioni utili:
 -
 - 1. non esistono le variabili in un linguaggio puramente funzionale. TUTTO e' fatto solo di funzioni.
 -    (no worries, e' possibile emulare la programmazione procedurale con l'aiuto delle monadi
 -    e in particolare la State Monad, che pero' qua non serve)
 -
 - 2. le function signature assomigliano molto a una scrittura matematica:
 -    f :: a -> b         prende un argomento di tipo a e restituisce un tipo b
 -    g :: a -> b -> c    prende un argomento di tipo a e un argomento di tipo b e restituisce un tipo c
 -
 - 3. se hai una funzione f(x,y,z), la si chiama senza parentesi e virgole:
 -    f x y z
 -
 - 4: il punto '.' e' la composizione di funzioni:
 -    f :: a -> b,  g :: b -> c     =>   g . f :: a -> c
 -
 - 5. il carattere $ e' praticamente un sostituto delle parentesi perche' modifica l'ordine di
 -    evaluation di un'espressione (leggilo come "valuta prima l'espressione che sta alla destra
 -    di $ fino alla fine" (cioe' fino alla prossima parentesi, che ha una precedenza piu' forte)
 -    
 -    perche' haskell si confonde se hai una cosa tipo `someFunction x + 2`, non sempre sa cosa eseguire tra
 -    `(someFunction x) + 2` oppure `someFunction (x + 2)`. 
 -
 -    => per evitare le parentesi che spesso possono appesantire troppo la lettura, scrivi
 -       `someFunction $ x + 2`, e se questa espressione sta in una parentesi piu' grande, il $ valutera'
 -       tutto solo fino alla parentesi che racchiude l'espressione in cui si trova.
 -
 - 6. se una funzione e' chiamata senza l'ultimo argomento, allora sta implicitamente creando una nuova funzione
 -    che prende un solo argomento e restituisce la funzione originale valutata negli argomenti gia' passati e nell'ultimo argomento.
 -    Cosi' via se manca ANCHE il penultimo argomento, e poi per il terzultimo etc...
 -
 -    Questo meccanismo si chiama function currying, o partial function application, e la funzione originale si chiama
 -    higher order function rispetto alle "funzioni parziali"
 -
 -    Questo si legge molto bene nelle function signatures: ogni argomento e' in realta' l'output di una funzione
 -    che prende gli argomenti precedenti, e questo output e' in realta' a sua volta una funzione che prende dei
 -    parametri. L'ultimo tipo di output e' il "vero" output nel senso procedurale della programmazione.
 -
 -    ES: `zipWith (*) xs ys` prende le due liste xs = [x_i] e ys = [y_i] e restituisce una lista i cui elementi sono
 -        la funzione * applicata agli argomenti x_i e y_i
 -        => [x_i * y_i]
 -
 -        pensa all'operazione * come una funzione * :: a -> a -> a, usata in una notazione particolare per cui
 -        la funzione e' un "infix", cioe' viene chiamata in mezzo ai suoi due argomenti
 -
 -
 -
 - sintassi utile:
 -
 - 1. 'where' specifica i simboli che compaiono nell'espressione precedente
 -
 - 2. 'let ... in ...' specifica (nel blocco let) i simboli che compaiono nel blocco in
 -
 - 3. 'let' da solo si usa nei "do block" che sono l'emulazione della programmazione procedurale
 -    nelle IO actions (in generale e' un operatore su una monade che permette di estrarre il risultato
 -    di un'espressione che agisce su una monade)
 -
 - 4. (\ ... -> ...) e' una lambda function (il \ dovrebbe rappresentare la stanga di una lambda... oh boy):
 -    dopo il \ si mettono i nomi degli argomenti e dopo la -> si mette l'espressione che viene valutata come funzione
 -
 - 5. `case ... of ...` e' l'equivalente di uno switch statemente in c++, guarda il valore che ha l'esressione nel blocco case,
 -    e valuta l'espressione che corrisponde al caso
 -
 - -}



-- ..:: Program ::..

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
  {- ASNI colors:
   - default: \ESC[0m
   - red:     \ESC[31m
   - green:   \ESC[32m
   -}
  putStrLn $ "\n[*] Processing " ++ path
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
      NoTest -> ()

    -- return results of calculations
    return (avg, err))



-- ..:: Entry Point ::..

main :: IO ()
main = do
  (_,_) <- processFile "./data/misure_viola1.csv" (simpleParser) (yoScriviLaFunziaPerCalcolareLambda) (yoIdemPerGliErrori) (weightedAverage) "nm" (10**6) (NoTest)
  return ()
