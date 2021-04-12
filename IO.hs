module IO where


----------------------------------------------------------------------------
-- Matriz de String


---- Leitura do arquivo de teste

parseLines :: [String] -> (Int, [[String]])
parseLines (k_line : matrix_lines) =
    (k, matrix)
    where
        k = read k_line::Int                -- máximo de iterações
        matrix = parseMatrix matrix_lines   -- matriz

parseMatrix :: [String] -> [[String]]
parseMatrix = map words

loadMatrix :: FilePath -> IO (Int, [[String]])
loadMatrix filename =
    fmap (parseLines . lines) (readFile filename)


---- Saída no terminal

printCell :: [Char] -> IO ()
printCell l = do
    if l == "dead" then
        putStr "    dead"
    else if l == "alive" then
        putStr "   alive"
    else
        putStr "  zombie"


printLine :: [[Char]] -> IO ()
printLine (l:ls) = do
    printCell l
    if not $ null ls
        then printLine ls
    else
        putStr ""


printMatrix :: [[[Char]]] -> IO ()
printMatrix (m:ms) = do
    printLine m
    putStrLn ""
    if not $ null ms
        then printMatrix ms
    else
        putStrLn ""




----------------------------------------------------------------------------
-- Matriz de Int


parseLines' :: [String] -> (Int, [[Int]])
parseLines' (k_line : matrix_lines) =
    (k, matrix)
    where
        k = read k_line::Int                -- máximo de iterações
        matrix = parseMatrix' matrix_lines  -- matriz

parseMatrix' :: [String] -> [[Int]]
parseMatrix' = map (map read . words)


loadMatrix' :: FilePath -> IO (Int, [[Int]])
loadMatrix' filename =
    fmap (parseLines' . lines) (readFile filename)



