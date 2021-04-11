module IO where


----------------------------------------------------------------------------
-- Matriz de String

parseLines :: [String] -> (Int, [[String]])
parseLines (k_line : matrix_lines) =
    (k, matrix)
    where
        k = read k_line::Int                -- máximo de iterações
        matrix = parseMatrix matrix_lines  -- matriz

parseMatrix :: [String] -> [[String]]
parseMatrix = map words


loadMatrix :: FilePath -> IO (Int, [[String]])
loadMatrix filename =
    fmap (parseLines . lines) (readFile filename)



----------------------------------------------------------------------------
-- Matriz de Int


parseLines' :: [String] -> (Int, [[Int]])
parseLines' (k_line : matrix_lines) =
    (k, matrix)
    where
        k = read k_line::Int               -- máximo de iterações
        matrix = parseMatrix' matrix_lines  -- matriz

parseMatrix' :: [String] -> [[Int]]
parseMatrix' = map (map read . words)


loadMatrix' :: FilePath -> IO (Int, [[Int]])
loadMatrix' filename =
    fmap (parseLines' . lines) (readFile filename)



