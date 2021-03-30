type Grid a = [[a]]

data GameState = GameState {
    grid :: Grid String,
    nIteration :: Int
}   deriving (Show)

-- Vizinhanca | estado desejado
countStates :: Grid String -> String -> Int
countStates [] _ = 0
countStates (s:ss) c = (length $ filter (==c) s) + countStates ss c

-- Vizinhanca
countAlive :: Grid String -> Int
countAlive n = countStates n "alive"

-- Vizinhanca
countDead :: Grid String -> Int
countDead n = countStates n "dead"

-- Vizinhanca
countZombies :: Grid String -> Int
countZombies n = countStates n "zombie"

-- # vivos | # zumbis
runAlive :: Int -> Int -> String
runAlive a z
    | z >= 1 = "zombie"
    | z == 0 && (a > 3 || a < 2) = "dead"
    | otherwise = "alive"

-- # vivos
runDead :: Int -> String
runDead a
    | a == 3 = "alive"
    | otherwise = "dead"

-- # vivos
runZombie :: Int -> String
runZombie a
    | a == 0 = "dead"
    | otherwise = "zombie"

-- Vizinhanca | valor da celula
runCell :: Grid String -> String -> String
runCell n cell
    | cell == "alive" = runAlive a z
    | cell == "dead" = runDead a
    | cell == "zombie" = runZombie a
    where 
        a = countAlive n
        z = countZombies n

-- Linhas vizinhas | id coluna | tamanho horizontal vizinhança
getNeighbourCols :: Grid String -> Int -> Int -> [Grid String]
getNeighbourCols (a:b:[]) col size
    | col == 0 = [na, nb]:getNeighbourCols [a, b] (col+1) 3                                         -- Primeira viznhinhança da linha
    | length na == 2 = [[na, nb]]                                                                   -- Última vizinhana da linha
    | otherwise = [na, nb]:getNeighbourCols [drop 1 a, drop 1 b] (col+1) 3                          -- Demais vizinhanças
    where
        na = take size a
        nb = take size b
getNeighbourCols (a:b:c:_) col size
    | col == 0 = [na, nb, nc]:getNeighbourCols [a, b, c] (col+1) 3                                  -- Primeira viznhinhança da linha
    | length na == 2 = [[na, nb, nc]]                                                               -- Última vizinhana da linha
    | otherwise = [na, nb, nc]:getNeighbourCols [drop 1 a, drop 1 b, drop 1 c] (col+1) 3            -- Demais vizinhanças
    where 
        na = take size a
        nb = take size b
        nc = take size c

-- Matriz | id linha
getNeighbourhoodMatrix :: Grid String -> Int -> Grid (Grid String)
getNeighbourhoodMatrix (a:[]) _ = []
getNeighbourhoodMatrix (a:b:rows) 0 = getNeighbourCols [a, b] 0 2:getNeighbourhoodMatrix (a:b:rows) 1
getNeighbourhoodMatrix rows row = getNeighbourCols neighbourRows 0 2:getNeighbourhoodMatrix (drop 1 rows) (row+1)
    where
        neighbourRows = take 3 rows

main = do
    let matrix = [["dead", "dead", "alive", "zombie"], ["zombie", "alive", "dead", "dead"],  ["zombie", "alive", "dead", "dead"]]
    let grid = GameState matrix 0
    print $ grid
    print $ getNeighbourhoodMatrix matrix 0