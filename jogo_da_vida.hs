-- Vizinhanca | estado desejado
countStates :: [[Char]] -> Char -> Int
countStates [] _ = 0
countStates (s:ss) c = (length $ filter (==c) s) + countStates ss c

-- Vizinhanca
countAlive :: [[Char]] -> Int
countAlive n = countStates n 'a'

-- Vizinhanca
countDead :: [[Char]] -> Int
countDead n = countStates n 'd'

-- Vizinhanca
countZombies :: [[Char]] -> Int
countZombies n = countStates n 'z'

-- # vivos | # zumbis
runAlive :: Int -> Int -> Char
runAlive a z
    | z >= 1 = 'z'
    | z == 0 && (a > 3 || a < 2) = 'd'
    | otherwise = 'a'

-- # vivos
runDead :: Int -> Char
runDead a
    | a == 3 = 'a'
    | otherwise = 'd'

-- # vivos
runZombie :: Int -> Char
runZombie a
    | a == 0 = 'd'
    | otherwise = 'z'

-- Vizinhanca | valor da celula
runCell :: [[Char]] -> Char -> Char
runCell n cell
    | cell == 'a' = runAlive a z
    | cell == 'd' = runDead a
    | cell == 'z' = runZombie a
    where 
        a = countAlive n
        z = countZombies n

-- Linhas vizinhas | id linha | id coluna
getNeighbourCols :: [[a]] -> Int -> Int -> [[[a]]]

getNeighbourCols (a:b:_) 0 0                                                        -- Linha 0 couna 0 -> vizinhanca 2x2
    | neighbourColsLength == 2 = [neighbourCols]
    | otherwise = neighbourCols:getNeighbourCols [a, b] 0 1
    where
        neighbourCols = [take 2 a, take 2 b]
        neighbourColsLength = length a

getNeighbourCols (a:b:_) 0 _                                                        -- Linha 0 coluna _ -> vizinhanca 2x3 (também trata a última vizinhanca 2x2)
    | neighbourColsLength == 2 = [neighbourCols]
    | otherwise = neighbourCols:getNeighbourCols rowsTail 0 1
    where
        neighbourCols = [take 3 a, take 3 b]
        neighbourColsLength = length a
        rowsTail = [drop 1 a, drop 1 b]

getNeighbourCols (a:b:c:_) _ 0 = neighbourCols:getNeighbourCols [a, b, c] 1 1       -- Linha _ coluna 0 -> vizinhanca 3x2
    where
        neighbourCols = [take 2 a, take 2 b, take 2 c]

getNeighbourCols (a:b:c:_) _ _                                                      -- Linha _ coluna _ -> vizinhaca 3x3 (também trata ultima linha 2x_)
    | neighbourColsLength == 2 = [neighbourCols]
    | otherwise = neighbourCols:getNeighbourCols rowsTail 1 1
    where
        neighbourCols = [take 3 a, take 3 b, take 3 c]
        neighbourColsLength = length a
        rowsTail = [drop 1 a, drop 1 b, drop 1 c]

getNeighbourCols (a:b:_) _ 0                                                        -- Linha última couna 0 -> vizinhanca 2x2
    | neighbourColsLength == 2 = [neighbourCols]
    | otherwise = neighbourCols:getNeighbourCols [a, b] 0 1
    where
        neighbourCols = [take 2 a, take 2 b]
        neighbourColsLength = length a

getNeighbourCols (a:b:_) _ _                                                        -- Linha última coluna _ -> vizinhanca 2x3 (também trata a última vizinhanca 2x2)
    | neighbourColsLength == 2 = [neighbourCols]
    | otherwise = neighbourCols:getNeighbourCols rowsTail 0 1
    where
        neighbourCols = [take 3 a, take 3 b]
        neighbourColsLength = length a
        rowsTail = [drop 1 a, drop 1 b]

-- Matriz | id linha
getNeighbourhoodMatrix :: [[Char]] -> Int -> [[[[Char]]]]
getNeighbourhoodMatrix (a:[]) _ = []
getNeighbourhoodMatrix (a:b:rows) 0 = getNeighbourCols [a, b] 0 0:getNeighbourhoodMatrix (a:b:rows) 1
getNeighbourhoodMatrix rows _ = getNeighbourCols neighbourRows 1 0:getNeighbourhoodMatrix (drop 1 rows) 1
    where
        neighbourRows = take 3 rows


main = do
    let matrix = [['d', 'd', 'a', 'z'], ['z', 'a', 'd', 'd'],  ['z', 'a', 'd', 'd']]
    let neighbours = [['d', 'd', 'a'], ['d', 'd'], ['d', 'd', 'd']]
    let cell = 'a'
    print $ matrix !! 0
    print $ matrix !! 1
    print $ matrix !! 2
    print $ getNeighbourhoodMatrix matrix 0
   
    -- print $ runCell neighbours cell