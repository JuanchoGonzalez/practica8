nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True 

maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _       = False

existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys f = or [f x | x <- xs]

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys f = and [f x | x <- xs]

--f :: [a] -> Bool

existeUnNMayA0 :: [Int] -> Bool
existeUnNMayA0 xs = or [x > 10 | x <- xs]

todosSonPos :: [Int] -> Bool
todosSonPos xs = and [x > 0 | x <- xs]

sumatoriaPares :: [Int] -> Int 
sumatoriaPares [] = 0 -- elemento neutro para la operacion suma
sumatoriaPares (x:xs) = sum [x | x <- xs, even x] 

productoriaDivisoresDe3 :: [Int] -> Int
productoriaDivisoresDe3 [] = 1 -- elemento neutro para la operacion producto
productoriaDivisoresDe3 xs = product [x | x <- xs, mod x 3 == 0]


contatoriaImparesMayA15 :: [Int] -> Int
contatoriaImparesMayA15 [] = 0 -- elemento neutro para la operacion contatoria
contatoriaImparesMayA15 xs = length [x | x <- xs, odd x && x > 15]
