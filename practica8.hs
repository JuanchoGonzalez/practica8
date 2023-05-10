nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True 

maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _       = False

existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys f = or [f x ys | x <- xs]

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys f = and [f x ys | x <- xs]

paraTodo2 :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo2 xs ys f = foldr (&&) True [f x ys | x <- xs]

isEven2 :: Int -> [Int] -> Bool 
isEven2 i xs = xs !! i `mod` 2 == 0 

isEven :: (Eq a,Integral a) => Int -> [a] -> Bool 
isEven i xs = mod (xs!!i) 2 == 0 

productoria :: (Num a) => [Int] -> [a] -> (Int -> [a] -> a) -> a
productoria xs [] f = 1
productoria xs ys f = product [f x ys | x <- xs] 

cuadrado :: (Num a) => Int -> [a] -> a
cuadrado i xs = (xs!!i) ^ 2

sumatoria :: (Num a) => [Int] -> [a] -> (Int -> [a] -> a) -> a
sumatoria xs [] f = 0
sumatoria xs ys f = sum [f x ys | x <- xs] 
{-
contatoria :: (Num a) => [Int] -> [a] -> (Int -> [a] -> Bool) -> Int
contatoria xs [] f = 0
contatoria xs ys f = length [f x ys | x <- ys] 
-}

-- ejemplos concretos con otro perfil

sumatoriaPares :: [Int] -> Int 
sumatoriaPares [] = 0 -- elemento neutro para la operacion suma
sumatoriaPares (x:xs) = sum [x | x <- xs, even x] 

productoriaDivisoresDe3 :: [Int] -> Int
productoriaDivisoresDe3 [] = 1 -- elemento neutro para la operacion producto
productoriaDivisoresDe3 xs = product [x | x <- xs, mod x 3 == 0]

contatoriaImparesMayA15 :: [Int] -> Int
contatoriaImparesMayA15 [] = 0 -- elemento neutro para la operacion contatoria
contatoriaImparesMayA15 xs = length [x | x <- xs, odd x && x > 15]
