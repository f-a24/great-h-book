-- 文字列から大文字だけを残す関数に型宣言を与える
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- 複数の引数を持つ場合の型宣言
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Int、Integerは整数
-- Intは有界（最小値と最大値あり）
-- Integerは有界ではないのでとても大きい数を扱える
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Floatは単精度浮動小数点数
cirumference :: Float -> Float
cirumference r = 2 * pi * r

-- Doubleは倍精度浮動小数点数
cirumference' :: Double -> Double
cirumference' r = 2 * pi * r
