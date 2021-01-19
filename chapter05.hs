-- カリー化関数
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
multTwoWithNine = multThree 9
{- 実行例
multTwoWithNine 2 3
54
-}

-- Intを引数に取って100と比較する関数
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x
-- こちらも可
--compareWithHundred = compare 100

-- 中置関数をセクションする
-- (-4)のように書くとマイナス4を意味するので(subtract 4)にする
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- 与えられた文字が大文字かどうか調べる関数
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- 関数を受け取り、それを2回適用する関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
{- 実行例
applyTwice (+3) 10
16
applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
applyTwice ("HAHA "　++) "HEY"
"HAHA HAHA HEY"
applyTwice (multThree 2 2) 9
144
applyTwice (3:) [1]
[3, 3, 1]
-}

-- 高階関数でzipWithを実装
-- zipWith：関数と2つのリストを引数に取り、2つのリストの各要素にその関数を適用することで、2つのリストを1つに結合する
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
{- 実行例
zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
-}

-- 高階関数でflipを実装
-- flip：関数を引数に取り、2つの引数が入れ替わった関数を返す
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- こちらも可
--flip' f y x = f x y
{- 実行結果
zip [1,2,3,4,5] "hello"
[(1,'h'),(2,'e'),(3,'l'),(4,'l'),(1,'o')]
flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
zipWith div [2,2..] [10,8,6,4,2]
[0,0,0,0,1]
zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]
-}

-- 関数とリストを受け取り、その関数をリストのすべての要素に適用して新しいリストを生成
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

-- 述語(真理値を返す関数)とリストを受け取り、そのリストの要素のうち、述語を満たすもののみからなるリストを返す
-- filter :: (a -> Bool) -> [a] -> [b]
-- filter _ [] = []
-- filter p (x:xs)
--     | p x = x : filter p xs
--     | otherwise = filter p xs

-- filterを使ったクイックソート
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- 10万以下の数のうち3829で割り切れる最大の数を探す
-- largestDivisible :: Integer
-- largestDivisible head (filter p [100000, 99999..])
--     where p x = x `mod` 3829 == 0

-- コラッツの数列を生成する関数
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

-- 1から100までの数のうち、長さ15以上のコラッツ列の開始数になるものはいくつあるかを解く
-- numLongChains :: Int
-- numLongChains length  (filter isLong (map chain [1..100]))
--     where isLong xs = length xs > 15

-- ラムダ式を使ったnumLongChains
-- numLongChains :: Int
-- numLongChains length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- ラムダ式を使うと読みにくい場合も
-- addThree :: Int -> Int -> Int -> Int
-- addThree :: x y z = x + y + z

-- ↑の式と同じだが読みづらい
-- addThree' :: Int -> Int -> Int -> Int
-- addThree' = \x -> \y -> \z -> x + y + z

-- flip`など読みやすい場合もある
-- flip' :: (a -> b -> c) -> b -> a -> c
-- flip' f = \x y -> f y x

-- foldlで左畳み込み
-- jsで言うArray.prototype.reduce
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- もっと簡潔に書ける
-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0

-- foldrで右畳み込み
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- 左畳み込みでも出来るが++関数は:よりもはるかに遅いので、リストから新しいリストを構築する際には普通は右畳み込みを使う
--map' f xs = foldl (\x acc -> acc ++ [f x]) [] xs

-- 右畳み込みは無限リストに対しても動作する（左畳み込みは×）
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl1とfoldr1はリストの先頭または末尾の要素を初期アキュムレータにするので初期値不要
maximamu' :: (Ord a) => [a] -> a
maximamu' = foldl1 max

-- 畳み込みを使ったreverse関数
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
--引数が逆になっているのを除けば単なる:関数なのでflipを使える
--reverse' = foldl (flip (:)) []

-- 畳み込みを使ったproduct関数
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- 畳み込みを使ったfilter関数
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- 畳み込みを使ったlast関数
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- foldrで実装したand関数
-- 右畳み込みなので無限リストも対応している
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- 自然数の平方根を小さいものから足していったときに1000を超えるのは何個目か求める
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $は関数適用演算子：右側の式が左側の関数に引数として渡される
-- 例．sqrt $ 3 + 4 + 9 は sqrt (3 + 4 + 9)と同じ

-- 関数合成は .
-- sum (replicate 5 (max 6.7 8.9)) は sum . replicate 5 $ max 6.7 8.9 とも書ける

-- 両側のxsを省略できる：ポイントフリースタイル
sum' :: (Num a) => [a] => a
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0

-- 奇数の平方数で10000より小さいものの総和を求める関数
oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 関数合成を使うと以下のようになる
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
