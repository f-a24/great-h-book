-- パターンマッチ

-- 渡された数が7かどうか調べる関数
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- 1から5の単語を出力する関数
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- 再帰的に定義することも可能
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 予期しない値が入力されるとエラーになるので、すべてに合致するパターンを最後に入れる
-- ↓ NG
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- タプルでも使える
{- パターンマッチではない方法方法
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)
-}
-- パターンマッチを使って書き換え
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- トリプルの3番目の要素を取り出す関数は提供されていないのでを独自に定義
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = x

third :: (a, b, c) -> c
third (_, _, z) = x

-- パターンマッチを使った独自のhead関数
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- リストの要素を回りくどくて不便な書式で出力する関数
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
-- ([x])でも可
tell (x:[]) = "The list has one element: " ++ show x
-- ([x,y])でも可
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- 予期しないリスト([1, 2]など)が与えられるとエラーになる
-- ↓ NG
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

-- asパターン：例えば、xs@(x:y:ys)の場合はx:y:ysに合致するものとまったく同じものに合致しつつ、xsで元のリスト全体にアクセスすることも可能
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- ガード

-- 計算済みのBMIを受け取って忠告するだけの関数
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.\
    \ Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

-- BMIの計算もするように変更
bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal.\
    \ Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

-- ガードを使った独自のmax関数
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

-- ガードを使った独自のcompare関数
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT


-- where

-- BMIの計算をwhereキーワードを使って書き換え
-- whereブロックの変数は複数可能、インデントは揃える
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal.\
    \ Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- where節で定義したは変数は、その関数からしか見えず、関数の違うパターンでも共有されない
{- NG
greet :: String -> String
greet "Juan" ++ niceGreeting ++ " Juan!"
greet "Fernando" ++ niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name
    where niceGreeting = "Hello! So very nice to see you,"
          badGreeting = "Oh! Pffft. It's you."
-}

-- ↑を正しく動かすにはグローバルに定義する
badGreeting :: String
badGreeting = "Oh! Pffft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" ++ niceGreeting ++ " Juan!"
greet "Fernando" ++ niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

-- whereの束縛の中でパターンマッチを使うことができる
-- ファーストネームとラストネームを受け取ってイニシャルを返す関数
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- whereブロックの中では定数だけでなく巻子も定義可能
-- 体重と身長のペアのリストを受け取ってBMIのリストを返す関数
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- let

-- let式は let bindings in expressionという形をとる
-- 円柱の表面積を高さと半径から求める関数
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- 体重と身長のペアのリストを受け取ってBMIのリストを返す関数
-- letを使って書き換え
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- letをリスト内包表記で使うと出力(|より前の部分)とそのletより後ろのリスト内包表記の全てから見える
-- 肥満な人のBMIのみを返す関数
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]


-- case

{- パターンマッチを使った独自のhead関数
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
-}

-- パターンマッチはcase式の構文糖衣
-- case式を使った独自のhead関数
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- case式はどこでも使える
describeList :: [a] -> String
describeList ls = "The list is "
    ++ case ls of [] -> "empty."
                  [x] -> "a singleton list."
                  xs -> "a longer list."

-- 関数定義でのパターンマッチはcase式と同じように使える
describeList :: [a] -> String
describeList ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
