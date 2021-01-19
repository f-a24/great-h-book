import Data.List
import Data.Char
import qualified Data.Map as Map

-- nubとsortのみ
-- import Data.List (nub, sort)

-- nub以外全部
-- import Data.List hiding (nub)

-- 修飾子付き
-- import qualified Data.Map as M
-- 使うときはM.filterなど

-- nubは重複する要素を取り除く
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- 各単語が何回現れるかを求める
-- group・sort・wordsはData.Listモジュールに含まれる
wordsNums :: String -> [(String, Int)]
wordsNums = map (\xs -> (head ws, length ws)) . group . sort . words

-- 2つのリストを受け取り、1つ目のリストが2つ目のリストのどこかに含まれているかを調べる関数
-- anyとtailsはData.Listモジュールに含まれる
-- isIn関数はData.ListモジュールのisInfixOfと同じ
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- 文字をシフトする数と文字列を受け取り、文字列中の各文字をアルファベット上で指定された数だけ前方向にシフトする関数
-- シーザー暗号
-- chrとordはData.Charモジュールに含まれる
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

-- メッセージの複合
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- 数を引数に取って、その各桁の数の合計を返す関数
-- digitToIntはData.Charモジュールに含まれる
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

-- 各桁の数の合計が40になる最初の自然数を求める関数
-- findはData.Listモジュールに含まれる
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- 各桁の数の合計がnになる最初の自然数を求める関数
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- 与えられたキーに対して値を検索する関数
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- 探しているキーが無い場合はエラーになってしまうのでMaybe型を使う
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k = Just v
    | otherwise findKey key xs

-- 畳み込みを使ったパターン
-- リストに対する標準的な再帰パターンは畳み込みを使った方が良い
-- 実装したfindKeyはData.Listにあるlookup関数
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey = key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- 連想リストをMapに変換
-- Mapにはlookupやinsert、sizeなど便利な関数がある
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("hoge", "111-1111")
    ,("foo", "222-2222")
    ,("bar", "333-3333")
    ]

-- 文字列から数字だけのリストに変換する関数
-- isDigitはData.Charモジュールに含まれる
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- fromListはキーの重複を削除するが、fromListWithは重複時にどうするかを決める関数を受け取る
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", "　++ number2

-- あらかじめ連想リストの値を単一要素のリストにしておけば、++が使える
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs


-- 自分で作った（エクスポートした）モジュールは同じフォルダにないといけない
-- import chapter06.Geometry
-- import qualified chapter06.Geometry.Sphere as Sphere
-- import qualified chapter06.Geometry.Cuboid as Cuboid
-- import qualified chapter06.Geometry.Cube as Cube
