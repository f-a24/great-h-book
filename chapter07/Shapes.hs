-- chapter07
-- 型名の後に(値コンストラクタ名)や(..)することで自作のデータ型もエクスポート可
-- (..)などを付けないと値コンストラクタがエクスポートされない
module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where

-- dataキーワードを使うと自作のデータ型を作れる
-- Circle（円）：最初の2つのフィールドは円の中心座標、3つめのフィールドは円の半径
-- Rectangle（長方形）：最初の2は左下の角、あとの2つは右上の角の座標
-- data Shape = Circle Float Float Float |
--              Rectangle Float Float Float Float
--     deriving (Show) -- Show型クラスのインスタンスにしておけば、プロンプトからも表示できる
-- Circle :: Float -> Float -> Float -> Shape
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

-- CircleやRectangleは値コンストラクタなのでCircle -> Floatのようには書けない
-- area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 二次元空間の点を表す中間データ構造
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Circleパターンでは点全体を無視
-- Rectangleパターンではパターンマッチをネストすることで一気に点が属するフィールドへアクセスする
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 図形を動かす関数
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- 指定したサイズの円を原点に作る補助関数
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

-- 指定したサイズの長方形を原点に作る補助関数
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
