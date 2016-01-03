module Data.Picture where

import Prelude

import Data.Foldable

data Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x = x, y = y }) =
    "(" ++ show x ++ ", " ++ show y ++ ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " ++ show c ++ ", radius: " ++ show r ++ "]"
  show (Rectangle c w h) =
    "Rectangle [center: " ++ show c ++ ", width: " ++ show w ++ ", height: " ++ show h ++ "]"
  show (Line start end) =
    "Line [start: " ++ show start ++ ", end: " ++ show end ++ "]"
  show (Text loc text) =
    "Text [location: " ++ show loc ++ ", text: " ++ show text ++ "]"

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map show
