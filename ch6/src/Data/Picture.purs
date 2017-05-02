module Data.Picture where

import Prelude
import Data.Foldable (foldl, foldr)
import Global as Global
import Math (pi, abs, pow)
import Data.Maybe

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture

type Picture = Array Shape

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
    "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
  show (Text loc text) =
    "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
  show (Clipped picture) = foldr (<>) "" $ map show picture

showPicture :: Picture -> Array String
showPicture = map show

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    min p1.y p2.y
  , left:   min p1.x p2.x
  , bottom: max p1.y p2.y
  , right:  max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped picture) = bounds picture

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    min b1.top    b2.top
  , left:   min b1.left   b2.left
  , bottom: max b1.bottom b2.bottom
  , right:  max b1.right  b2.right
  }

infixl 4 union as \/

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    max b1.top    b2.top
  , left:   max b1.left   b2.left
  , bottom: min b1.bottom b2.bottom
  , right:  min b1.right  b2.right
  }

infixl 4 intersect as /\

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b

makeCircle :: forall r. { x :: Number, y :: Number | r } -> Number -> Shape
makeCircle { x, y } radius = (Circle <<< Point $ { x, y }) radius

area :: Shape -> Number
area (Circle c r)      = pi * r `pow` 2.0
area (Rectangle c w h) = w * h
area _                 = 0.0

leftMost :: Point -> Point -> Point
leftMost pa@(Point a) pb@(Point b)
  | a.x < b.x = pa
  | otherwise = pb

rightMost :: Point -> Point -> Point
rightMost pa@(Point a) pb@(Point b)
  | a.x > b.x = pa
  | otherwise = pb

topMost :: Point -> Point -> Point
topMost pa@(Point a) pb@(Point b)
  | a.y > b.y = pa
  | otherwise = pb

bottomMost :: Point -> Point -> Point
bottomMost pa@(Point a) pb@(Point b)
  | a.y < b.y = pa
  | otherwise = pb

scaleShape :: Shape -> Shape
scaleShape (Circle c r)      = Circle c $ r * 2.0
scaleShape (Rectangle c w h) = Rectangle c (w * 2.0) (h * 2.0)
scaleShape (Line a b)        =
  let
    (Point { x: rx, y: ry }) = rightMost a b
    (Point { x: lx, y: ly }) = leftMost a b
    (Point { y: ty }) = topMost a b
    (Point { y: by }) = bottomMost a b
    xDiff = abs (rx - lx)
    yDiff = abs (ty - by)
    cx = lx + (xDiff / 2.0)
    cy = by + (yDiff / 2.0)
    nrx = cx + ((rx - cx) * 2.0)
    nlx = cx - ((cx - lx) * 2.0)
    nty = cy + ((ty - cy) * 2.0)
    nby = cy - ((cy - by) * 2.0)
  in
    if ly < ry
    then Line (Point { x: nlx, y: nby }) (Point { x: nrx, y: nty })
    else Line (Point { x: nlx, y: nty }) (Point { x: nrx, y: nby })
scaleShape s                 =
  s

showText :: Shape -> Maybe String
showText (Text _ text) = Just text
showText _             = Nothing
