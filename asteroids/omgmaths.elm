module OmgMaths exposing (..)


type alias Point = (Float, Float)


-- for rotateUnitCircle:
-- cos  -sin
-- sin  cos


unitX : Float -> Point
unitX degrees =
    ( cos degrees, sin degrees )


unitY : Float -> Point
unitY degrees =
    ( negate (sin degrees), cos degrees)


rotateUnitCircle : Float -> Point
rotateUnitCircle degrees =
    add (unitX degrees) (unitY degrees)


add : Point -> Point -> Point
add a b =
    let
        ( ax, ay ) = a

        ( bx, by ) = b
    in
        (ax + bx, ay + by)
