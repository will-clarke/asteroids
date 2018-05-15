module OmgMaths exposing (..)


type alias Vector = (Float, Float)


-- for rotateUnitCircle:
-- cos  -sin
-- sin  cos


unitX : Float -> Vector
unitX degrees =
    ( cos degrees, sin degrees )


unitY : Float -> Vector
unitY degrees =
    ( negate (sin degrees), cos degrees)


rotateUnitCircle : Float -> Vector
rotateUnitCircle degrees =
    add (unitX degrees) (unitY degrees)


add : Vector -> Vector -> Vector
add a b =
    let
        ( ax, ay ) = a

        ( bx, by ) = b
    in
        (ax + bx, ay + by)
