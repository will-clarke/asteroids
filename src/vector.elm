module Vector exposing (..)


type alias Vector =
    ( Float, Float )



-- for rotateUnitCircle:
-- cos  -sin
-- sin  cos


rotate : Vector -> Float -> Vector
rotate vec angle =
    let
        ( x, y ) =
            vec

        x_ =
            (x * cos angle) - (y * sin angle)

        y_ =
            (x * sin angle) + (y * cos angle)


    in
        ( x_, y_ )


unitXRotated : Float -> Vector
unitXRotated degrees =
    ( cos degrees, sin degrees )


unitYRotated : Float -> Vector
unitYRotated degrees =
    ( negate (sin degrees), cos degrees )


rotateUnitCircle : Float -> Vector
rotateUnitCircle degrees =
    add (unitXRotated degrees) (unitYRotated degrees)


mult : Vector -> Vector -> Vector
mult a b =
    let
        ( ax, ay ) =
            a

        ( bx, by ) =
            b
    in
        ( ax * bx, ay * by )


multScalar : Vector -> Float -> Vector
multScalar vector constant =
    let
        ( x, y ) =
            vector
    in
        ( x * constant, y * constant )


add : Vector -> Vector -> Vector
add a b =
    let
        ( ax, ay ) =
            a

        ( bx, by ) =
            b
    in
        ( ax + bx, ay + by )
