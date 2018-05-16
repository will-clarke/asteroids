module Ship exposing (..)

import Vector exposing (Vector)


shipVelocity : Float
shipVelocity =
    90.0


type alias Ship =
    { position : Vector
    , velocity : Vector
    , angle : Float
    }



-- cos -sin
-- sin  cos


shipCoords : Ship -> List Vector
shipCoords ship =
    [ Vector.add ship.position (Vector.rotate ( 1, 20 ) ship.angle)
    , Vector.add ship.position (Vector.rotate ( 10, -20 ) ship.angle)
    , Vector.add ship.position (Vector.rotate ( -10, -20 ) ship.angle)
    ]


setPosition : ( Float, Float ) -> Ship -> Ship
setPosition ( x, y ) ship =
    Ship ( x, y )
        ship.velocity
        ship.angle


setVelocity : ( Float, Float ) -> Ship -> Ship
setVelocity ( x, y ) ship =
    Ship ship.position
        ( x, y )
        ship.angle


addRelativePosition : ( Float, Float ) -> Ship -> Ship
addRelativePosition ( x, y ) ship =
    let
        ( shipX, shipY ) =
            ship.position
    in
        Ship ( shipX + x, shipY + y )
            ship.velocity
            ship.angle


addRelativeVelocity : ( Float, Float ) -> Ship -> Ship
addRelativeVelocity ( x, y ) ship =
    let
        ( velX, velY ) =
            ship.velocity
    in
        Ship ship.position
            ( velX + x, velY + y )
            ship.angle


addRelativeAngle : Float -> Ship -> Ship
addRelativeAngle angle ship =
    Ship ship.position
        ship.velocity
        (ship.angle + angle)


reduceVelocity : Float -> Ship -> Ship
reduceVelocity n ship =
    let
        ( x, y ) =
            ship.velocity

        newX =
            if x > n then
                x - n
            else if x < -n then
                x + n
            else
                0

        newY =
            if y > n then
                y - n
            else if y < -n then
                y + n
            else
                0
    in
        Ship ship.position
            ( newX, newY )
            ship.angle
