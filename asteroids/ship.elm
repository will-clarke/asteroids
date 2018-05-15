module Ship exposing (..)

import OmgMaths exposing (Vector)


shipVelocity : Float
shipVelocity =
    90.0


type alias Ship =
    { position : Vector
    , velocity : Vector
    , angle : Float
    }


shipCoords : Ship -> List Vector
shipCoords ship =
    let
        ( shipX, shipY ) =
            ship.position
    in
        [ ( shipX, shipY + 20 )
        , ( shipX + 10, shipY - 20 )
        , ( shipX - 10, shipY - 20 )
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
