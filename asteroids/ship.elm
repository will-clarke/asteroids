module Ship exposing (..)

import OmgMaths exposing (Point)


shipVelocity : Float
shipVelocity =
    90.0


type alias Velocity =
    { x : Float
    , y : Float
    }


type alias Ship =
    { position : Point
    , velocity : Velocity
    , angle : Float
    }


shipCoords : Ship -> List Point
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
        (Velocity ship.velocity.x ship.velocity.y)
        ship.angle


setVelocity : ( Float, Float ) -> Ship -> Ship
setVelocity ( x, y ) ship =
    Ship ship.position
        (Velocity x y)
        ship.angle


addRelativePosition : ( Float, Float ) -> Ship -> Ship
addRelativePosition ( x, y ) ship =
    let
        ( shipX, shipY ) =
            ship.position
    in
        Ship ( shipX + x, shipY + y )
            (Velocity ship.velocity.x ship.velocity.y)
            ship.angle


addRelativeVelocity : ( Float, Float ) -> Ship -> Ship
addRelativeVelocity ( x, y ) ship =
    Ship ship.position
        (Velocity (ship.velocity.x + x) (ship.velocity.y + y))
        ship.angle


addRelativeAngle : Float -> Ship -> Ship
addRelativeAngle angle ship =
    Ship ship.position
        (Velocity ship.velocity.x ship.velocity.y)
        (ship.angle
            + angle
        )
