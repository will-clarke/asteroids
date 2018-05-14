module Ship exposing (..)

import OmgMaths exposing (Position)


shipVelocity : Float
shipVelocity =
    90.0


type alias Velocity =
    { x : Float
    , y : Float
    }


type alias Ship =
    { position : Position
    , velocity : Velocity
    , angle : Float
    }


setPosition : ( Float, Float ) -> Ship -> Ship
setPosition ( x, y ) ship =
    Ship (Position x y)
        (Velocity ship.velocity.x ship.velocity.y)
        ship.angle


setVelocity : ( Float, Float ) -> Ship -> Ship
setVelocity ( x, y ) ship =
    Ship (Position ship.position.x ship.position.y)
        (Velocity x y)
        ship.angle


addRelativePosition : ( Float, Float ) -> Ship -> Ship
addRelativePosition ( x, y ) ship =
    Ship (Position (ship.position.x + x) (ship.position.y + y))
        (Velocity ship.velocity.x ship.velocity.y)
        ship.angle


addRelativeVelocity : ( Float, Float ) -> Ship -> Ship
addRelativeVelocity ( x, y ) ship =
    Ship (Position ship.position.x ship.position.y)
        (Velocity (ship.velocity.x + x) (ship.velocity.y + y))
        ship.angle


addRelativeAngle : Float -> Ship -> Ship
addRelativeAngle angle ship =
    Ship (Position ship.position.x ship.position.y)
        (Velocity ship.velocity.x ship.velocity.y)
        (ship.angle
            + angle
        )
