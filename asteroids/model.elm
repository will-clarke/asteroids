module Model exposing (..)
import Ship
import Vector exposing (Vector)
import Set
import Keyboard

addRelativeVelocityToShip: ( Float, Float ) -> Model -> Model
addRelativeVelocityToShip coordPair model =
    { model | ship = Ship.addRelativeVelocity coordPair model.ship }


addRelativeAngleToShip : Float -> Model -> Model
addRelativeAngleToShip angle model =
    { model | ship = Ship.addRelativeAngle angle model.ship }


addRelativePositionToShip : ( Float, Float ) -> Model -> Model
addRelativePositionToShip coordPair model =
    { model | ship = Ship.addRelativePosition coordPair model.ship }


type alias Model =
    { ship : Ship.Ship
    , paused : Bool
    , windowSize : Vector
    , keysDown : Set.Set Keyboard.KeyCode
    }

