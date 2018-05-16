module Bullet exposing (..)

import Vector exposing (Vector)
import Ship exposing (Ship)


type alias Bullet =
    { position : Vector
    , velocity : Vector
    }


fire : Ship -> List Bullet -> List Bullet
fire ship bullets =
    let
        newBullet =
            Bullet ship.position ship.velocity
    in
        newBullet :: bullets
