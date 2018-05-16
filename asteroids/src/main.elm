module Main exposing (..)

import AnimationFrame
import Collage
import Color
import Element
import Html
import Key
import Keyboard
import Model exposing (Model)
import Set
import Ship
import Task
import Text
import Vector
import Window










-- ====================================
-- Shooting
-- ====================================


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Direction
    = Up
    | Down
    | Left
    | Right


type Msg
    = Move Direction
    | PauseToggle
    | SetWindowSize Window.Size
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            ( model, Cmd.none )

        PauseToggle ->
            ( { model | paused = not model.paused }, Cmd.none )

        SetWindowSize size ->
            ( { model
                | windowSize = ( toFloat size.width, toFloat (size.height - 50) )
              }
            , Cmd.none
            )

        KeyDown keycode ->
            ( keysPressed { model | keysDown = Set.insert keycode model.keysDown }, Cmd.none )

        KeyUp keycode ->
            ( { model | keysDown = Set.remove keycode model.keysDown }, Cmd.none )

        Tick dt ->
            step dt model


step : Float -> Model -> ( Model, Cmd Msg )
step dt model =
    let
        m =
            model
                |> updateVelocity dt
                |> ensureWorldWrap
    in
        ( m, Cmd.none )


ensureWorldWrap : Model -> Model
ensureWorldWrap model =
    let
        ( shipX, shipY ) =
            model.ship.position

        ( windowX, windowY ) =
            model.windowSize
    in
        if shipX > (windowX / 2) then
            Model.addRelativePositionToShip ( negate windowX, 0 ) model
        else if shipX < (negate windowX / 2) then
            Model.addRelativePositionToShip ( windowX, 0 ) model
        else if shipY > (windowY / 2) then
            Model.addRelativePositionToShip ( 0, negate windowY ) model
        else if shipY < (negate (windowY) / 2) then
            Model.addRelativePositionToShip ( 0, windowX ) model
        else
            model


updateVelocity : Float -> Model -> Model
updateVelocity dt model =
    let
        ship =
            model.ship

        ( posX, posY ) =
            ship.position

        ( velX, velY ) =
            ship.velocity

        newPositionX =
            posX + velX * dt / Ship.shipVelocity

        newPositionY =
            posY + velY * dt / Ship.shipVelocity
    in
        { model
            | ship =
                Ship.Ship ( newPositionX, newPositionY )
                    ship.velocity
                    ship.angle
        }


keysPressed : Model -> Model
keysPressed model =
    let
        foldlFunction keycode model =
            keyPressed keycode model
    in
        Set.foldl foldlFunction model model.keysDown


keyPressed : Keyboard.KeyCode -> Model -> Model
keyPressed keycode model =
    let
        ship =
            model.ship
    in
        case Key.fromCode keycode of
            Key.Q ->
                { model | ship = Ship.reduceVelocity 10.0 model.ship }

            Key.Meta ->
                { model | ship = Ship.reduceVelocity 10.0 model.ship }

            Key.Space ->
                { model | ship = Ship.reduceVelocity 10.0 model.ship }

            Key.Up ->
                model
                    |> Model.addRelativeVelocityToShip (Vector.rotate ( 0, 2 ) ship.angle)

            Key.Down ->
                model
                    |> Model.addRelativeVelocityToShip (Vector.rotate ( 0, -2 ) ship.angle)

            Key.Left ->
                model
                    |> Model.addRelativeAngleToShip 0.1

            Key.Right ->
                model
                    |> Model.addRelativeAngleToShip -0.1

            _ ->
                model


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ship.Ship ( 0, 0 ) ( 0, 0 ) 0)
        False
        ( 0, 0 )
        Set.empty
    , Task.perform SetWindowSize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\winSize -> SetWindowSize winSize)
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]


view : Model -> Html.Html Msg
view model =
    Collage.collage
        (round (Tuple.first model.windowSize))
        (round (Tuple.second model.windowSize))
        [ Collage.text (Text.fromString (toString model.keysDown))
        , Collage.polygon (Ship.shipCoords model.ship)
            |> Collage.filled Color.green
        ]
        |> Element.toHtml
