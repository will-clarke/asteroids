module Main exposing (..)

import AnimationFrame
import Html
import Key
import Keyboard
import Set
import Svg
import Svg.Attributes
import Task
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Ship =
    { x : Float, y : Float, vx : Float, vy : Float }


type alias WindowSize =
    { x : Int, y : Int }


type alias Model =
    { ship : Ship
    , paused : Bool
    , windowSize : WindowSize
    , keysDown : Set.Set Keyboard.KeyCode
    , log : String
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
                | windowSize = WindowSize size.width (size.height - 50)
                , ship = Ship ((toFloat size.width) / 2.0) ((toFloat size.height) / 2.0) model.ship.vx model.ship.vx
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
    ( updateVelocity dt model, Cmd.none )


updateVelocity : Float -> Model -> Model
updateVelocity dt model =
    let ship = model.ship
    in
    { model | ship = Ship (ship.x + ship.vx * dt / 10) (ship.y + ship.vy * dt / 10) ship.vx ship.vy }


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
            Key.Up ->
                { model | ship = Ship ship.x ship.y ship.vx (ship.vy - 10) }

            Key.Down ->
                { model | ship = Ship ship.x ship.y ship.vx (ship.vy + 10) }

            Key.Left ->
                { model | ship = Ship ship.x ship.y (ship.vx - 10) ship.vy }

            Key.Right ->
                { model | ship = Ship ship.x ship.y (ship.vx + 10) ship.vy }

            _ ->
                model


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ship 50 50 0 0)
        False
        (WindowSize 0 0)
        Set.empty
        ""
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
    Html.div []
        [ Html.text
            ("Keydowns: "
                ++ (toString model.keysDown)
                ++ " --- Ship.x: "
                ++ (toString model.ship.x)
                ++ " --- right button pressed: "
                ++ (toString (Set.member 39 model.keysDown))
            )
        , Html.br [] []
        , Svg.svg
            [ Svg.Attributes.width (toString model.windowSize.x)
            , Svg.Attributes.height (toString model.windowSize.y)
            , Svg.Attributes.viewBox "0 0 (toString model.windowSize.x)(toString model.windowSize.y)"
            ]
            [ Svg.circle
                [ Svg.Attributes.cx (toString model.ship.x)
                , Svg.Attributes.cy (toString model.ship.y)
                , Svg.Attributes.r "40"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.fill "red"
                ]
                []
            ]
        ]
