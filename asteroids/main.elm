module Main exposing (..)

import AnimationFrame
import Collage
import Color
import Element
import Html
import Key
import Keyboard
import Set
import Task
import Window

shipVelocity : Float
shipVelocity = 90.0

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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


addRelativeVelocityToModel : ( Float, Float ) -> Model -> Model
addRelativeVelocityToModel coordPair model =
    { model | ship = addRelativeVelocity coordPair model.ship }


addRelativeAngleToModel : Float -> Model -> Model
addRelativeAngleToModel angle model =
    { model | ship = addRelativeAngle angle model.ship }


addRelativePositionToModel : ( Float, Float ) -> Model -> Model
addRelativePositionToModel coordPair model =
    { model | ship = addRelativePosition coordPair model.ship }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Velocity =
    { x : Float
    , y : Float
    }


type alias Ship =
    { position : Position
    , velocity : Velocity
    , angle : Float
    }


type alias WindowSize =
    { x : Int, y : Int }


type alias Model =
    { ship : Ship
    , paused : Bool
    , windowSize : WindowSize
    , keysDown : Set.Set Keyboard.KeyCode
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
    if model.ship.position.x > ((toFloat model.windowSize.x) / 2) then
        addRelativePositionToModel ( negate (toFloat model.windowSize.x), 0 ) model
    else if model.ship.position.x < (negate (toFloat model.windowSize.x) / 2) then
        addRelativePositionToModel ( (toFloat model.windowSize.x), 0 ) model
    else if model.ship.position.y > ((toFloat model.windowSize.y) / 2) then
        addRelativePositionToModel ( 0, negate (toFloat model.windowSize.y) ) model
    else if model.ship.position.y < (negate (toFloat model.windowSize.y) / 2) then
        addRelativePositionToModel ( 0, (toFloat model.windowSize.x) ) model
    else
        model


updateVelocity : Float -> Model -> Model
updateVelocity dt model =
    let
        ship =
            model.ship

        newPositionX =
            ship.position.x + ship.velocity.x * dt / shipVelocity

        newPositionY =
            ship.position.y + ship.velocity.y * dt / shipVelocity
    in
        { model
            | ship =
                Ship (Position newPositionX newPositionY)
                    (Velocity ship.velocity.x ship.velocity.y)
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
            Key.Up ->
                model
                    |> addRelativeVelocityToModel ( 0, 2 )

            Key.Down ->
                model
                    |> addRelativeVelocityToModel ( 0, -2 )

            Key.Left ->
                model
                    -- |> addRelativeVelocityToModel ( -10, 0 )
                    |> addRelativeAngleToModel 0.1

            Key.Right ->
                model
                    -- |> addRelativeVelocityToModel ( 10, 0 )
                    |> addRelativeAngleToModel -0.1

            _ ->
                model


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ship (Position 0 0) (Velocity 0 0) 0)
        False
        (WindowSize 0 0)
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
        model.windowSize.x
        model.windowSize.y
        [ Collage.polygon (shipCoords model.ship)
            |> Collage.filled Color.green
            |> Collage.rotate model.ship.angle
        ]
        |> Element.toHtml


shipCoords : Ship -> List ( Float, Float )
shipCoords ship =
    [ ( ship.position.x, ship.position.y + 20 )
    , ( ship.position.x + 10, ship.position.y - 20 )
    , ( ship.position.x - 10, ship.position.y - 20 )
    ]
