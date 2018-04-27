module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (regex)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


model : Model
model =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , viewValidation model
        ]


passwordIsLongEnough : String -> Bool
passwordIsLongEnough password =
    (String.length password) > 8


containsNumbers : String -> Bool
containsNumbers string =
    Regex.contains (regex "\\d") string


containsUppercaseLetters : String -> Bool
containsUppercaseLetters string =
    Regex.contains (regex "[A-Z]+") string


containsLowercaseLetters : String -> Bool
containsLowercaseLetters string =
    Regex.contains (regex "[a-z]+") string

passwordContainsFunkyLetters : String -> Bool
passwordContainsFunkyLetters password =
    containsUppercaseLetters password
        && containsLowercaseLetters password
        && containsNumbers password

viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( color, message ) =
            if not (passwordIsLongEnough model.password) then
                ( "orange", "Password Not Long enough!: It's " ++ toString (String.length model.password) ++ " characters" )
            else if model.password == model.passwordAgain && passwordContainsFunkyLetters model.password then
                ( "green", "OK" )
            else
                ( "red", "Passwords do not match!" )
    in
        div [ style [ ( "color", color ) ] ] [ text message ]
