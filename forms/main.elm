module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (regex)


main : Program Never Model Msg
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
    (String.length password) >= 8


containsNumbers : String -> Bool
containsNumbers string =
    Regex.contains (Regex.regex "\\d") string


containsUppercaseLetters : String -> Bool
containsUppercaseLetters string =
    Regex.contains (Regex.regex "[A-Z]+") string


containsLowercaseLetters : String -> Bool
containsLowercaseLetters string =
    Regex.contains (Regex.regex "[a-z]+") string


passwordsAreTheSame : String -> String -> Bool
passwordsAreTheSame a b =
    a == b


shouldBeLongPassword : String -> Bool
shouldBeLongPassword password =
    String.length password > 8


validationsAndMessages : String -> List ( String -> Bool, String )
validationsAndMessages otherPassword =
    [ ( shouldBeLongPassword, "should be long password" )
    , ( containsUppercaseLetters, "should have upper" )
    , ( containsLowercaseLetters, "should have lower" )
    , ( containsNumbers, "should have number" )
    , ( passwordsAreTheSame otherPassword, "Passwords Should be the same" )
    ]


relevantValidationMesage : String -> String -> Maybe String
relevantValidationMesage password passwordAgain =
    let
        filterFunction tuple =
            not ((Tuple.first tuple) password)
    in
        List.filter filterFunction (validationsAndMessages passwordAgain)
            |> List.map (\a -> Tuple.second a)
            |> List.head


colourAndMessage : String -> String -> ( String, String )
colourAndMessage password passwordAgain =
    case relevantValidationMesage password passwordAgain of
        Just a ->
            ( "Red", a )

        Nothing ->
            ( "Green", "OK" )


viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( color, message ) =
            colourAndMessage model.password model.passwordAgain
    in
        div [ style [ ( "color", color ) ] ] [ text message ]
