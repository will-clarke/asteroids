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
    , age : String
    }


model : Model
model =
    Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = age }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "text", placeholder "Age", onInput Age ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , viewValidation model
        ]


type alias Validation =
    { errorMessage : String
    , condition : Bool
    }


validations : Model -> List Validation
validations model =
    [ Validation "Age should be a number" (Regex.contains (Regex.regex "^\\d+$") model.age)
    , Validation "shouldBeLongPassword" (String.length model.password >= 8)
    , Validation "containsNumbers" (Regex.contains (Regex.regex "\\d") model.password)
    , Validation "containsUppercaseLetters" (Regex.contains (Regex.regex "[A-Z]+") model.password)
    , Validation "containsLowercaseLetters" (Regex.contains (Regex.regex "[a-z]+") model.password)
    , Validation "passwordsAreTheSame" (model.password == model.passwordAgain)
    ]


relevantValidations : Model -> List String
relevantValidations model =
    List.filter (\a -> not a.condition) (validations model)
        |> List.map (\a -> a.errorMessage)


type alias ColourText =
    { colour : String, text : String }


colourAndMessages : Model -> List ColourText
colourAndMessages model =
    case relevantValidations model of
        [] ->
            [ ColourText "Green" "OK" ]

        errorMessages ->
            List.map (\a -> ColourText "Red" a) errorMessages


viewValidation : Model -> Html Msg
viewValidation model =
    colourAndMessages model
        |> List.map (\l -> li [ style [ ( "color", l.colour ) ] ] [ text l.text ])
        |> ul []
