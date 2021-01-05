-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (Model, Msg(..), PasswordStrength, init, isMaxStrength, isStrongPassword, main, update, updatePasswordStrength, validatePassword, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
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

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


type alias PasswordStrength =
    { hasUppercase : Bool
    , hasLowercase : Bool
    , numDigits : Int
    }


isMaxStrength : PasswordStrength -> Bool
isMaxStrength p =
    if p.hasUppercase && p.hasLowercase && (p.numDigits > 1) then
        True

    else
        False


updatePasswordStrength : Char -> PasswordStrength -> PasswordStrength
updatePasswordStrength c p =
    if Char.isUpper c then
        { p | hasUppercase = True || p.hasUppercase }

    else if Char.isLower c then
        { p | hasLowercase = True || p.hasLowercase }

    else if Char.isDigit c then
        { p | numDigits = p.numDigits + 1 }

    else
        p


isStrongPassword : String -> Bool
isStrongPassword s =
    isMaxStrength (String.foldl updatePasswordStrength { hasUppercase = False, hasLowercase = False, numDigits = 0 } s)
        && (String.length s > 8)


validatePassword : Model -> Bool
validatePassword model =
    if isStrongPassword model.password then
        True

    else
        False


viewValidation : Model -> Html msg
viewValidation model =
    if model.password == model.passwordAgain then
        if validatePassword model then
            div [ style "color" "green" ] [ text "OK" ]

        else
            div [ style "color" "red" ] [ text "Password is not strong enough." ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]
