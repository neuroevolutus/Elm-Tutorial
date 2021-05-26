module FormsExercises exposing (..)

-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Basics exposing (Never, never)
import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy, lazy4)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordConfirmation : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordConfirmation String



{- Handle updates of the name, password and password confirmation fields. -}


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordConfirmation password ->
            { model | passwordConfirmation = password }



-- VIEW


type alias PasswordData =
    { hasUppercase : Bool
    , hasLowercase : Bool
    , numDigits : Int
    , length : Int
    }



{- Returns an `Html msg` corresponding to an input field given String values for the
   input type (i.e. text vs. password), placeholder text, and value of the input
   field content as well as a function for transforming the content of the field into
   a Msg.
-}


viewInputField : String -> String -> String -> (String -> msg) -> Html msg
viewInputField t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



{- Checks whether the password attributes encapsulated in the inputted PasswordData
   value is representative of a sufficiently varied password.
-}


hasSufficientVariety : PasswordData -> Bool
hasSufficientVariety p =
    if p.hasUppercase && p.hasLowercase && (p.numDigits > 1) then
        True

    else
        False



{- Checks whether the inputted PasswordData value is representative of a sufficient
   ly long password.
-}


hasSufficientLength : PasswordData -> Bool
hasSufficientLength p =
    p.length >= 8



{- Helper function that given a input character and `PasswordData` struct
   generates a new `PasswordData` struct where the data concerning a password's
   length and overall strength are apprpropriately updated. This is used as a
   parameter to the `String.foldl` call in the `isPasswordSufficientlyStrong` in
   order to calculate the needed information on a password string in a single pass
   over the string.
-}


updatePasswordData : Char -> PasswordData -> PasswordData
updatePasswordData c p =
    let
        p_new =
            { p | length = p.length + 1 }
    in
    if Char.isUpper c then
        { p_new | hasUppercase = True || p.hasUppercase }

    else if Char.isLower c then
        { p_new | hasLowercase = True || p.hasLowercase }

    else if Char.isDigit c then
        { p_new | numDigits = p.numDigits + 1 }

    else
        p_new



{- Takes as input a Model and returns whether or not the password contained within
   the model is sufficiently strong.
-}


isPasswordSufficientlyStrong : Model -> Bool
isPasswordSufficientlyStrong model =
    let
        p =
            String.foldl updatePasswordData (PasswordData False False 0 0) model.password
    in
    hasSufficientVariety p && hasSufficientLength p



{- Display text in response to the user's input to indicate whether a change to
   the input is required, additional input is required, or whether all input is
   correct.
-}


viewValidation : Model -> Html Never
viewValidation model =
    if String.isEmpty model.password then
        -- Use Nord color palette for pleasant and friendly messaging.
        div [ style "color" "#5e81ac" ] [ text "Please enter your password." ]

    else if String.isEmpty model.passwordConfirmation then
        div [ style "color" "#8fbcbb" ] [ text "Please enter a password confirmation." ]

    else if model.password == model.passwordConfirmation then
        if isPasswordSufficientlyStrong model then
            div [ style "color" "#a3be8c" ] [ text "OK" ]

        else
            div [ style "color" "#bf616a" ] [ text "Password is not strong enough." ]

    else
        div [ style "color" "#bf616a" ] [ text "Passwords do not match!" ]


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "height" "50vh"
        , style "align-items" "center"
        , style "justify-content" "space-evenly"
        ]
        [ lazy4 viewInputField "text" "Name" model.name Name
        , lazy4 viewInputField "password" "Password" model.password Password
        , lazy4 viewInputField "password" "Re-enter Password" model.passwordConfirmation PasswordConfirmation
        , lazy viewValidation model |> Html.map never
        ]
