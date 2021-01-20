module TextFieldExercises exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, size, style, value)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , placeholderText : String
    , placeholderTextLength : Int
    }


defaultPlaceholderText : String
defaultPlaceholderText =
    "Enter the text to be reversed"


init : Model
init =
    { content = ""
    , placeholderText = defaultPlaceholderText
    , placeholderTextLength = String.length defaultPlaceholderText
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }


lengthDisplay : String -> String
lengthDisplay str =
    case String.length str of
        0 ->
            ""

        n ->
            "Text length: " ++ String.fromInt n



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "height" "50vh"
        , style "align-items" "center"
        , style "justify-content" "space-evenly"
        ]
        -- Note: To make sure that the placeholder text actually fits the width
        -- of the input box one would probably need to adjust the ::placeholder
        -- pseudo-attribute using CSS.
        [ input
            [ size model.placeholderTextLength
            , placeholder model.placeholderText
            , style "font-size" "inherit"
            , style "font-style" "inherit"
            , style "font-family" "inherit"
            , value model.content
            , onInput Change
            ]
            []
        , div [] [ text (String.reverse model.content) ]
        , div [] [ text (lengthDisplay model.content) ]
        ]
