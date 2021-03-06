module ButtonExercises exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment Int
    | Decrement Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment n ->
            model + n

        Decrement n ->
            model - n

        Reset ->
            0



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
        [ button [ onClick (Decrement 1) ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick (Increment 1) ] [ text "+" ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "align-items" "center"
            , style "width" "10vw"
            , style "justify-content" "space-evenly"
            ]
            [ button [ onClick (Increment 10) ] [ text "+10" ]
            , button [ onClick Reset ] [ text "X" ]
            ]
        ]
