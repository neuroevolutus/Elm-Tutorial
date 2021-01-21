module RandomExercises exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

import Array exposing (Array, foldl, fromList)
import Basics exposing (Never, never)
import Browser
import Cmd.Extra exposing (pure)
import Extras.Core exposing (flip)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maybe exposing (andThen, Maybe(..), withDefault)
import Random
import String exposing (fromInt)
import Svg exposing (svg, Svg)
import Svg.Attributes exposing (fontSize, height, width, x, y)
import Time
import Tuple exposing (second)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias TimeInMillis =
    Float


sum : Array number -> number
sum arr =
    foldl (+) 0 arr


reverse : Array a -> Array a
reverse arr =
    foldl (::) [] arr
        |> fromList


sumAccum : Array number -> ( number, Array number )
sumAccum ary =
    foldl (\n ( accum, arr ) -> ( n + accum, (n + accum) :: arr )) ( 0, [] ) ary
        |> (\( accum, list ) -> ( accum, list |> fromList |> reverse ))


defaultFlickerIntervals : Array TimeInMillis
defaultFlickerIntervals =
    Array.initialize 10 (always defaultTickInterval)
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 2)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 4)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 8)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 16)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 32)))


applyOnTwoThenCombine : ( a, b ) -> (a -> c) -> (b -> d) -> (c -> d -> e) -> e
applyOnTwoThenCombine ( m, n ) f g h =
    h (f m) (g n)


defaultFlickerCheckpoints : Array TimeInMillis
defaultFlickerCheckpoints =
    defaultFlickerIntervals
        |> sumAccum
        |> (\( acc, ary ) -> ( acc, ary |> Array.map (\n -> acc - n) ))
        |> second


defaultRollingCountdownStart : TimeInMillis
defaultRollingCountdownStart =
    sum defaultFlickerIntervals


type RollingState
    = Rolling
    | Landed


type alias Die =
    { face : Int
    , rollingState : RollingState
    , rollingCountdown : TimeInMillis
    , rollingCountdownStart : TimeInMillis
    , generator : Random.Generator Int
    , flickerCheckpoints : Array TimeInMillis
    , flickerCheckpointCursor : Int
    }


type alias Model =
    { dice : Array Die
    }


dice : List Die
dice =
    [ Die 1 Landed defaultRollingCountdownStart defaultRollingCountdownStart (Random.weighted ( 10, 1 ) [ ( 20, 2 ), ( 30, 3 ), ( 10, 4 ), ( 10, 5 ), ( 20, 6 ) ]) defaultFlickerCheckpoints 0
    , Die 1 Landed defaultRollingCountdownStart defaultRollingCountdownStart (Random.int 1 6) defaultFlickerCheckpoints 0
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    pure { dice = fromList dice }


-- UPDATE


type Msg
    = RollDie Int
    | ChangeFace Int Int
    | UpdateCountdown Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDie n ->
            Array.get n model.dice
                |> andThen (\die -> Just (Cmd.Extra.pure (Model (Array.set n { die | rollingState = Rolling, rollingCountdown = die.rollingCountdownStart, flickerCheckpointCursor = 0 } model.dice))))
                |> withDefault (Cmd.Extra.pure model)

        ChangeFace n a ->
            Array.get n model.dice
                |> andThen (\die -> Just (Cmd.Extra.pure (Model (Array.set n { die | face = a } model.dice))))
                |> withDefault (Cmd.Extra.pure model)

        UpdateCountdown n ->
            Array.get n model.dice
                |> andThen
                    (\die ->
                        if die.rollingCountdown >= 0 && die.rollingState == Rolling then
                            let
                                currentCountdownCheckpoint =
                                    withDefault 0 (Array.get die.flickerCheckpointCursor die.flickerCheckpoints)

                                newFlickerCheckpointCursor =
                                    if currentCountdownCheckpoint >= die.rollingCountdown then
                                        die.flickerCheckpointCursor + 1

                                    else
                                        die.flickerCheckpointCursor

                                changeFace =
                                    if newFlickerCheckpointCursor > die.flickerCheckpointCursor then
                                        True

                                    else
                                        False

                                newCountdown =
                                    die.rollingCountdown - defaultTickInterval
                            in
                            Just
                                ( Model
                                    (Array.set n { die | rollingCountdown = newCountdown, flickerCheckpointCursor = newFlickerCheckpointCursor } model.dice)
                                , if changeFace then
                                    Random.generate (\f -> ChangeFace n f) die.generator

                                  else
                                    Cmd.none
                                )

                        else
                            Just ( Model (Array.set n { die | rollingState = Landed } model.dice), Cmd.none )
                    )
                |> withDefault (Cmd.Extra.pure model)

        Tick t ->
            model.dice
                |> Array.indexedMap (\i _ -> i)
                |> Array.map (\i -> Cmd.Extra.perform (UpdateCountdown i))
                |> Cmd.batch
                << Array.toList
                |> flip Cmd.Extra.with model



-- SUBSCRIPTIONS

defaultTickInterval : TimeInMillis
defaultTickInterval =
    1


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every defaultTickInterval Tick


-- VIEW


viewDieAtIndex : Int -> Array Die -> Html Never
viewDieAtIndex n array =
    let
        roll = Array.get n array |> andThen (\die -> Just die.face) |> withDefault 1
    in
        svg
            [ height "20vh", width "20vh" ]
            [
                fromInt roll
                |> Svg.text
                |> List.singleton
                |> Svg.text_ [ fontSize "10vh", x "35%", y "80%" ]
            ]


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "height" "100vh"
        , style "align-items" "center"
        , style "justify-content" "space-evenly"
        ]
        [ viewDieAtIndex 0 model.dice |> Html.map never
        , button [ onClick (RollDie 0) ] [ Html.text "Roll" ]
        , viewDieAtIndex 1 model.dice |> Html.map never
        , button [ onClick (RollDie 1) ] [ Html.text "Roll" ]
        ]

