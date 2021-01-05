module RandomExercises exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

import Array exposing (Array, foldl, fromList)
import Browser
import Cmd.Extra
import Extras.Core exposing (flip)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe(..), andThen, withDefault)
import Random
import String exposing (fromFloat, fromInt)
import Svg
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
    Array.initialize 10 (always tickInterval)
        |> flip Array.append (Array.initialize 10 (always (tickInterval * 2)))
        |> flip Array.append (Array.initialize 10 (always (tickInterval * 4)))
        |> flip Array.append (Array.initialize 10 (always (tickInterval * 8)))
        |> flip Array.append (Array.initialize 10 (always (tickInterval * 16)))
        |> flip Array.append (Array.initialize 10 (always (tickInterval * 32)))


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


tickInterval : TimeInMillis
tickInterval =
    10


type RollingState = Rolling | Landed

type alias Die =
    { face : Int
    , rollingState: RollingState
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
    ( { dice = fromList dice }
    , Cmd.none
    )



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
                                    die.rollingCountdown - tickInterval
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
                            Just ( Model ( Array.set n { die | rollingState = Landed } model.dice), Cmd.none)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every tickInterval Tick



-- VIEW


dieAtIndexToFace : Int -> Array Die -> Int
dieAtIndexToFace n =
    Array.get n >> andThen (\die -> Just die.face) >> withDefault 0


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ dieAtIndexToFace 0 model.dice |> fromInt |> Svg.text ]
        , button [ onClick (RollDie 0) ] [ text "Roll" ]
        , h1 [] [ dieAtIndexToFace 1 model.dice |> fromInt |> Svg.text ]
        , button [ onClick (RollDie 1) ] [ text "Roll" ]
        ]
