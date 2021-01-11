module TimeExercises exposing (..)

-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Basics exposing (Never, cos, modBy, never, round, sin, toFloat, turns)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Cmd.Extra exposing (pure)
import Debug exposing (log)
import Extras.Core exposing (flip)
import Html exposing (Html, button, div, h1, map, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Iso8601 exposing (fromTime)
import List exposing (head)
import Maybe exposing (withDefault)
import Platform.Sub exposing (Sub)
import Svg exposing (Svg, animate, line, svg)
import Svg.Attributes exposing (attributeName, calcMode, dur, height, keySplines, repeatCount, stroke, to, values, width, x1, x2, y1, y2)
import Task
import Time exposing (millisToPosix, posixToMillis)
import Tuple exposing (first, second)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias AnalogClock =
    { timeOfLastAnimatedSecondInMillis : Int
    , lastAnimatedSecondDurationInMillis : Int
    , secondHandTransitionTimeInMillis : Int
    }


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , timeState : TimeState
    , timeDisplayState : TimeDisplayState
    , subscriptions : List (Sub Msg)
    , analogClock : AnalogClock
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 1000) Running Undisplayable [ redrawSub, timeSub ] (AnalogClock 0 0 500)
    , Task.perform AdjustTimeZone Time.here
    )


type TimeState
    = Running
    | Paused


type TimeDisplayState
    = Undisplayable
    | Displayable



-- UPDATE


updateTimeState : Model -> ( Model, Cmd Msg )
updateTimeState model =
    case model.timeState of
        Running ->
            pure { model | timeState = Paused, subscriptions = [ redrawSub ] }

        Paused ->
            pure { model | timeState = Running, subscriptions = [ redrawSub, timeSub ] }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleTime
    | Redraw


roundUpExpensive : Float -> Float -> Float
roundUpExpensive a b =
    let
        c =
            round a

        d =
            round b

        e =
            modBy d c

        f =
            d - e

        e2 =
            e ^ 2

        f2 =
            f ^ 2

        g =
            min e2 f2

        h =
            (g - f2) // (e2 - f2) * e

        j =
            max (1 - (e2 - f2) ^ 2) 0

        k =
            (g - e2) // (f2 - e2) * f + j * f

        m =
            c - h + k
    in
    toFloat m


updateAnalogClock : Model -> Model
updateAnalogClock model =
    let
        timeInMillis =
            posixToMillis model.time

        clock =
            model.analogClock

        midSecondTimeInMillis =
            modBy 1000 timeInMillis

        changeSecond =
            clock.timeOfLastAnimatedSecondInMillis + clock.lastAnimatedSecondDurationInMillis < timeInMillis && (midSecondTimeInMillis > clock.secondHandTransitionTimeInMillis)

        newAnimatedSecond =
            timeInMillis

        newDuration =
            1000 - midSecondTimeInMillis
    in
    if changeSecond then
        { model | analogClock = { clock | timeOfLastAnimatedSecondInMillis = newAnimatedSecond, lastAnimatedSecondDurationInMillis = newDuration } }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime, timeDisplayState = Displayable }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ToggleTime ->
            updateTimeState model

        Redraw ->
            Cmd.Extra.pure (updateAnalogClock model)



-- SUBSCRIPTIONS


timeSub : Sub Msg
timeSub =
    Time.every 50 Tick


redrawSub : Sub Msg
redrawSub =
    onAnimationFrame (always Redraw)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch model.subscriptions



-- VIEW


getTimeToggleText : Model -> String
getTimeToggleText model =
    case model.timeState of
        Paused ->
            "Resume"

        Running ->
            "Pause"


getHour : Model -> Int
getHour model =
    Time.toHour model.zone model.time


getMinute : Model -> Int
getMinute model =
    Time.toMinute model.zone model.time


getSecond : Model -> Int
getSecond model =
    Time.toSecond model.zone model.time


getTime : Model -> String
getTime model =
    List.map ((|>) model) [ getHour, getMinute, getSecond ]
        |> List.map String.fromInt
        |> String.join ":"


viewDigitalClock : Model -> Html Never
viewDigitalClock model =
    h1
        [ style "background" "black"
        , style "color" "#f0d921"
        , style "padding" ".3em .4em"
        , style "border-radius" "10% / 20%"
        ]
        [ div
            [ style "transform" "skewX(-10deg)"
            , style "text-shadow" ".05em -.08em #f0cc23"
            ]
            [ text (getTime model) ]
        ]


interleaveHelper : Bool -> List a -> List a -> List a -> List a
interleaveHelper b result list1 list2 =
    if b then
        case list1 of
            [] ->
                List.append result list2

            x :: xs ->
                interleaveHelper False (List.append result [ x ]) xs list2

    else
        case list2 of
            [] ->
                List.append result list1

            x :: xs ->
                interleaveHelper True (List.append result [ x ]) list1 xs


interleave : List a -> List a -> List a
interleave list1 list2 =
    interleaveHelper True [] list1 list2


viewAnalogClock : Model -> Html Never
viewAnalogClock model =
    let
        duration =
            10

        durStr =
            duration |> String.fromInt |> flip String.append "s"

        xPercentages =
            [ 0.5, 0.95 ] |> List.map (flip (/) duration) |> List.map ((*) (toFloat model.analogClock.lastAnimatedSecondDurationInMillis)) |> List.map (flip (/) 1000)

        yPercentages =
            [ 0.15, 0.9 ]

        keySplinesValues =
            interleave xPercentages yPercentages

        keySplinesStr =
            keySplinesValues |> List.map String.fromFloat |> String.join " "

        x1Pos =
            50

        x1PosStr =
            String.fromInt x1Pos |> flip String.append "%"

        y1Pos =
            50

        y1PosStr =
            String.fromInt y1Pos |> flip String.append "%"

        length =
            50

        endpointPositions =
            [ turns (toFloat (model.analogClock.timeOfLastAnimatedSecondInMillis |> flip (//) 1000 |> (+) 45 |> modBy 60) / 60)
            , turns (toFloat (model.analogClock.timeOfLastAnimatedSecondInMillis |> flip (//) 1000 |> (+) 45 |> (+) 1 |> modBy 60) / 60)
            ]

        endpointValues =
            endpointPositions
                |> (\ps ->
                        ( List.map cos ps, List.map sin ps )
                            |> Tuple.mapBoth (List.map ((*) length)) (List.map ((*) length))
                            |> Tuple.mapBoth (List.map ((+) x1Pos)) (List.map ((+) y1Pos))
                   )

        x2Pos =
            endpointValues |> first |> head |> withDefault 0

        y2Pos =
            endpointValues |> second |> head |> withDefault 0

        x2PosStr =
            String.fromFloat x2Pos |> flip String.append "%"

        y2PosStr =
            String.fromFloat y2Pos |> flip String.append "%"

        endpointValuesStrs =
            endpointValues
                |> Tuple.mapBoth (List.map String.fromFloat) (List.map String.fromFloat)
                |> Tuple.mapBoth (List.map (flip String.append "%")) (List.map (flip String.append "%"))
                |> Tuple.mapBoth (String.join ";") (String.join ";")

        animations =
            [ animate [ repeatCount "indefinite", attributeName "x2", dur durStr, values (first endpointValuesStrs), calcMode "spline", keySplines keySplinesStr ] [], animate [ repeatCount "indefinite", attributeName "y2", dur durStr, values (second endpointValuesStrs), calcMode "spline", keySplines keySplinesStr ] [] ]
    in
    svg [ width "50vh", height "50vh" ] [ line [ x1 x1PosStr, y1 y1PosStr, x2 x2PosStr, y2 y2PosStr, stroke "red" ] animations ]


view : Model -> Html Msg
view model =
    if model.timeDisplayState == Displayable then
        div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ lazy viewDigitalClock model |> Html.map never
            , button [ onClick ToggleTime ] [ text (getTimeToggleText model) ]
            , lazy viewAnalogClock model |> Html.map never
            ]

    else
        div [] []
