module TimeExercises exposing (..)

-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Array exposing (Array)
import Basics exposing (Never, cos, modBy, never, round, sin, toFloat, turns)
import Browser
import Cmd.Extra exposing (pure)
import Extras.Core exposing (flip)
import Html exposing (Html, button, div, h1, map)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Maybe exposing (Maybe(..))
import Platform.Sub exposing (Sub)
import Svg exposing (Svg, line, svg, text_)
import Svg.Attributes exposing (dx, dy, fontSize, height, stroke, textLength, width, x, x1, x2, y, y1, y2)
import Task
import Time exposing (Posix, Zone, millisToPosix, posixToMillis, toHour, toMinute, toSecond)
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
{- The model for the analog clock.
   `animationStartOffsetInMillis` represents the number of milliseconds relative
   to the time at which a particular second `x` has been reached the animation for
   the second hand transitioning to second `x` should be played. This value can be
   negative, positive or zero but should have absolute value less than zero.
   `animationKeyTimesInMillis` is a sorted array of values ranging from 0 to the
   duration of the animation (of the second hand's transition from one second to
   the next). This array should always have the values 0 and the duration of the
   animation. Any extra intermediate values control the smoothness with which the
   second hand moves from one second to the next. The duration of the animation
   should not exceed one thousand milliseconds.
-}


type alias AnalogClock =
    { animationKeyTimesInMillis : Array Int
    , animationStartOffsetInMillis : Int
    , animationDurationInMillis : Int
    , numKeyTimes : Int
    , baseSecondCorrectionsInMillis : Array ( Int, Int )
    }



{- The binarySearchBy function in which the function used to transform an
   arbitrary element of an array into a comparable number is simply the identity
   function (that is to say, all the elements of the array are already comparable
   numbers).
-}


binarySearch : Int -> number -> Array number -> ( Maybe Int, Int )
binarySearch =
    binarySearchBy identity



{- A binary search.
   Given a function to transform an arbitrary element of an array into a
   comparable number, the array length, a number to search for, and the input
   array, the function returns a tuple of `Maybe Int` and `Int`. The first element
   of the tuple will be a `Just` variant containing the index of the element that
   yields the  desired number if and only if such an element exists in the array.
   If the first  element of the tuple is a `Nothing` variant, the second element
   of the tuple will a) contain the greatest index containing an element that
   yields an number less than the desired number, b) contain 0 if  the desired
   number is less than the smallest number yielded by any element in the array or
   c) contain the maximum index  of the array if the desired number is greater
   than the greatest number yielded by any element in the array.
   The implementation is adapted from the F# code in the Japanese binary search
   Wikipedia article:
   <https://ja.wikipedia.org/wiki/%E4%BA%8C%E5%88%86%E6%8E%A2%E7%B4%A2>.
-}


binarySearchBy : (a -> number) -> Int -> number -> Array a -> ( Maybe Int, Int )
binarySearchBy f len value array =
    let
        binarySearchHelper minIndex maxIndex =
            if maxIndex < minIndex then
                ( Nothing, clamp 0 (len - 1) maxIndex )

            else
                let
                    c =
                        minIndex + (maxIndex - minIndex) // 2

                    arrayAtC =
                        Array.get c array
                            |> (\x ->
                                    case x of
                                        Just n ->
                                            f n

                                        Nothing ->
                                            0
                               )
                in
                if arrayAtC > value then
                    binarySearchHelper minIndex (c - 1)

                else if arrayAtC < value then
                    binarySearchHelper (c + 1) maxIndex

                else
                    ( Just c, 0 )
    in
    binarySearchHelper 0 (len - 1)



{- The default number of milliseconds relative to the beginning of a new second at
   which the animation of the second hand's transition to that new second should
   start.
-}


defaultAnimationOffsetInMillis : Int
defaultAnimationOffsetInMillis =
    -625



{- The first value in the returned tuple are the default key times describing the
   animation of the second hand from one second to the next. The spacing of the
   key times controls the smoothness of the  overall animation. The largest key
   time should be no larger than one thousand  milliseconds. The second value in
   the returned tuple is the length of the array of key values.
-}


defaultAnimationKeyTimes : ( Array Int, Int )
defaultAnimationKeyTimes =
    ( List.range 0 25
        |> List.map (\n -> n ^ 2)
        |> List.map ((-) 625)
        |> List.sort
        |> Array.fromList
    , 26
    )



{- Given the current time in milliseconds and the start offset of the animation
   in milliseconds, the function returns the amount of time that has passed in
   milliseconds for the currently active second hand animation. For example, if
   the current time in milliseconds mod 1000 were equal to 500 and the start
   offset of the second hand animation were 300 milliseconds, the time elapsed
   for the currently active second hand animation would be 200 milliseconds.
   Similarly, if the current time in milliseconds mod 1000 were equal to 700
   and the start offset of the second hand animation were -200 milliseconds,
   the time elapsed for the currently active second hand animation would be 900
   milliseconds.
-}


getTimeWithinAnimation : Int -> Int -> Int
getTimeWithinAnimation timeInMillis animationStartOffsetInMillis =
    let
        midSecondTimeInMillis =
            modBy 1000 timeInMillis
    in
    modBy 1000 (midSecondTimeInMillis - animationStartOffsetInMillis)



{- Given the current time in milliseconds, the start offset of the second hand
   a nimation, the number of key times for the second hand animation, and an
   array of key times, the function returns the index of the largest key time
   that is less than the current time elapsed in the second hand animation.
-}


getAnimationKeyTimeIndexHelper : Int -> Int -> Int -> Array Int -> ( Maybe Int, Int )
getAnimationKeyTimeIndexHelper timeInMillis animationStartOffsetInMillis numKeyTimes keyTimes =
    binarySearch numKeyTimes (getTimeWithinAnimation timeInMillis animationStartOffsetInMillis) keyTimes



{- Given a model,  the function returns the index (into the array of animation
   key times stored by the model) of the largest key time that is less than the
   current time stored by the model.
-}


getAnimationKeyTimeIndex : Model -> ( Maybe Int, Int )
getAnimationKeyTimeIndex model =
    getAnimationKeyTimeIndexHelper (posixToMillis model.time) model.analogClock.animationStartOffsetInMillis (second defaultAnimationKeyTimes) (first defaultAnimationKeyTimes)



{- Given the start offset of the second hand animation and an array of animation
   key times, the function returns an array of tuples that maps each key time to
   an offset that must be applied to the current time in milliseconds mod 1000
   in order to obtain the current base second of the currently active second
   hand animation. For example, a specific tuple (500, -1000) would indicate that
   on the 500th millisecond of the active second hand animation, one would have
   to calculate (the current time in milliseconds mod 60000) - 1000 in order to
   determine the current second hand position in terms of milliseconds.
-}


getBaseSecondCorrectionsInMillis : Int -> Array Int -> Array ( Int, Int )
getBaseSecondCorrectionsInMillis animationStartOffset animationKeyTimes =
    let
        -- A helper function to generate the time corrections given an array of already calculated corrections,
        -- the designated start offset of the animation in milliseconds, the time in milliseconds associated with
        -- the previously processed animation key time, the currently active time correction in milliseconds and
        -- the remaining worklist of animation key times.
        generateCorrections timeCorrectionsInMillis startOffsetInMillis lastAnimationKeyTime currentTimeCorrectionInMillis remainingAnimationKeyTimes =
            case remainingAnimationKeyTimes of
                -- If there are no more key times to process, return the calculated list of corrections.
                [] ->
                    timeCorrectionsInMillis

                x :: xs ->
                    let
                        -- Calculate the absolute time in milliseconds (modulo 1000ms) of the key time
                        -- currently being processed.
                        nonZeroCurrentTimeInMillis =
                            modBy 1000 (startOffsetInMillis + x)

                        -- Calculate the absolute time in milliseconds (modulo 1000ms) of the previously
                        -- processed animation key time.
                        nonZeroLastAnimationTime =
                            modBy 1000 (startOffsetInMillis + lastAnimationKeyTime)
                    in
                    let
                        -- If the absolute time associated with the current key time being processed
                        -- loops around the 1000ms mark, subtract an additional 1000ms to the currently
                        -- applied base second time correction.
                        newTimeCorrectionInMillis =
                            if nonZeroCurrentTimeInMillis < nonZeroLastAnimationTime then
                                currentTimeCorrectionInMillis - 1000

                            else
                                currentTimeCorrectionInMillis
                    in
                    generateCorrections (Array.push ( x, newTimeCorrectionInMillis ) timeCorrectionsInMillis) startOffsetInMillis x newTimeCorrectionInMillis xs

        -- If the animation for reaching second x begins after second x has been reached,
        -- set the initial base second time correction to be -1000ms.
        initialCorrection =
            if animationStartOffset <= 0 then
                0

            else
                -1000
    in
    generateCorrections (Array.fromList [ ( 0, initialCorrection ) ])
        animationStartOffset
        0
        initialCorrection
        (animationKeyTimes
            |> Array.toList
            -- Sort the list of animation key times in case they are
            -- passed in out of order.
            |> List.sort
            -- Ensure that the animation key time corresponding to time
            -- zero of the animation is dropped from the worklist of
            -- key times passed to the `generateCorrections` method.
            |> (\x ->
                    case List.tail x of
                        Just l ->
                            l

                        Nothing ->
                            []
               )
        )


type alias Model =
    { zone : Zone
    , time : Posix
    , timeState : TimeState
    , timeDisplayState : TimeDisplayState
    , subscriptions : List (Sub Msg)
    , analogClock : AnalogClock
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        keyTimesArrayandLength =
            defaultAnimationKeyTimes

        keyTimes =
            first keyTimesArrayandLength

        numKeyTimes =
            second keyTimesArrayandLength

        animationDuration =
            Array.get (numKeyTimes - 1) keyTimes |> Maybe.withDefault 0

        baseSecondCorrections =
            getBaseSecondCorrectionsInMillis defaultAnimationOffsetInMillis keyTimes
    in
    -- Set the display state to be undisplayable initially so that an incorrect time is not
    -- displayed upon page load.
    ( Model Time.utc (millisToPosix 1000) Running Undisplayable [ timeSub ] (AnalogClock keyTimes defaultAnimationOffsetInMillis animationDuration numKeyTimes baseSecondCorrections)
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
            pure { model | timeState = Paused, subscriptions = [] }

        Paused ->
            pure { model | timeState = Running, subscriptions = [ timeSub ] }


type Msg
    = Tick Posix
    | AdjustTimeZone Zone
    | ToggleTime


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



-- SUBSCRIPTIONS


timeSub : Sub Msg
timeSub =
    Time.every 10 Tick


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


{- Applies a function `f` to turn a time zone and Posix time into an integer on the
   model's zone and time attributes.
-}


getTimeAttribute : (Zone -> Posix -> Int) -> Model -> Int
getTimeAttribute f model =
    f model.zone model.time


{- Returns a HH:MM:SS formatted string representing the time currently stored in
   the model.
-}


getTime : Model -> String
getTime model =
    List.map (flip getTimeAttribute model) [ toHour, toMinute, toSecond ]
        |> List.map String.fromInt
        |> List.map (String.padLeft 2 '0')
        |> String.join ":"


{- Returns an HTML element containing the current time formatted as a HH:MM:SS
   style string.
-}


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
            [ Html.text (getTime model) ]
        ]


{- Returns an SVG line given the desired length of the line as a percentage of the 
   enclosing container along any dimension (note that this implies that the contain
   er should be square), the angle of the line in radians, the starting x position 
   of the line as a percentage (as mentioned before), the starting y position of
   the line as a percentage (as mentioned before), and the model.
-}


viewLine : Float -> Float -> Float -> Float -> Model -> Svg Never
viewLine lengthPercentage angleInRadians x1Percentage y1Percentage model =
    let
        x2Percentage =
            x1Percentage + lengthPercentage * cos angleInRadians

        y2Percentage =
            y1Percentage - lengthPercentage * sin angleInRadians

        x1Str =
            x1Percentage |> String.fromFloat |> flip String.append "%"

        y1Str =
            y1Percentage |> String.fromFloat |> flip String.append "%"

        x2Str =
            x2Percentage |> String.fromFloat |> flip String.append "%"

        y2Str =
            y2Percentage |> String.fromFloat |> flip String.append "%"
    in
    line [ x1 x1Str, y1 y1Str, x2 x2Str, y2 y2Str, stroke "red" ] []


analogClockCenterXCoordinate : Float
analogClockCenterXCoordinate =
    50


analogClockCenterYCoordinate : Float
analogClockCenterYCoordinate =
    50


{- A function to convert a position in an arbitrary set of units,
   measured in terms of counter-clockwise angle from the positive
   x-axis, into a clock position, which measures in radians the
   clockwise angle from the positive y-axis, where `position`
   denotes the angle to be converted and `numTicksInFullTurn`
   represents the number of units (in terms of the measure used
   for `position` needed to constitute full turn of the clock
   face).
-}


convertToClockPosition : Float -> Float -> Float
convertToClockPosition position numTicksInFullTurn =
    position |> (*) -1.0 |> flip (/) numTicksInFullTurn |> (+) .25 |> turns


{- A function to display the clock indices at hours 3, 6, 9 and 12.
-}


viewMajorClockIndices : Model -> List (Svg Never)
viewMajorClockIndices model =
    let
        majorIndexLengthPercentage =
            7
    in
    List.range 0 11
        |> List.map toFloat
        |> List.map
            (\i ->
                viewLine
                    majorIndexLengthPercentage
                    -- See `viewAnalogClockMinuteHand` for a more detailed
                    -- explanation of the purpose of these calculations.
                    (i |> (*) -1 |> flip (/) 12 |> (+) (1 / 4) |> turns)
                    (50
                        + (50 - majorIndexLengthPercentage)
                        * (((-i / 12) + (1 / 4)) |> turns |> cos)
                    )
                    -- Multiply the result of sin by -1 since the y
                    -- direction is flipped when working with SVGs as
                    -- opposed to mathematical quadrants.
                    (50
                        + (50 - majorIndexLengthPercentage)
                        * (((-i / 12) + (1 / 4)) |> turns |> sin |> (*) -1)
                    )
                    model
            )


{- A function to display the clock indices at hours 1, 2, 4, 5, 7, 8, 10 and 11.
-}


viewMinorClockIndices : Model -> List (Svg Never)
viewMinorClockIndices model =
    let
        minorIndexLengthPercentage =
            3
    in
    List.range 0 59
        |> List.filter (\i -> modBy 5 i /= 0)
        |> List.map toFloat
        |> List.map
            (\i ->
                viewLine
                    minorIndexLengthPercentage
                    (i |> (*) -1 |> (+) 15 |> flip (/) 60 |> turns)
                    -- See `viewAnalogClockMinuteHand` for a more detailed
                    -- explanation of the purpose of these calculations.
                    (50
                        + (50 - minorIndexLengthPercentage)
                        * (((-i / 60) + (1 / 4)) |> turns |> cos)
                    )
                    -- Multiply the result of sin by -1 since the y
                    -- direction is flipped when working with SVGs as
                    -- opposed to mathematical quadrants.
                    (50
                        + (50 - minorIndexLengthPercentage)
                        * (((-i / 60) + (1 / 4)) |> turns |> sin |> (*) -1)
                    )
                    model
            )


{- A function to display the analog clock hour hand.
-}


viewAnalogClockHourHand : Model -> List (Svg Never)
viewAnalogClockHourHand model =
    let
        clock =
            model.analogClock

        hourHandLengthPercentage =
            15

        timeInMillis =
            toHour model.zone model.time * 60 * 60 * 1000 + toMinute model.zone model.time * 60 * 1000

        x1Percentage =
            50

        y1Percentage =
            50

        hourHandPosition =
            -- There are 43,200,000 milliseconds in an interval of 12 hours.
            -- There are 10,800,000 milliseconds in an interval of 4 hours.
            -- See `viewAnalogClockMinuteHand` for a more detailed
            -- explanation of the purpose of these calculations.
            timeInMillis |> modBy 43200000 |> toFloat |> (*) -1 |> (+) 10800000 |> flip (/) 43200000 |> turns
    in
    [ viewLine hourHandLengthPercentage hourHandPosition x1Percentage y1Percentage model ]


{- A function to display the analog clock minute hand.
-}


viewAnalogClockMinuteHand : Model -> List (Svg Never)
viewAnalogClockMinuteHand model =
    let
        clock =
            model.analogClock

        minuteHandLengthPercentage =
            30

        timeInMillis =
            toMinute model.zone model.time * 60 * 1000 + toSecond model.zone model.time * 1000

        x1Percentage =
            50

        y1Percentage =
            50

        minuteHandPosition =
            -- 1/4 of an hour is 900,000 milliseconds.
            -- There are 3,600,000 milliseconds in an hour.
            -- Multiply the time in milliseconds by -1.
            -- Add 1/4 of an hour.
            -- Divide by the duration of an hour.
            -- Convert to radians.
            -- Note: these calculations are necessary because radians are calculated
            -- by moving counter-clockwise around the unit circle starting at the
            -- position x = 1, y = 0.
            timeInMillis |> toFloat |> (*) -1 |> (+) 900000 |> flip (/) 3600000 |> turns
    in
    [ viewLine minuteHandLengthPercentage minuteHandPosition x1Percentage y1Percentage model ]


{- A function to display the analog clock second hand.
-}


viewAnalogClockSecondHand : Model -> List (Svg Never)
viewAnalogClockSecondHand model =
    let
        clock =
            model.analogClock

        x1Percentage =
            50

        y1Percentage =
            50

        animationDurationInMillis =
            clock.animationDurationInMillis

        -- Get the index of the largest applicable animation key
        -- time based on the current time.
        animationKeyTimeIndex =
            case getAnimationKeyTimeIndex model of
                ( Just x, _ ) ->
                    x

                ( _, x ) ->
                    x

        animationCompletionPercentage =
            Array.get animationKeyTimeIndex clock.animationKeyTimesInMillis |> Maybe.withDefault 0 |> toFloat |> flip (/) (toFloat animationDurationInMillis)

        timeInMillis =
            posixToMillis model.time

        midSecondTimeInMillis =
            modBy 1000 timeInMillis

        -- Calculate the correction that must be applied to the current value of
        -- the time in seconds based on the amount of time that has passed since
        -- the beginning of the latest second animation.
        baseSecondCorrectionIndex =
            case binarySearchBy first clock.numKeyTimes (modBy 1000 (midSecondTimeInMillis - clock.animationStartOffsetInMillis)) clock.baseSecondCorrectionsInMillis of
                ( Just x, _ ) ->
                    x

                ( _, x ) ->
                    x

        -- Retrieve the base second correction (as value that is a multiple of 1000ms)
        -- that must be applied to the current time.
        baseSecondCorrectionInMillis =
            Array.get baseSecondCorrectionIndex clock.baseSecondCorrectionsInMillis |> Maybe.withDefault ( 0, 0 ) |> second

        -- Calculate the position of the second hand using the current time and the
        -- calculated base second correction value.
        -- See `viewAnalogClockMinuteHand` for a more detailed
        -- explanation of the purpose of these calculations.
        secondHandPosition =
            timeInMillis - midSecondTimeInMillis |> toFloat |> (+) (animationCompletionPercentage * 1000) |> (+) (toFloat baseSecondCorrectionInMillis) |> (*) -1 |> (+) 15000 |> flip (/) 60000 |> turns

        secondHandLengthPercentage =
            40
    in
    [ viewLine secondHandLengthPercentage secondHandPosition x1Percentage y1Percentage model ]



{- A function to display the text corresponding to each hour designation from
   1 through 12.
-}


viewAnalogClockHourFaces : Model -> List (Svg Never)
viewAnalogClockHourFaces model =
    let
        distanceFromCenter =
            35

        -- Apply a small correction to the x position of the
        -- hour faces based on their numerical value. Note
        -- that the hour faces with 2 digits require a larger
        -- correction.
        getXOffset i =
            if i >= 10 && i <= 12 then
                -4

            else
                -2.4

        getYOffset =
            always 1

        -- See `viewAnalogClockMinuteHand` for a more detailed
        -- explanation of the purpose of these calculations.
        getPos i trigFunc offset centerCoordinate =
            i
                |> toFloat
                |> (*) -1
                |> (+) 3
                |> flip (/) 12
                |> turns
                |> trigFunc
                |> (*) distanceFromCenter
                |> (+) centerCoordinate
                |> (+) (offset i)
                |> String.fromFloat
                |> flip String.append "%"

        getXPos i =
            getPos i cos getXOffset analogClockCenterXCoordinate

        getYPos i =
            getPos i (\a -> -(sin a)) getYOffset analogClockCenterYCoordinate
    in
    List.range 1 12
        |> List.map (\i -> text_ [ fontSize "3vh", dx "1", dy "1", x (getXPos i), y (getYPos i) ] [ Svg.text (String.fromInt i) ])


{- Displays the analog clock. -}


viewAnalogClock : Model -> Html Never
viewAnalogClock model =
    svg [ width "50vh", height "50vh" ]
        (viewAnalogClockSecondHand model
            |> List.append (viewAnalogClockMinuteHand model)
            |> List.append (viewAnalogClockHourHand model)
            |> List.append (viewMinorClockIndices model)
            |> List.append (viewMajorClockIndices model)
            |> List.append (viewAnalogClockHourFaces model)
        )


view model =
    if model.timeDisplayState == Displayable then
        div
            [ style "display" "flex"
            , style "height" "100vh"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "justify-content" "space-evenly"
            ]
            [ lazy viewDigitalClock model |> Html.map never
            , button [ onClick ToggleTime ] [ Html.text (getTimeToggleText model) ]
            , lazy viewAnalogClock model |> Html.map never
            ]

    else
        div [] []
