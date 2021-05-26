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
import Maybe exposing (Maybe(..))
import Random
import String exposing (fromInt)
import Svg exposing (Svg, svg)
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



{- Given an array of numbers, the following function returns a tuple containing
   the sum of all the numbers in the array and an array containing the cumulative
   sums of all numbers between 0 and i inclusive for each i, 0 <= i <= n - 1,
   where n is the length of the input array.
-}


sumAccum : Array number -> ( number, Array number )
sumAccum ary =
    foldl (\n ( accum, arr ) -> ( n + accum, (n + accum) :: arr )) ( 0, [] ) ary
        |> (\( accum, list ) -> ( accum, list |> fromList |> reverse ))



{- For each index i of the following array, the array specifies the minimum number
   of milliseconds that must pass after the i-1th flicker before a die can once
   again change its face during its "flickering" phase. In the case of index 0,
   the zeroth element of the array specifies the minimum number of seconds that
   must pass after its corresponding roll button is clicked before it can change
   its face for the first time during its flickering phase. Note that the sum of
   the numbers in this array indicates the time (in milliseconds) that the
   flickering phase of the die will last.
-}


defaultFlickerIntervals : Array TimeInMillis
defaultFlickerIntervals =
    Array.initialize 10 (always defaultTickInterval)
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 2)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 4)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 8)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 16)))
        |> flip Array.append (Array.initialize 10 (always (defaultTickInterval * 32)))



{- Each element in the following descending-order sorted array specifies a time in
   the range [0 milliseconds, duration of flickering phase in milliseconds] at
   which a die should flicker. Note that the flicker phase is initiated at the
   instant its roll button is pressed - we denote this time t as having a value
   equivalent to the total duration of the flickering phase. Time zero denotes the
   time at which the final flicker of the die face is performed.
-}


defaultFlickerCheckpoints : Array TimeInMillis
defaultFlickerCheckpoints =
    defaultFlickerIntervals
        |> sumAccum
        |> (\( acc, ary ) -> ( acc, ary |> Array.map (\n -> acc - n) ))
        |> second


defaultRollingCountdownStart : TimeInMillis
defaultRollingCountdownStart =
    sum defaultFlickerIntervals



{- A type to keep track of whether a die is in the process of being rolled or in
   the state of having already landed on a face. Note that maintaining such a
   value as part of the program state is necessary even in spite of maintaining a
   "flicker" countdown timer as otherwise the die, having a non-zero countdown
   value upon page load, would automatically be rolled. Using `RollingState` helps
   us ensure that the dice only get rolled when the user actually clicks on of the
   roll buttons.
-}


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
    [ Die
        1
        Landed
        defaultRollingCountdownStart
        defaultRollingCountdownStart
        (Random.weighted ( 10, 1 ) [ ( 20, 2 ), ( 30, 3 ), ( 10, 4 ), ( 10, 5 ), ( 20, 6 ) ])
        defaultFlickerCheckpoints
        0
    , Die
        1
        Landed
        defaultRollingCountdownStart
        defaultRollingCountdownStart
        (Random.int 1 6)
        defaultFlickerCheckpoints
        0
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



{- Given a model and a die number, the following function returns an updated model
   with the cursor (or index) into the array of key times in milliseconds of the
   specified die in the model based on the current time. The key times start at a
   number greater than zero and progress downward. If the first key time were 500
   milliseconds stored at index zero in the array of key times with the next key time
   being 400 milliseconds and the current time elapsed for the current rolling
   animation were greater than 100 milliseconds, then if the cursor value were
   previously equal to zero the function would return a new model where the die's
   cursor were incremented to one.
-}


updateCountdownHelper : Model -> Int -> ( Model, Cmd Msg )
updateCountdownHelper model n =
    let
        -- Create a function to get a new model and message given the state of a given die
        getNextStateAndCommandFromDie die =
            let
                -- Get the time in milliseconds of the amount of time left in the flicker animation for the specified die.
                currentCountdownCheckpoint =
                    Maybe.withDefault 0 (Array.get die.flickerCheckpointCursor die.flickerCheckpoints)

                -- Determine whether the cursor into the list of animation checkpoints for the die can be incremented by one.
                newFlickerCheckpointCursor =
                    if currentCountdownCheckpoint >= die.rollingCountdown then
                        die.flickerCheckpointCursor + 1

                    else
                        die.flickerCheckpointCursor

                -- Set the boolean `changeFace` to true if the flicker checkpoint cursor is deemed to be updateable.
                changeFace =
                    newFlickerCheckpointCursor
                        > die.flickerCheckpointCursor
                        && die.rollingCountdown
                        -- Make sure the animation countdown is not currently equal to zero.
                        >= 0
                        && die.rollingState
                        -- Check to make sure the die is currently rolling. Otherwise, the die will roll on page load.
                        == Rolling

                -- Update the die's animation countdown.
                newCountdown =
                    die.rollingCountdown - defaultTickInterval

                -- Calculate the new model.
                nextModel =
                    if changeFace || die.rollingCountdown >= 0 then
                        Model (Array.set n { die | rollingCountdown = newCountdown, flickerCheckpointCursor = newFlickerCheckpointCursor } model.dice)

                    else
                        Model (Array.set n { die | rollingState = Landed } model.dice)

                -- Issue a message to change the die face if `changeFace` is true.
                nextCommand =
                    if changeFace then
                        Random.generate (\f -> ChangeFace n f) die.generator

                    else
                        Cmd.none
            in
            ( nextModel, nextCommand )
    in
    -- Use the helper function to determine the new combination of model and message given the inputed model and die index.
    Array.get n model.dice
        |> Maybe.map getNextStateAndCommandFromDie
        |> Maybe.withDefault (Model Array.empty |> Cmd.Extra.pure)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Roll the die at the specified index by setting its state to rolling and resetting its rolling countdown.
        RollDie n ->
            Array.get n model.dice
                |> Maybe.andThen (\die -> Just (Cmd.Extra.pure (Model (Array.set n { die | rollingState = Rolling, rollingCountdown = die.rollingCountdownStart, flickerCheckpointCursor = 0 } model.dice))))
                |> Maybe.withDefault (Cmd.Extra.pure model)

        -- Change the die at the specified index to the given face.
        ChangeFace n a ->
            Array.get n model.dice
                |> Maybe.andThen (\die -> Just (Cmd.Extra.pure (Model (Array.set n { die | face = a } model.dice))))
                |> Maybe.withDefault (Cmd.Extra.pure model)

        -- Update the animation state of the die at index n.
        UpdateCountdown n ->
            updateCountdownHelper model n

        -- Issue messages to update the state of all dice.
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
{- Returns an SVG element displaying the face of the die at the specified index of
   the inputted dice array.
-}


viewDieAtIndex : Int -> Array Die -> Html Never
viewDieAtIndex n array =
    let
        roll =
            Array.get n array |> Maybe.andThen (\die -> Just die.face) |> Maybe.withDefault 1
    in
    svg
        [ height "20vh", width "20vh" ]
        [ fromInt roll
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
