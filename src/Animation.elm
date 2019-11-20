module Animation exposing (..)

import Browser
import Time
import Random
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import DataEntry exposing (..)
import Layout exposing (..)
import SingleObservation exposing (..)
import Spinner exposing (..)
import OneSample exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
--import Bootstrap.Form.Range as Range
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Utilities.Spacing as Spacing



-- main for debugging

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model



type alias AnimationConfig =  { ticksPerPause : Int
                              , ticksPerRotation : Int
                              , minRotation : Int
                              , maxRotation : Int
                              }

initAnimationConfig = { ticksPerPause = 15
                      , ticksPerRotation = 5 -- 20 to 50 is a good range for the last number
                      , minRotation = 1
                      , maxRotation = 2
                      }


type alias Positioned a = { a | location : Float }

type alias SpinsLeft a = { a | spinsLeft : List (Int, Float) }

type alias TicksLeft a = { a | ticksLeft  : Int}

type alias Moving a = { a | finalPosition : Float
                          , propPerTick : Float
                      }

type alias NotSpinningState = Positioned {}

type alias PausedState = TicksLeft (SpinsLeft (Positioned {}))

type alias SpinningState = Moving (TicksLeft (SpinsLeft (Positioned {})))

type AnimationState = NotSpinning NotSpinningState 
                    | Spinning SpinningState
                    | Paused PausedState
                    | UpdateSample PausedState
                    | UpdateSampleNoAnimation NotSpinningState
                    | UpdateDist NotSpinningState 


type SpinMode = SpinOnce | SpinNTimes

type alias Model =   { nCorrect : Bool
                     , n : Int
                     , animationOff : Bool
                     , state : AnimationState
                     , config : AnimationConfig
                     , sample : List Float
                     , splitDropState : Dropdown.State
                     }

initNotSpinningState = { location = 0.125 }
initAnimationState = NotSpinning initNotSpinningState

initN = {str = "20", val = String.toFloat "20", state = Correct }

initModel = { nCorrect = True
            , n = 20
            , animationOff = False
            , state = initAnimationState
            , config = initAnimationConfig
            , sample = []
            , splitDropState = Dropdown.initialState
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- message
type RandomResult = RandomPair (Int, Float) | RandomList (List (Int, Float))

type Msg  = Tick Time.Posix
          | Spin
          | NewAngleProb (List (Int, Float))
          | UpdateN (NumericData Int)
          | ToggleAnimation Bool
          | SplitMsg Dropdown.State

-- update helpers

getRandomPair : Model -> Random.Generator (Int, Float)
getRandomPair model =
    let
        minRot = model.config.minRotation
        maxRot = model.config.maxRotation
    in
      Random.pair (Random.int minRot maxRot) (Random.float 0 1)

getRandomList : Int -> Model -> Random.Generator (List (Int, Float))
getRandomList n model =
    let
        minRot = model.config.minRotation
        maxRot = model.config.maxRotation
    in
      Random.list n (getRandomPair model)


getRandomAngleRotations : Model -> Cmd Msg
getRandomAngleRotations model =
  Random.generate NewAngleProb (getRandomList model.n model)
 

getCurrentLocation model =
  case model.state of
    Spinning s ->
       s.location

    Paused s ->
       s.location

    NotSpinning s ->
       s.location

    UpdateSample s ->
       s.location

    UpdateSampleNoAnimation s ->
       s.location

    UpdateDist s ->
       s.location


getRemaingSpins model =
  case model.state of
    Spinning s ->
       s.spinsLeft

    Paused s ->
       s.spinsLeft

    UpdateSample s ->
       s.spinsLeft

    _ ->
       []


nextSpinningState : Int -> Float -> List (Int, Float) ->  Model -> SpinningState
nextSpinningState newRotations newOmega remainingSpins model = 
  let
    currentLocation = getCurrentLocation model
    distToTravel = newOmega - currentLocation + (toFloat newRotations)
    animationTicks = round (distToTravel * (toFloat model.config.ticksPerRotation))
  in
    { location = currentLocation
    , spinsLeft = remainingSpins
    , ticksLeft = animationTicks
    , finalPosition = newOmega
    , propPerTick = distToTravel / (toFloat animationTicks)
    }


getPosition : Model -> NotSpinningState
getPosition model =
  let
    position = getCurrentLocation model
  in
    {location = position}

updateDist : Model -> AnimationState
updateDist model =
  UpdateDist {location = model |> getCurrentLocation }

nextSpinIfNeeded : List (Int, Float) -> Model -> AnimationState
nextSpinIfNeeded outcomes model =
  case outcomes of
    (newRotations, newOmega) :: rest ->
      Spinning (nextSpinningState newRotations newOmega rest model)

    [] ->
      updateDist model


notSpinning : NotSpinningState -> AnimationState
notSpinning state =
    NotSpinning state


lastLocation : List (Int, Float) -> Float
lastLocation outcomes =
  case List.map Tuple.second outcomes of
    f :: _ ->
      f

    [] ->
      initNotSpinningState.location


updateWithoutAnimation : List (Int, Float) -> Model -> Model
updateWithoutAnimation outcomes model =
  let
    location = lastLocation outcomes
  in
    
  { model | state = (UpdateSampleNoAnimation {location = location})}
    

updateSample : AnimationConfig ->  SpinningState -> AnimationState
updateSample config s =
  UpdateSample  { location = s.finalPosition
                , spinsLeft = s.spinsLeft
                , ticksLeft = config.ticksPerPause
                }

pause : PausedState -> AnimationState
pause state =
  Paused state


full model =
  List.length model.sample >= model.n


storeOneObservation : Float -> Model -> Model
storeOneObservation outcome model = 
  let
    currentSample = if model |> full then [] else model.sample
  in
    { model | sample = outcome :: currentSample }

storeAllObservation : List Float -> Model -> Model
storeAllObservation outcomes model =
  { model | sample = outcomes}

tickCount : AnimationState -> Int
tickCount state = 
  case state of
    Spinning s ->
       s.ticksLeft

    Paused s ->
       s.ticksLeft

    _ ->
      0


decrementSpinTickCount : SpinningState -> SpinningState
decrementSpinTickCount s =
  { s | ticksLeft = s.ticksLeft - 1}


decrementPauseTickCount : PausedState -> PausedState
decrementPauseTickCount s =
  { s | ticksLeft = s.ticksLeft - 1}


shiftHand : SpinningState -> SpinningState
shiftHand state = 
  { state | location = state.location + state.propPerTick}


updateAnimationOnTick : Model -> Model
updateAnimationOnTick model =
  case (model.state, tickCount model.state) of
    (UpdateSample s, _) ->
      {model | state = pause s}

    (UpdateSampleNoAnimation s, _) ->
      {model | state = updateDist model}

    (UpdateDist s, _) ->
      {model | state = notSpinning s}

    (Paused s, 0) ->
      { model | state = nextSpinIfNeeded s.spinsLeft model}

    (Paused s, _) ->
      { model | state = Paused (decrementPauseTickCount s)}

    (Spinning s, 0) ->
      { model | state = updateSample model.config s, sample = s.finalPosition :: model.sample}

    (Spinning s, _) ->
      { model | state = Spinning (s 
                                  |> shiftHand 
                                  |> decrementSpinTickCount)}

    _ ->
      model 


-- update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SplitMsg state ->
      ({model | splitDropState = state }
      , Cmd.none
      )

    Tick _ ->
      (model 
       |> updateAnimationOnTick
      , Cmd.none
      )

    Spin -> 
      case model.state of
        NotSpinning s ->
          ( model
          , getRandomAngleRotations model
          )

        _ ->
          ( model
          , Cmd.none) -- ignore button presses when a spin/pause is in process


    NewAngleProb outcomes ->
      case model.animationOff of
        False ->
          ({ model | state = nextSpinIfNeeded outcomes model, sample = []}
          , Cmd.none
          ) 


        True ->
          ( model 
            |> updateWithoutAnimation outcomes
            |> storeAllObservation (List.map Tuple.second outcomes)
          , Cmd.none
          )

    UpdateN nData ->
      case nData.state of
        Correct ->
          let
            n = Maybe.withDefault 20 nData.val
            animationOff = n > 50
          in
            ( { model | n = n, sample = [], nCorrect = True, animationOff = animationOff}
            , Cmd.none
            )

        _ ->
          ( { model | n = 1, sample = [], nCorrect = False}
          , Cmd.none
          )


    ToggleAnimation isOff ->
      let
        nTooLarge = model.n > 50  
      in
        
      ( {model | animationOff = (isOff || nTooLarge)}
      , Cmd.none
      )


-- SUBSCRIPTIONS
    --Sub.batch
    --    [ Dropdown.subscriptions model.pulldown ChangePulldown ]


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    baseBatch = [ Dropdown.subscriptions model.splitDropState SplitMsg ]
      
  in
    
    case model.state of
      NotSpinning _ ->
        Sub.batch baseBatch


      _ -> 
        Sub.batch ((Time.every 10 Tick) :: baseBatch)

-- view helpers

spinNTimesText model =
    let
        nStr = String.fromInt model.n
    in
        ["Spin", nStr, "Times"]
        |> String.join " "


spinButtonText model =
  model.n 
  |> String.fromInt
  |> \s -> "Spin " ++ s ++ " Times"

nToLargeWarning = errorView (\model -> model.n > 50) "Animation off when n > 50" 

spinButtonGrid model  =
  Form.form []
    [ Form.group []
        [
          Grid.row []
            [ Grid.col  [ Col.xs6 ]
                        [ Button.button
                            [ Button.primary, Button.small, Button.block, Button.onClick Spin]
                            [ Html.text (spinButtonText model) ]
                        ]
            , Grid.col  [ Col.xs6 ]
                        [ Checkbox.custom 
                            [ Checkbox.id "animationChkbox"
                            , Checkbox.checked model.animationOff
                            , Checkbox.onCheck ToggleAnimation
                            , Checkbox.attrs [ class "col-2" ] 
                            ] 
                            "Animation Off" 
                        ]

            ]
        , Grid.row []
            [ Grid.col  [ Col.xs12 ]
                        [ nToLargeWarning model
                        ]
            ]
        ]
    ] 



spinButton label msg =
  Button.button
    [ Button.primary, Button.block, Button.small, Button.onClick msg]
    [ Html.text label ]


-- debug view

debugView : Model -> Html msg
debugView model =
    div []
        [ Html.h4 [] [Html.text "Test Stuff"]
        , makeHtmlText "nCorrect: " (model.nCorrect |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "n: " (model.n |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "AnimationState: " (model.animationOff |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "state: " (model.state |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "config: " (model.config |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "sample: " (model.sample |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "splitDropState: " (model.splitDropState |> Debug.toString)
        , br [] [] 
        , br [] [] 
        ]


  
getSpinnerModel : Float -> Visibility -> String -> String -> Model -> Spinner.Model
getSpinnerModel p visibility successLbl failureLbl model =
  let
    baseSpinner =   { p = p
                    , currentOutcome = ""
                    , handLocation = 0
                    , successLbl = successLbl
                    , failureLbl = failureLbl
                    , visibility = visibility
                    }
  in
    case model.state of
        NotSpinning s ->
          baseSpinner |> updateWithLocation s.location

        Spinning s ->
          baseSpinner |> updateWithLocation s.location

        Paused s ->
          baseSpinner |> updateWithLocation s.location

        UpdateSample  s ->
          baseSpinner |> updateWithLocation s.location

        UpdateSampleNoAnimation s ->
          baseSpinner |> updateWithLocation s.location

        UpdateDist s ->
          baseSpinner |> updateWithLocation s.location


-- main view for debug

view : Model -> Html Msg
view model =
    mainGrid (exampleSingleObservationView) blankCollectButtons  blankPvalue (spinnerView initSpinnerConfig (getSpinnerModel 0.25 Shown "Success" "Failure" model)) (spinButtonGrid model) (exampleSample) (debugView model) Hidden

