module Spinner exposing (..)

import DataEntry exposing (..)
import Layout exposing (..)
import SingleObservation exposing (exampleSingleObservationView)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SingleObservation exposing (Model, Msg, update, singleObservationView)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


-- local main


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

 
-- Spinner config

type alias SpinnerConfig =  { height : Int
                            , width : Int
                            , centerX : Int
                            , centerY : Int
                            , radius : Int
                            , handWidth : Int
                            , successColor : String
                            , failureColor : String
                            , handColor : String
                            , labelOffset : Int
                            , percentOffset : Int
                            }


initSpinnerConfig : SpinnerConfig
initSpinnerConfig = { height = 200
                    , width = 300
                    , centerX = 150
                    , centerY = 100
                    , radius = 60
                    , handWidth = 2
                    , successColor = "#4f77a8"
                    , failureColor = "#ea8f18"
                    , handColor = "white"
                    , labelOffset = 10
                    , percentOffset = 20
                    }


-- model



type alias Model =  { p : Float
                    , currentOutcome : String
                    , handLocation : Float
                    , successLbl : String
                    , failureLbl : String
                    , visibility : Visibility
                    }

initModel = { p = 0.25
            , currentOutcome = ""
            , handLocation  = 0.125
            , successLbl = ""
            , failureLbl = ""
            , visibility = Hidden
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- messages

type Msg = Dummy | ToSingleObservation SingleObservation.Msg

-- update helpers

updateWithLocation :  Float -> Model -> Model
updateWithLocation location model =
  let
    outcomeLbl = if location <= model.p then model.successLbl else model.failureLbl
  in
    { model | handLocation = location, currentOutcome = outcomeLbl}


--updateSingleObservation newSingleObsModel model =
--   { model | singleObservation = newSingleObsModel } 

 
--updateSpinnerVisability model = 
--      let
--        canShow = (  model.singleObservation.successLbl.state == Correct 
--                  && model.singleObservation.failureLbl.state == Correct
--                  && model.singleObservation.pData.state      == Correct
--                  )
--        visability = if canShow then Shown else Hidden
--      in
--        {model  | visability = visability}

-- update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- view helpers

type alias SpinnerComponents msg =  { config : SpinnerConfig 
                                    , state : Model
                                    , components : List (Svg msg)
                                    }

initSpinner : SpinnerConfig -> Model -> SpinnerComponents msg
initSpinner config model =
  { config = config 
  , state = model
  , components = []
  }

updateComponents : SpinnerComponents msg -> Svg msg -> SpinnerComponents msg
updateComponents currentComponents nextComponent =
  {currentComponents | components = nextComponent :: currentComponents.components}

drawCircle comps =
  let
    config = comps.config
    p = comps.state.p
  in
    (circle [ cx (config.centerX |> String.fromInt)
           , cy (config.centerY |> String.fromInt)
           , r (config.radius |> String.fromInt)
           , fill (if p < 0.5 then config.failureColor else config.successColor)
           ] 
           [])
    |> updateComponents comps

  
viewHand : SpinnerComponents msg -> SpinnerComponents msg
viewHand comps =
  let
    config = comps.config
    p = comps.state.p
    prop = comps.state.handLocation
    offsetProp = prop - p/2.0
    xf = config.centerX |> toFloat
    yf = config.centerY |> toFloat
    r = config.radius |> toFloat
    t = 2 * pi * offsetProp
    x = xf + r * cos t
    y = yf + r * sin t
  in
    (line
      [ x1 (config.centerX |> String.fromInt)
      , y1 (config.centerY |> String.fromInt)
      , x2 (String.fromFloat x)
      , y2 (String.fromFloat y)
      , stroke config.handColor
      , strokeWidth (String.fromInt config.handWidth)
      , strokeLinecap "round"
      ]
      [])
    |> updateComponents comps

moveToStart : SpinnerComponents msg -> List String
moveToStart comps =
  let
    config = comps.config
    p = comps.state.p
    x = config.centerX |> toFloat
    y = config.centerY |> toFloat
    radius = config.radius |> toFloat
    startX = x + radius * cos(-1.0 * pi * p)
    startY = y + radius * sin(-1.0 * pi * p)
  in
    (["M"] ++ (List.map String.fromFloat [startX, startY]))

    
traceArc : SpinnerComponents msg -> List String
traceArc comps =
  let
    config = comps.config
    p = comps.state.p
    x = config.centerX |> toFloat
    y = config.centerY |> toFloat
    radius = config.radius |> toFloat
    endX = x + radius * cos(pi * p)
    endY = y + radius * sin(pi * p)
  in
    (["A"] 
    ++ (List.map String.fromFloat [radius, radius])
    ++ ["0", "0", if p < 0.5 then "1" else "0"]
    ++ (List.map String.fromFloat [endX, endY]))

    
lineToCenter : SpinnerComponents msg -> List String
lineToCenter comps =
  let
    config = comps.config
  in
    ("L" :: (List.map 
              String.fromInt 
              [ config.centerX
              , config.centerY
              ]
            ))


drawSuccessWedge : SpinnerComponents msg -> SpinnerComponents msg
drawSuccessWedge comps =
  let
    config = comps.config
    p = comps.state.p
    paths = 
      moveToStart comps
      ++ traceArc comps
      ++ lineToCenter comps
      |> String.join " "
    color = if p < 0.5 then config.successColor else config.failureColor
  in
     (Svg.path [d paths, stroke color, strokeWidth "1.0", fill color] [])
     |> updateComponents comps


coloredText x_ y_ color anchor t =
    text_ 
          [ x (x_ |> String.fromInt)
          , y (y_ |> String.fromInt)
          , fill color
          , textAnchor anchor
          ] 
          [Svg.text t]

type Case = Failure | Success
         
caseDirection : Case -> Int
caseDirection side =
  case side of
    Failure ->
      -1

    Success ->
      1

caseProportion side p = 
  case side of
    Failure ->
      1 - p

    Success ->
      p

caseColor config side =
  case side of
    Failure ->
      config.failureColor

    Success ->
      config.successColor

caseJustify side =
  case side of
    Failure ->
      "end"

    Success ->
      "start"

offsetLabelX : SpinnerConfig -> Case -> Int
offsetLabelX config side = 
  config.centerX + (caseDirection side) * (config.radius + config.labelOffset)

labelText : Case -> SpinnerConfig -> String -> Svg msg
labelText side config label =
  coloredText 
    (offsetLabelX config side) 
    config.centerY 
    (caseColor config side) 
    (side |> caseJustify) 
    label

failureLabel : SpinnerComponents msg -> SpinnerComponents msg
failureLabel comps = 
  let
    config = comps.config
    label = comps.state.failureLbl
  in
    (labelText Failure config label)
    |> updateComponents comps

successLabel : SpinnerComponents msg -> SpinnerComponents msg
successLabel comps =
  let
    config = comps.config
    label = comps.state.successLbl
  in
    (labelText Success config label)
    |> updateComponents comps

    
roundPercentTo : Int -> Float -> Float
roundPercentTo digits prop =
  prop*(toFloat (10^(digits + 2))) |> round |> \n ->  (toFloat n)/10.0^(toFloat digits)


percentString : Float -> Case -> String
percentString p side = 
  let
    percentStr  = p 
                |> caseProportion side
                |> roundPercentTo 1 
                |> String.fromFloat
  in
     "(" ++ percentStr ++ "%)"


offsetPercentY config =
     config.centerY + config.percentOffset


percentText : Case -> SpinnerConfig -> Float -> Svg msg
percentText side config p =
  coloredText 
    (side |> offsetLabelX config) 
    (offsetPercentY config) 
    (side |> caseColor config)
    (side |> caseJustify) 
    (side |> percentString p)


failurePercent : SpinnerComponents msg -> SpinnerComponents msg
failurePercent comps = 
  let
    config = comps.config
    p = comps.state.p
  in
    (percentText Failure config p)
    |> updateComponents comps


successPercent : SpinnerComponents msg -> SpinnerComponents msg
successPercent comps =
  let
    config = comps.config
    p = comps.state.p
  in
    (percentText Success config p)
    |> updateComponents comps
    

outcomeHeader : SpinnerComponents msg -> SpinnerComponents msg
outcomeHeader comps =
  let
    config = comps.config
    cx = config.centerX
    cy = config.centerY
    r = config.radius
  in
    (text_ 
      [ x (cx - 5 |> String.fromInt)
      , y (cy + r + 20 |> String.fromInt)
      , textAnchor "end"
      , fill "black"
      ] 
      [ Svg.text "Outcome:"
      ]) 
    |> updateComponents comps

currentOutcomeColor : SpinnerComponents msg -> String
currentOutcomeColor comps =
  let
    config = comps.config
    current = comps.state.currentOutcome
    success = comps.state.successLbl
  in
    case (current, current == success) of
      ("", _) ->
        "black"

      (_, True) ->
        Success |> caseColor config

      (_, False) ->
        Failure |> caseColor config

outcomeText : SpinnerComponents msg -> SpinnerComponents msg
outcomeText comps =
  let
    config = comps.config
    color = comps |> currentOutcomeColor
    label = comps.state.currentOutcome
    cx = config.centerX
    cy = config.centerY
    r = config.radius
  in
    (text_ 
      [ x (cx + 5 |> String.fromInt)
      , y (cy + r + 20 |> String.fromInt)
      , textAnchor "start"
      , fill color
      ] 
      [Svg.text label
      ])
    |> updateComponents comps

-- To create a spinner pipe components into this function
spinnerViewBox : SpinnerComponents msg -> Html msg
spinnerViewBox comps =
  let
    config = comps.config
    components = comps.components
  in
   svg 
    [ [0, 0, config.width, config.height]
      |> List.map String.fromInt
      |> String.join " "
      |> viewBox 
    , config.width 
      |> String.fromInt 
      |> Svg.Attributes.width 
    , config.height 
      |> String.fromInt 
      |> Svg.Attributes.height 
    ]
    components


spinner : SpinnerConfig -> Model  -> Html msg
spinner config state =
  let
    blankSpinner = initSpinner config state
  in
    blankSpinner
    |> viewHand 
    |> drawSuccessWedge
    |> drawCircle 
    |> failureLabel 
    |> failurePercent 
    |> successLabel 
    |> successPercent 
    |> outcomeHeader 
    |> outcomeText 
    |> spinnerViewBox

spinnerView : SpinnerConfig -> Model -> Html msg
spinnerView config model =
  div []
    (case model.visibility of
      Shown -> 
        [spinner config model]
      Hidden ->
        [])

exampleSpinner =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      , handLocation = 0.725
      , currentOutcome = "Incorrect"
      , visibility = Shown
      }
  in
    spinnerView initSpinnerConfig state

exampleSingleObservation =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      , handLocation = 0.725
      , currentOutcome = "Incorrect"
      , visibility = Shown
      }
  in
    h4 [] [ Html.text "A Single Observation"
          , Html.br [] []
          , Form.group []
              [
                Grid.row []
                  [ Grid.col  [ Col.xs7 ]
                              [Html.text ("Success: " ++ state.successLbl) ]
                  , Grid.col [ Col.xs5 ]
                             [Html.text ("p: " ++ (String.fromFloat state.p)) ]
                  ]
              , Grid.row []
                  [ Grid.col [ Col.xs7 ]
                      [Html.text ("Failure: " ++ state.failureLbl) ]
                  , Grid.col [ Col.xs2 ]
                      []
                  ]
              ]
          ]

-- view

view : Model -> Html Msg
view model =
    mainGrid (exampleSingleObservationView) blankCollectButtons  blankPvalue (exampleSpinner) blankSpinButton blankSample blankDistPlot 

