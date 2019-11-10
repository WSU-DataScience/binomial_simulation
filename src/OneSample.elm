module OneSample exposing (..)
--port module OneSample exposing (..)


import Browser
import Debug
import DataEntry exposing (..)
import Layout exposing (..)
import SingleObservation exposing (..)
import Spinner exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import VegaLite exposing (..)


-- main for debugging

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- model

type Statistic = NotSelected | Count | Proportion

type alias Sample = { successLbl : String
                    , failureLbl : String
                    , numSuccess : Int
                    , numFailures : Int
                    }

type alias SampleData = { p : Float
                        , successLbl : String
                        , failureLbl : String
                        , ws : List Float
                        }

initSampleData = { p = 0.25
                 , successLbl = "Success"
                 , failureLbl = "Failure"
                 , ws = []
                 }

emptySample sampleData =  
  { successLbl = sampleData.successLbl
  , failureLbl = sampleData.failureLbl
  , numSuccess = 0
  , numFailures = 0
  }

type alias SamplePlotConfig = { height : Int
                              , width : Int
                              , lblAngle : Int
                              }

samplePlotConfig =  { height = 100
                    , width = 200
                    , lblAngle = 0
                    }


type alias Model =  { nData : NumericData Int
                    , n : Int
                    , p : Float
                    , sample : Sample
                    , plotVisibility : Visibility 
                    }


initModel = { nData = initInt
            , n = 20
            , p = 0.25
            , sample = emptySample initSampleData
            , plotVisibility = Hidden
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- messages

type Msg
  = ChangeN String
  | UpdateSample SampleData


-- update

isNInOfBounds n = n > 0

nEntryState = numericEntryState String.toInt isNInOfBounds

updateNData : String -> NumericData Int -> NumericData Int 
updateNData = updateNumeric String.toInt isNInOfBounds

updateN : String -> Model -> Model
updateN input model =
  let
    nData = model.nData |> updateNData input
  in
    
  {model | nData = nData
         , n = Maybe.withDefault 20 nData.val
  }

updatePlotVisibility model =
    let
        show = model.nData.state == Correct
    in
        if show then 
            {model | plotVisibility = Shown}
        else
            {model | plotVisibility = Hidden}


outcome : Float -> Float -> Int
outcome p w =
  if w < p then 1 else 0

updateSuccessLbl : String -> { a | successLbl : String } -> { a | successLbl : String }
updateSuccessLbl lbl sample =
    { sample | successLbl = lbl}

updateFailureLbl lbl sample =
    { sample | failureLbl = lbl}

updateCounts numSuccess numFailures sample = 
    { sample | numSuccess = numSuccess
             , numFailures = numFailures
    }

updateP p model =
  { model | p = Maybe.withDefault model.p p}

updateSampleFromOutcome ws model =
  let
    outcomes = 
      ws
      |> List.map (outcome model.p)
    numSuccess =
      outcomes
      |> List.sum
    numFailures =
      (List.length outcomes) - numSuccess 
    newSample = model.sample |> updateCounts numSuccess numFailures
  in
    { model | sample = newSample }

updateSample sampleData = 
  let
    outcomes = 
      sampleData.ws
      |> List.map (outcome sampleData.p)
    numSuccess =
      outcomes
      |> List.sum
    numFailures =
      (List.length outcomes) - numSuccess 
  in
    { numSuccess = numSuccess
    , numFailures = numFailures
    , successLbl = sampleData.successLbl
    , failureLbl = sampleData.failureLbl
    }


resetSample model =
  { model | sample = emptySample model.sample}
        

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeN input ->
      let
        newModel =
           model 
            |> updateN input
            |> updatePlotVisibility
            |> resetSample
      in
        (newModel 
        -- , samplePlotCmd newModel
        , Cmd.none
        )

    UpdateSample sampleData ->
      ({model | sample = updateSample sampleData}
      --, samplePlotCmd model.sample
      , Cmd.none
      )


--samplePlotCmd model =
--      samplePlotToJS (samplePlot model.sample)

-- ports
samplePlot : Int -> Sample -> Spec
samplePlot n sample =
    let
        xs = [sample.failureLbl, sample.successLbl]
        ys = [sample.numFailures, sample.numSuccess] |> List.map toFloat

        data =
            dataFromColumns []
                << dataColumn "Outcome" (strs xs)
                << dataColumn "Frequency" (nums ys)

        enc =
            encoding
                << position X
                    [ pName "Outcome"
                    , pMType Nominal
                    ]
                << position Y [ pName "Frequency"
                              , pMType Quantitative
                              , pScale [ scDomain (doNums [ 0, (toFloat n) ]) ]
                              ]
                << color [ mName "Outcome", mMType Nominal ]
                << tooltips [ [ tName "Outcome", tMType Nominal]
                              , [ tName "Frequency", tMType Quantitative]
                              ]
        cfg =
               configure
                   << configuration
                       (coAxis
                           [ axcoGridOpacity 0.1
                           , axcoLabelAngle samplePlotConfig.lblAngle
                           ]
                       )
    in
      toVegaLite [ data []
                 , VegaLite.height samplePlotConfig.height
                 , VegaLite.width samplePlotConfig.width
                 , cfg []
                 , bar []
                 , enc []
                 ]

---- send samplePlot to vega
--port samplePlotToJS : Spec -> Cmd msg



-- subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view helpers


nEntry = entryView "20" "n" 


sampleView model =
  sampleGrid
    ( nEntry ChangeN model.nData.state )

maybeSampleView visibility model =
  case visibility of
    Shown ->
      sampleView model

    _ ->
      div [] []  

exampleSample = 
  sampleGrid
    (makeHtmlText "n: " "20")


-- debug views


debugView model =
    div [] 
        [ makeHtmlText "Plot Visability: " (model.plotVisibility |> Debug.toString)
        , Html.br [][]
        , model.nData.str |> makeHtmlText "n string: "
        , Html.br [][]
        , makeHtmlText "nData: " (model.nData |> Debug.toString)
        , Html.br [][]
        , Html.br [][]
        , makeHtmlText "n: " (model.n |> Debug.toString)
        , Html.br [][]
        , Html.br [][]
        ]

-- main view for debug

view : Model -> Html Msg
view model =
    mainGrid (exampleSingleObservationView) (debugView model)  blankPvalue (exampleSpinner) (sampleView model) blankSample blankDistPlot 

