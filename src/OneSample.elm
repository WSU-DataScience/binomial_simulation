module OneSample exposing (..)
--port module OneSample exposing (..)


import Browser
import Debug
import DataEntry exposing (..)
import Binomial exposing (..)
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


type alias Model =  { n : Int
                    , p : Float
                    , sample : Sample
                    , statistic : Statistic
                    }


initModel = { n = 20
            , p = 0.25
            , sample = emptySample initSampleData
            , statistic = NotSelected 
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- messages

type Msg
  = ChangeN (NumericData Int)
  | ChangeP (NumericData Float)
  | ChangeSuccessLbl String
  | ChangeFailureLbl String
  | UpdateSample (List Float)
  | UseCount
  | UseProp


-- update
updateN : NumericData Int -> Model -> Model
updateN nData model =
  case nData.state of
    Correct ->
      { model | n = Maybe.withDefault initModel.n nData.val}

    _ ->
      model

updateP : NumericData Float -> Model -> Model
updateP pData model =
  case pData.state of
    Correct ->
      { model | p = Maybe.withDefault initModel.p pData.val}

    _ ->
      model


outcome : Float -> Float -> Int
outcome p w =
  if w < p then 1 else 0

updateSampleFromOutcome : List Float -> Model -> Sample
updateSampleFromOutcome ws model = 
  let
    outcomes =
      ws
      |> List.map (outcome model.p)
    sample = model.sample
    numSuccess =
      outcomes
      |> List.sum
    numFailures =
      (List.length outcomes) - numSuccess 
  in
    { sample | numSuccess = numSuccess
                 , numFailures = numFailures
    }

updateSampleFromNumSuccess : Int -> Model -> Model
updateSampleFromNumSuccess numSuccess model = 
  let
    sample = model.sample
    numFailures = model.n - numSuccess
    newSample = 
        { sample | numSuccess = numSuccess
                , numFailures = numFailures
        }
  in
    { model | sample = newSample }


updateSuccessLbl : String -> Sample -> Sample
updateSuccessLbl newLbl sample =
    { sample | successLbl = newLbl }


updateFailureLbl : String -> Sample -> Sample
updateFailureLbl newLbl sample =
    { sample | failureLbl = newLbl }


resetSample : Model -> Model
resetSample model =
  { model | sample = emptySample model.sample}
        

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UseCount ->
        ({model | statistic = Count}
        , Cmd.none
        )

    UseProp ->
        ({model | statistic = Proportion}
        , Cmd.none
        )
    ChangeN nData ->
      ( model
        |> updateN nData
        |> resetSample
      , Cmd.none
      )

    ChangeP pData ->
      ( model
        |> updateP pData
        |> resetSample
      , Cmd.none
      )


    ChangeSuccessLbl lbl ->
      ( { model | sample = model.sample |> updateSuccessLbl lbl}
      , Cmd.none
      )


    ChangeFailureLbl lbl ->
      ( { model | sample = model.sample |> updateFailureLbl lbl}
      , Cmd.none
      )


    UpdateSample ws ->
      ({model | sample = model |> updateSampleFromOutcome ws}
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
numSuccessesView model =
  model.sample.numSuccess 
    |> String.fromInt 
    |> makeHtmlText "Count(Success) = "

propSuccessesView model =
  model.sample.numSuccess 
    |> \n -> (toFloat n)/(toFloat model.n) 
    |> roundFloat 3
    |> String.fromFloat 
    |> makeHtmlText "Proportion(Success) = "

statisticView model =
  case model.statistic of
    Count ->
      numSuccessesView model

    Proportion ->
      propSuccessesView model

    _ ->
      makeHtmlText "" ""

maybeSampleView : Visibility -> Model -> Html msg
maybeSampleView visibility model =
  case visibility of
    Shown ->
       sampleGrid 
        (statisticView  model) 

    _ ->
      div [] []  

exampleSample = 
  sampleGrid
    (makeHtmlText "N(Success) ="  "10")


-- debug views


debugView model =
    div [] 
        [ makeHtmlText "sample: " (model.sample |> Debug.toString)
        , Html.br [][]
        , Html.br [][]
        , makeHtmlText "n: " (model.n |> Debug.toString)
        , Html.br [][]
        , Html.br [][]
        , makeHtmlText "p: " (model.p |> Debug.toString)
        , Html.br [][]
        , Html.br [][]
        ]

-- main view for debug

view : Model -> Html Msg
view model =
    mainGrid (exampleSingleObservationView) (debugView model)  blankPvalue (exampleSpinner) (maybeSampleView Shown model) blankSample blankDistPlot Hidden

