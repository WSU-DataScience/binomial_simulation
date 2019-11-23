module Layout exposing (..)



import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Form as Form
import DataEntry exposing (..)

mainGrid singleObs collectButtons pValue spinner spinButton sample debug  distPlotVisibility =
    div [] [  Grid.container []
                [CDN.stylesheet -- creates an inline style node 
                , Grid.row []
                    [ Grid.col  [ Col.md4] 
                                [ singleObs ]
                    , Grid.col [ Col.md3]
                               [ collectButtons ]
                    , Grid.col [ Col.md5]
                               [ pValue ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.md4]
                               [div [] 
                                    [ spinner
                                    , spinButton
                                    , Html.br [][]
                                    , sample
                                    ]
                                ]
                    , Grid.col [ Col.md8] 
                               [ 
                                   case distPlotVisibility of
                                    Shown ->
                                        div [id "distPlot" ] []

                                    _ ->
                                        div [] []

                               ]
                    ]
                , Grid.row []
                    [ Grid.col  [ Col.md4] 
                                [ ]
                    , Grid.col [ Col.md8]
                               [ debug ]
                    ]
                ]
              ]

singleObsGrid successInput failureInput pInput nInput =
    h4 [] [ Html.text "Simulation Setup"
          , Html.br [] []
          , Form.group []
              [
                Grid.row []
                  [ Grid.col  [ Col.xs7 ]
                              [ successInput ]
                  , Grid.col [ Col.xs5 ]
                             [ pInput ]
                  ]
              , Grid.row []
                  [ Grid.col [ Col.xs7 ]
                      [ failureInput ]
                  , Grid.col [ Col.xs5 ]
                      [ nInput ]
                  ]
              ]
          ]

exampleSingleObservation =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      , n = 20
      , handLocation = 0.725
      , currentOutcome = "Incorrect"
      , visability = Shown
      }
  in
    singleObsGrid 
      (Html.text ("Success: " ++ state.successLbl))
      (Html.text ("p: " ++ (String.fromFloat state.p)))
      (Html.text ("Failure: " ++ state.failureLbl))
      (Html.text ("n: " ++ (String.fromFloat state.n)))


sampleGrid statistic =
  div []
      [ Form.group []
        [ h4 [] [ Html.text "Latest Sample"]
        , Grid.row []
            [ Grid.col [ Col.xs10 ]
                      [ statistic ]
            , Grid.col [ Col.xs2 ] 
                       [ ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs12]
                       [ div [id "samplePlot"] [] ]
            ]
        ]
      ]



collectButtonGrid reset buttons count =
    div [] [ Form.group []
              [ Grid.row []
                  [ Grid.col  [ Col.xs9 ]
                              [ Html.h4 [] [Html.text "Collect Statistics"]
                              ]
                  , Grid.col  [ Col.xs3 ]
                              [ reset ]
                              
                  ]
              , Grid.row []
                  [ Grid.col  [ Col.xs10 ]
                              [ buttons ]
                              
                  ]
              , Grid.row []
                  [  Grid.col  [ Col.xs10 ]
                              [ count ]
                  ]
              ]
          ]


pValueGrid tailButtons xInput output =
    div [] [ Form.group []
              [ Grid.row []
                  [ Grid.col  [ Col.xs12 ]
                              [ Html.h4 [] [Html.text "P-Value"]
                              ]
                  ]
              , Grid.row []
                  [ Grid.col  [ Col.xs4 ]
                              [ xInput ]
                  , Grid.col  [ Col.xs8 ]
                              [ tailButtons ]
                  ]
              , Grid.row []
                  [ Grid.col  [ Col.xs12 ]
                              [ output ]
                              
                  ]
              ]
          ]



blankSingleObs = Html.text ""
blankCollectButtons = Html.text ""
blankPvalue = Html.text ""
blankSpinner = Html.text ""
blankSpinButton = Html.text ""
blankSample = Html.text ""
blankDistPlot = Html.text ""

blankMainGrid =
    mainGrid blankSingleObs blankCollectButtons blankPvalue blankSpinner blankSpinButton blankSample blankDistPlot 
