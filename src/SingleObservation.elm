module SingleObservation exposing (..)


import DataEntry exposing (..)
import Layout exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


-- This pages main

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = { successLbl : LblData
                   , failureLbl : LblData
                   , pData : NumericData Float
                   , nData : NumericData Int
                   , pulldown : Dropdown.State
                   , statistic : Statistic
                   }

-- Initialize

initFloat : NumericData Float
initFloat = {str = "", val = Nothing, state = Blank}

initLbl : LblData
initLbl = {str = "", state = Blank}

initModel = { successLbl = initLbl
            , failureLbl = initLbl
            , pData = initFloat
            , nData = initInt
            , pulldown = Dropdown.initialState
            , statistic = NotSelected
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- Messages

type Msg  = ChangeSuccessLbl String
          | ChangeFailureLbl String
          | ChangeP String
          | ChangeN String
          | ChangePulldown Dropdown.State
          | UseCount
          | UseProp


-- update functions

labelState thisLbl otherLbl =
    if thisLbl == "" then
        Blank
    else if thisLbl == otherLbl  then
        OtherwiseIncorrect
    else
        Correct

updateLabel : String -> String -> LblData ->  LblData
updateLabel thisLbl otherLbl labelData =
    {labelData | str = thisLbl, state = labelState thisLbl otherLbl}


isPInOfBounds p = (p >= 0) && (p <= 1)

updatePData : String -> NumericData Float -> NumericData Float 
updatePData = updateNumeric String.toFloat isPInOfBounds

isNInOfBounds n = n > 0

nEntryState = numericEntryState String.toInt isNInOfBounds

updateNData : String -> NumericData Int -> NumericData Int 
updateNData = updateNumeric String.toInt isNInOfBounds


updateN : String -> Model -> Model
updateN input model =
  {model | nData = model.nData |> updateNData input }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePulldown state ->
        ({model | pulldown = state }
        , Cmd.none
        )

    UseCount ->
        ({model | statistic = Count}
        , Cmd.none
        )

    UseProp ->
        ({model | statistic = Proportion}
        , Cmd.none
        )

    ChangeSuccessLbl lbl ->
        ( {model | successLbl = model.successLbl |> updateLabel lbl model.failureLbl.str}
        , Cmd.none
        )


    ChangeFailureLbl lbl ->
        ( {model | failureLbl= model.failureLbl |> updateLabel lbl model.successLbl.str}
        , Cmd.none
        )

    ChangeP text ->
        ( {model | pData = model.pData |> updatePData text}
        , Cmd.none
        )


    ChangeN text ->
        ( model
            |> updateN text
        , Cmd.none
        )


-- subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.pulldown ChangePulldown ]


validEntry state =
    case state of
        Correct ->
            Form.validFeedback [] []

        Blank ->
            Form.validFeedback [] []

        _ ->
            Form.invalidFeedback [] [ text "Something not quite right." ]


statPulldownText model =
  case model.statistic of
    NotSelected ->
      "Select"

    Count ->
      "Count"

    Proportion ->
      "Proportion" 


inputFeedback model =
  case model.statistic of
    NotSelected ->
      []

    _ ->
      [Input.success]

pulldownOutline model =
  case model.statistic of
    NotSelected ->
        Button.outlinePrimary
    
    _ ->
        Button.outlineSecondary

statPulldown model =
    InputGroup.config
        ( InputGroup.text ([ Input.placeholder (statPulldownText model), Input.disabled True] ++ inputFeedback model) )
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text "Statistic"] ]
        |> InputGroup.successors
            [InputGroup.dropdown
                model.pulldown
                { options = []
                , toggleMsg = ChangePulldown
                , toggleButton =
                    Dropdown.toggle [ pulldownOutline model, Button.small ] []
                , items =
                    [ Dropdown.buttonItem [ onClick UseCount ] [ Html.text "Count" ]
                    , Dropdown.buttonItem [ onClick UseProp ] [ Html.text "Proportion" ]
                    ]
                }
            ]

        |> InputGroup.view

singleObservationLayout success failure p n stat labelErr pErr nErr =
  Form.form []
    [ h4 [] [ Html.text "Simulation Setup"]
    , Html.br [] []
    , Form.group []
        [
          Grid.row []
            [ Grid.col  [ Col.xs7 ]
                        [ success ]
            , Grid.col [ Col.xs5 ]
                      [ p ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs7 ]
                [ failure ]
            , Grid.col [ Col.xs5 ]
                [ n ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs7 ]
                [ stat ]
            , Grid.col [ Col.xs5 ]
                [ ]
            ]
        ]
    , labelErr 
    , pErr 
    , nErr 
    ]

-- view entry point for main app
singleObservationView model =
    singleObservationLayout
       (successEntry ChangeSuccessLbl model.successLbl.state)
       (failureEntry ChangeFailureLbl model.failureLbl.state)
       (pEntry ChangeP model.pData.state)
       (nEntry ChangeN model.nData.state)
       (statPulldown model)
       (labelError model)
       (pError model)
       (nError model)


-- view for debug

exampleSingleObservationView =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      , n = 20
      , statistic = "Count"
      }
  in
    singleObservationLayout
        (Html.text ("Success: " ++ state.successLbl))
        (Html.text ("Failure: " ++ state.failureLbl))
        (Html.text ("p: " ++ (String.fromFloat state.p)))
        (Html.text ("n: " ++ (String.fromFloat state.n)))
        (Html.text ("Statistic: " ++ state.statistic))
        (Html.text "")
        (Html.text "")
        (Html.text "")


debugView model =
    div [] 
            [ model.successLbl |> Debug.toString |> makeHtmlText "Success: "
            , Html.br [][]
            , model.failureLbl |> Debug.toString |> makeHtmlText "Failure: "
            , Html.br [][]
            , model.pData |> Debug.toString |> makeHtmlText "pData: "
            , Html.br [][]
            , model.nData |> Debug.toString |> makeHtmlText "nData: "
            , Html.br [][]
            ]

-- main view for subpage debug

view : Model -> Html Msg
view model =
    mainGrid (singleObservationView model) (debugView model) blankPvalue blankSpinner blankSpinButton blankSample blankDistPlot Hidden
