module SingleObservation exposing (..)


import DataEntry exposing (..)
import Layout exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
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
                   }

-- Initialize

initFloat : NumericData Float
initFloat = {str = "", val = Nothing, state = Blank}

initLbl : LblData
initLbl = {str = "", state = Blank}

initModel = { successLbl = initLbl
            , failureLbl = initLbl
            , pData = initFloat
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- Messages

type Msg  = ChangeSuccessLbl String
          | ChangeFailureLbl String
          | ChangeP String


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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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


-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- view helpers

successEntry = entryView "Label" "Success" 

failureEntry = entryView "Label" "Failure"

pEntry = entryView "0.33" "p" 

hasLabelError model  =
    (model.successLbl.state == OtherwiseIncorrect) || (model.failureLbl.state == OtherwiseIncorrect)

labelError = errorView hasLabelError "The labels cannot be the same."

hasPError model =
    (model.pData.state == NotANumber) || (model.pData.state == OutOfBounds)

pError = errorView hasPError "p is a number between 0 and 1."

validEntry state =
    case state of
        Correct ->
            Form.validFeedback [] []

        Blank ->
            Form.validFeedback [] []

        _ ->
            Form.invalidFeedback [] [ text "Something not quite right." ]



singleObservationLayout success failure p labelErr pErr =
  Form.form []
    [ h4 [] [ Html.text "A Single Observation"]
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
            , Grid.col [ Col.xs2 ]
                []
            ]
        ]
    , labelErr 
    , pErr 
    ]

-- view entry point for main app
singleObservationView model =
    singleObservationLayout
       (successEntry ChangeSuccessLbl model.successLbl.state)
       (failureEntry ChangeFailureLbl model.failureLbl.state)
       (pEntry ChangeP model.pData.state)
       (labelError model)
       (pError model)


-- view for debug

exampleSingleObservationView =
  let
    state = 
      { successLbl = "Correct"
      , failureLbl = "Incorrect"
      , p = 0.25
      }
  in
    singleObservationLayout
        (Html.text ("Success: " ++ state.successLbl))
        (Html.text ("Failure: " ++ state.failureLbl))
        (Html.text ("p: " ++ (String.fromFloat state.p)))
        (Html.text "")
        (Html.text "")


debugView model =
    div [] 
            [ model.successLbl.str |> makeHtmlText "Success: "
            , Html.br [][]
            , model.successLbl.state |> entryStateView "Success State: "
            , Html.br [][]
            , model.failureLbl.str |> makeHtmlText "Failure: "
            , Html.br [][]
            , model.failureLbl.state |> entryStateView "Failure State: "
            , Html.br [][]
            , model.pData.str |> makeHtmlText "p string: "
            , Html.br [][]
            , model.pData.state |> entryStateView "p State: "
            , Html.br [][]
            ]

-- main view for subpage debug

view : Model -> Html Msg
view model =
    mainGrid (singleObservationView model) (debugView model) blankPvalue blankSpinner blankSpinButton blankSample blankDistPlot 
