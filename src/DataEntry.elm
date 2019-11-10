module DataEntry exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Form.InputGroup as InputGroup 
import Bootstrap.Form.Input as Input

type Visibility = Hidden | Shown

type EntryState = Blank
                | Correct
                | NotANumber
                | OutOfBounds
                | OtherwiseIncorrect

type alias NumericData a = { str : String
                           , val : Maybe a
                           , state : EntryState
                           }

initInt =  { str = ""
           , val = Nothing
           , state = Blank
           }

type alias LblData = { str : String
                     , state : EntryState
                     }

initLbL = { str = "", state = Blank}

type alias EntryConfig msg =  { placeholder : String
                              , label : String
                              , onInput : String -> msg
                              }

entryConfig : String -> String -> (String -> msg) -> EntryConfig msg
entryConfig placeholder label onInput =
  { placeholder = placeholder
  , label = label
  , onInput = onInput
  }

entryOptions : String -> String -> (String -> msg) -> EntryState -> List (Input.Option msg)
entryOptions placeholder label msg status =
  let
    baseOptions =
          [ 
            Input.placeholder placeholder
          , Input.onInput msg
          ]
      
  in
    case status of
        Blank ->
          baseOptions

        Correct -> 
          Input.success :: baseOptions

        _ ->
          Input.danger :: baseOptions

numericEntryState : (String -> Maybe a) -> (a -> Bool) -> String -> EntryState
numericEntryState convert isOutOfBounds input =
  case (input, convert input) of
              ("", _) -> 
                Blank

              (_, Nothing) ->
                NotANumber

              (_ , Just p) ->
                if (isOutOfBounds p) then Correct else OutOfBounds


updateNumericStr : (String -> Maybe a) -> String -> NumericData a -> NumericData a
updateNumericStr convert input numbericData  =
     { numbericData  | str = input
                     , val = (convert input)
     }

updateNumericState : (String -> Maybe a) -> (a -> Bool) -> NumericData a -> NumericData a
updateNumericState convert isOutOfBounds numbericData =
     { numbericData  | state = numericEntryState convert isOutOfBounds numbericData.str
     }

updateNumeric convert isOutOfBounds input numbericData =
  numbericData
  |> updateNumericStr convert input
  |> updateNumericState convert isOutOfBounds

entryView : String -> String -> (String -> msg) -> EntryState -> Html msg
entryView placeholder label msg status = 
  InputGroup.config
      ( InputGroup.text (entryOptions placeholder label msg status))
      |> InputGroup.small
      |> InputGroup.predecessors
        [ InputGroup.span [] [ Html.text label] ]
      |> InputGroup.view


errorView hasError msg model =
    let
        isInError = hasError model
    in
      case isInError of 
          True ->
              Html.span [Html.Attributes.style "color" "red"] [Html.text msg]

          False ->
              Html.span [] [Html.text ""]

-- debug views


makeHtmlText : String ->  String -> Html msg
makeHtmlText header str =
      Html.text (header ++ str)

entryStateView : String -> EntryState -> Html msg
entryStateView header state =
  case state of
    Blank ->
      "Blank" |> makeHtmlText header

    Correct ->
      "Correct" |> makeHtmlText header

    NotANumber ->
      "NotANumber" |> makeHtmlText header

    OutOfBounds ->
      "OutOfBounds" |> makeHtmlText header
    
    OtherwiseIncorrect ->
      "OtherwiseIncorrect" |> makeHtmlText header
