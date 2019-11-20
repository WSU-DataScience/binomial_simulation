module DataEntry exposing (..)

import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (..)
import Html.Attributes exposing (..)


type Statistic = NotSelected | Count | Proportion

type Visibility
    = Hidden
    | Shown


type EntryState
    = Blank
    | Correct
    | NotANumber
    | OutOfBounds
    | OtherwiseIncorrect


type alias NumericData a =
    { str : String
    , val : Maybe a
    , state : EntryState
    }


initInt : NumericData Int
initInt =
    { str = ""
    , val = Nothing
    , state = Blank
    }


type alias LblData =
    { str : String
    , state : EntryState
    }


initLbL : LblData
initLbL =
    { str = "", state = Blank }


type alias EntryConfig msg =
    { placeholder : String
    , label : String
    , onInput : String -> msg
    }


entryConfig : String -> String -> (String -> msg) -> EntryConfig msg
entryConfig placeholder label onInput =
    { placeholder = placeholder
    , label = label
    , onInput = onInput
    }


entryOptions : String -> Int -> (String -> msg) -> EntryState -> List (Input.Option msg)
entryOptions placeholder n msg status =
    let
        baseOptions =
            [ Input.placeholder placeholder
            , Input.onInput msg
            , Input.attrs [ Html.Attributes.tabindex n

            ]
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
    case ( input, convert input ) of
        ( "", _ ) ->
            Blank

        ( _, Nothing ) ->
            NotANumber

        ( _, Just p ) ->
            if isOutOfBounds p then 
               Correct 

           else 
               OutOfBounds


updateNumericStr : (String -> Maybe a) -> String -> NumericData a -> NumericData a
updateNumericStr convert input numbericData =
    { numbericData
        | str = input
        , val = convert input
    }


updateNumericState : (String -> Maybe a) -> (a -> Bool) -> NumericData a -> NumericData a
updateNumericState convert isOutOfBounds numbericData =
    { numbericData
        | state = numericEntryState convert isOutOfBounds numbericData.str
    }


updateNumeric : (String -> Maybe number) -> (number -> Bool) -> String-> NumericData number -> NumericData number
updateNumeric convert isOutOfBounds input numbericData =
    numbericData
        |> updateNumericStr convert input
        |> updateNumericState convert isOutOfBounds


entryView : String -> String -> Int -> (String -> msg) -> EntryState -> Html msg
entryView placeholder label n msg status =
    InputGroup.config
        (InputGroup.text (entryOptions placeholder n msg status))
        |> InputGroup.small
        |> InputGroup.predecessors
            [ InputGroup.span [] [ Html.text label ] ]
        |> InputGroup.view


errorView : (a -> Bool) -> String -> a -> Html msg
errorView hasError msg model =
    let
        isInError =
            hasError model
    in
      if isInError then
              Html.span [ Html.Attributes.style "color" "red" ] [ Html.text msg ]
      else
              Html.span [] [ Html.text "" ]


-- Data Entry for X--The limit of a p value

isXInOfBounds : Int -> Float -> Bool
isXInOfBounds n x = 
    (x >= 0) && (x <= (toFloat n))


updateXData : Int -> String -> NumericData Float -> NumericData Float 
updateXData n lbl xData = 
    updateNumeric String.toFloat (isXInOfBounds n) lbl xData


makeHtmlText : String -> String -> Html msg
makeHtmlText header str =
    Html.text (header ++ str)

xEntry = entryView "" "x" 7