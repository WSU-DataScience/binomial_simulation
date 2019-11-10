module CollectStats exposing (..)


import Browser
import Time
import Random
import Html exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (..)
import Debug exposing (..)
import Html.Attributes exposing (..)
import DataEntry exposing (..)
import Binomial exposing (..)
import Layout exposing (..)
import SingleObservation exposing (..)
import Spinner exposing (..)
import OneSample exposing (..)
import Binomial exposing (..)
import DataEntry exposing (..)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Dropdown as Dropdown
import List.Extra exposing (..)
import VegaLite exposing (..)


-- main for debugging

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type Tail = Left | Right | Two | None

type alias Defaults = { n : Int 
                      , p : Float
                      , trimAt : Int
                      , collectNs : List Int
                      , minTrialsForPValue : Int
                      }

defaults = { n = 200
           , p = 0.25
           , trimAt = 100
           , collectNs = [1, 10, 100, 1000, 10000, 100000]
           , minTrialsForPValue = 1000
           }



type alias SquareHistogram = { vs : Int
                             , ks : Int
                             , ts : Float
                             }


type alias Model = { n : Int 
                   , p : Float
                   , trials : Int
                   , ys : Dict Int Int
                   , statistic : Statistic
                   , binomGen : (Float -> Int)
                   , pulldown : Dropdown.State
                   , buttonVisibility : Visibility
                   , tail : Tail
                   , xData : NumericData Float
                   , pValue : Maybe Float
                   , output : String
                   }

getBinomGen : Int -> Float -> (Float -> Int)
getBinomGen n p =
    let
        (min, _) = trimmedXRange defaults.trimAt n p
        ps = trimmedProbs defaults.trimAt n p
        bars = ps
                |> initSqrHist
                |> sortBars
                |> updateBars
                |> postProcBars
    in
        makeConvertToBinomial min bars




initModel = { n = defaults.n
            , p = defaults.p
            , trials = 0
            , ys = Dict.empty
            , statistic = NotSelected
            , binomGen = getBinomGen defaults.n defaults.p
            , pulldown = Dropdown.initialState
            , buttonVisibility = Hidden
            , tail = None
            , xData = initFloat
            , pValue = Nothing
            , output = ""
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- message


type Msg  = Collect Int
          | NewStatistics (List Float)
          | UpdateN (NumericData Int)
          | UpdateP (NumericData Float)
          | ChangePulldown Dropdown.State
          | UseCount
          | UseProp
          | ChangeTail Tail
          | ChangeX String


-- update helpers


resetX model =
    { model | xData = initFloat}


resetTail model =
    { model | tail = None}


inLeftTail model n =
    case model.xData.val of
        Just x ->
            (toFloat n) <= x

        Nothing ->
            False

inRightTail model n =
    case model.xData.val of
        Just x ->
            (toFloat n) >= x

        Nothing ->
            False

inTwoTail model n =
    case model.xData.val of
        Just x ->
            let
                mean = meanBinom model.n model.p
                (lower, upper) = twoTailLimits mean x
            in
                ((toFloat n) <= lower) || ((toFloat n) >= upper)

        Nothing ->
            False

inTail model =
    case model.tail of
        None ->
            \n -> False

        Left ->
            inLeftTail model 

        Right ->
            inRightTail model 

        Two ->
            inTwoTail model


addCounts : Model -> Int -> Int -> Maybe Int -> Maybe Int
addCounts model key cnt cnts =
    let
        isInTail = inTail model
    in
        case (cnts, isInTail key) of
            (Nothing, _) ->
                Nothing

            (_, False) ->
                cnts

            (Just currentTotal, True) ->
                Just (cnt + currentTotal)


startingPValue model =
    let
        canHavePValue = (model.xData.state == Correct) && (model.tail /= None)
    in
        if canHavePValue then
            Just 0
        else
            Nothing


tailCount : Model -> Maybe Int
tailCount model =
    Dict.foldl (addCounts model) (startingPValue model) model.ys

updatePValue : Model -> Model
updatePValue model =
    let
        tail = tailCount model
    in 
        case tail of
            Nothing ->
                { model | pValue = Nothing}

            Just cnt ->
                { model | pValue = Just ((toFloat cnt)/(toFloat model.trials))}

resetPValue model =
    { model | pValue = Nothing}


isXInOfBounds : Int -> Float -> Bool
isXInOfBounds n x = 
    (x >= 0) && (x <= (toFloat n))

updateXData : Int -> String -> NumericData Float -> NumericData Float 
updateXData n lbl xData = 
    updateNumeric String.toFloat (isXInOfBounds n) lbl xData

updateBinomGen model =
    { model | binomGen = getBinomGen model.n model.p }


updateN nData model =
    case nData.state of
        Correct ->
            { model | n = Maybe.withDefault 20 nData.val}

        _ ->
            model

updateP pData model =
    case pData.state of
        Correct ->
            { model | p = Maybe.withDefault 0.25 pData.val}

        _ ->
            model


resetYs model =
    { model | ys = Dict.empty
            , trials = 0
            }


updateCount : Maybe Int -> Maybe Int
updateCount maybeN =
    case maybeN of
        Just n ->
            Just (n + 1)

        Nothing ->
            Just 1


updateY : Int -> Dict Int Int -> Dict Int Int
updateY x ys =
    Dict.update x updateCount ys


updateYs : List Float -> Model -> Model
updateYs outcomes model =
    let
        newXs = outcomes
                |> List.map model.binomGen
        newYs = List.foldl updateY model.ys newXs
        newTrials = model.trials + (List.length outcomes)
    in
        { model | ys = newYs 
                , trials = newTrials
                }

updateButtonVisibility model =
    { model | buttonVisibility = Shown}

-- update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeX text ->
        ( {model | xData = model.xData |> updateXData model.n text}
            |> updatePValue
        , Cmd.none
        )

    ChangeTail tail ->
        ( { model | tail = tail }
            |> updatePValue
        , Cmd.none
        )

    Collect n ->
        ( model
        , Random.generate NewStatistics (Random.list n (Random.float 0 1))
        )


    NewStatistics ws ->
        ( model 
            |> updateYs ws
            |> updatePValue
        , Cmd.none
        )


    UpdateP pData ->
        let
            newModel =
                case pData.state of
                    Correct ->
                        model 
                            |> updateP pData
                            |> resetYs
                            |> updateBinomGen
                            |> resetPValue
                            |> resetX
                            |> resetTail

                    _ ->
                        model
        in
            ( newModel
            , Cmd.none
            )


    UpdateN nData ->
        let
            newModel =
                case nData.state of
                    Correct ->
                        model 
                            |> updateN nData
                            |> resetYs
                            |> updateBinomGen
                            |> resetPValue
                            |> resetX
                            |> resetTail

                    _ ->
                        model
        in
            ( newModel
            , Cmd.none
            )

    ChangePulldown state ->
        ({model | pulldown = state }
        , Cmd.none
        )

    UseCount ->
        ({model | statistic = Count}
            |> updateButtonVisibility
            |> resetYs
            |> resetPValue
            |> resetX
            |> resetTail
        , Cmd.none
        )

    UseProp ->
        ({model | statistic = Proportion}
            |> updateButtonVisibility
            |> resetYs
            |> resetPValue
            |> resetX
            |> resetTail
        , Cmd.none
        )

-- subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.pulldown ChangePulldown ]

-- debug views
statPulldownText state =
  case state of
    NotSelected ->
      "Select Statistic"

    Count ->
      "Count"

    Proportion ->
      "Proportion" 

pulldownView model =
  Dropdown.dropdown
      model.pulldown
      { options = [ ]
      , toggleMsg = ChangePulldown
      , toggleButton =
          Dropdown.toggle [ Button.primary, Button.small] [ Html.text (statPulldownText model.statistic) ]
      , items =
          [ Dropdown.buttonItem [ onClick UseCount ] [ Html.text "Count" ]
          , Dropdown.buttonItem [ onClick UseProp ] [ Html.text "Proportion" ]
          ]
      } 

pullOffThree s =
    let
        n = String.length s
    in
        if  n == 0 then
            Nothing
        else if  n >= 3 then
            Just ( String.right 3 s
                 , String.left (n - 3) s
                 )
        else
            Just ( s
                 , ""
                 )


stringAndAddCommas n = 
    n
    |> String.fromInt
    |> List.Extra.unfoldr pullOffThree 
    |> List.reverse 
    |> String.join "," 



totalCollectedTxt model =
    (model.trials |> stringAndAddCommas) ++ " statistics collected" |> Html.text


collectButtonText n =
    let
        zeros = n
              |> toFloat
              |> logBase 10
              |> round
    in
        if zeros < 3 then (String.fromInt n) else (String.fromInt (10^(zeros - 3))) ++ "K"
        

collectButton n =
    ButtonGroup.button [ Button.primary
                     , Button.onClick (Collect n)
                     ] 
                     [  Html.text (collectButtonText n) ]

collectButtons : List Int -> Html Msg
collectButtons ns =
    div []
        [ ButtonGroup.buttonGroup
            [ ButtonGroup.small, ButtonGroup.attrs [ style "display" "block"] ]
            (List.map collectButton ns)
        ]

collectButtonView model =
    collectButtonGrid 
        ( pulldownView model
        )
        ( if model.buttonVisibility == Shown then 
            collectButtons defaults.collectNs 
          else 
            div [] [])
        ( totalCollectedTxt model )

xEntry = entryView "" "x" 

outputView model = 
    let
        pStr = 
            case model.pValue of
                Nothing ->
                    ""

                Just p ->
                    p |> roundFloat 4 |> String.fromFloat
            
    in
        makeHtmlText "p-value = " pStr

notEnoughTrials model =
    model.trials < defaults.minTrialsForPValue 

pvalueView model =
    case notEnoughTrials model of
        True ->
            pValueGrid 
                (div [] []) 
                (div [] []) 
                ( errorView notEnoughTrials "Need at least 1000 collected statistics" model)

        False ->
            pValueGrid (pvalueButtons model) (xEntry ChangeX model.xData.state) (outputView model)

pvalueButtons model =
  ButtonGroup.radioButtonGroup []
          [ ButtonGroup.radioButton
                  (model.tail == Left)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Left ]
                  [ Html.text "Left-tail" ]
          , ButtonGroup.radioButton
                  (model.tail == Right)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Right ]
                  [ Html.text "Right-tail" ]
          , ButtonGroup.radioButton
                  (model.tail == Two)
                  [ Button.primary, Button.small, Button.onClick <| ChangeTail Two ]
                  [ Html.text "Two-tail" ]
          ]

distPlot : Model -> Spec
distPlot model = 
    let
        mean = meanBinom model.n model.p
        expr = 
            case (model.tail, model.xData.val) of
                (_, Nothing) -> "false"
                (None, _) -> "false"
                (Left, Just limit) ->  "datum.X <= " ++ (String.fromFloat limit)
                (Right, Just limit) -> "datum.X >= " ++ (String.fromFloat limit)
                (Two, Just limit)->  
                    let
                        (lower, upper) = twoTailLimits mean limit 
                    in
                        if (mean == limit) then "true" 
                        else"datum.X <= " ++ (toString lower) ++ " || " ++
                             "datum.X >= " ++ (toString upper)

        xs = 
            let
                cnts = model.ys |> Dict.keys |> List.map toFloat
            in
                case model.statistic of
                    Proportion ->
                        cnts |> List.map (\x -> x/(toFloat model.n))

                    _ ->
                        cnts

        ys = model.ys |> Dict.values |> List.map (toFloat >> (\x -> x/(toFloat model.trials)))
        d = dataFromColumns []
            << dataColumn "X" (nums xs)
            << dataColumn "P(X)" (nums ys)

        trans =
            transform
                << VegaLite.filter (fiExpr expr)
        
        encPMF = 
            encoding
                << position X [ pName "X", pMType Quantitative]
                << position Y [ pName "P(X)", pAggregate opSum, pMType Quantitative ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "P(X)", tMType Quantitative, tFormat ".3f"]
                            ]
        
        selectedEnc =
            encoding
                << position X [ pName "X", pMType Quantitative]
                << position Y [ pName "P(X)", pAggregate opSum, pMType Quantitative ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "P(X)", tMType Quantitative, tFormat ".3f"]
                            ]
                << color [ mStr "red", mLegend []]


    in
        toVegaLite
            [ VegaLite.width 800
            , VegaLite.height 600
            ,
            d []
            , layer [ asSpec [ bar [], encPMF []]
                    , asSpec  [ bar [], selectedEnc [], trans []]
                    ]
            ]



debugView : Model -> Html msg
debugView model =
    div []
        [ Html.h4 [] [Html.text "Test Stuff"]
        , makeHtmlText "p: " (model.p |> String.fromFloat)
        , br [] [] 
        , makeHtmlText "n: " (model.n |> String.fromInt)
        , br [] [] 
        , makeHtmlText "trials: " (model.trials |> String.fromInt)
        , br [] [] 
        , makeHtmlText "ys: " (model.ys |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "model: " (model |> Debug.toString)
        , br [] [] 
        ]


-- main view for debug

view : Model -> Html Msg
view model =
    mainGrid exampleSingleObservationView (collectButtonView model)  (pvalueView model) exampleSpinner blankSpinButton  exampleSample (debugView model)


