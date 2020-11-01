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
import Display exposing (stringAndAddCommas, changeThousandsToK)
import CountDict exposing (..)
import PValue exposing (..)
import Binomial exposing (..)
import Defaults exposing (defaults)
import Layout exposing (..)
import DistPlot exposing (..)
import SingleObservation exposing (..)
import Spinner exposing (..)
import OneSample exposing (..)
import Binomial exposing (..)
import DataEntry exposing (..)
import Button exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Dropdown as Dropdown
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


type alias Model = { n : Int 
                   , p : Float
                   , trials : Int
                   , successLbl : String
                   , ys : CountDict
                   , statistic : Statistic
                   , binomGen : (Float -> Int)
                   , buttonVisibility : Visibility
                   , tail : Tail
                   , xData : NumericData Float
                   , pValue : PValue Int
                   , output : String
                   , tailLimit : PValue Float
                   , lastCount : Maybe Int
                   }



initModel : Model
initModel = { n = defaults.n
            , p = defaults.p
            , successLbl = "Success"
            , trials = 0
            , ys = Dict.empty
            , statistic = NotSelected
            , binomGen = getBinomGen defaults.n defaults.p
            , buttonVisibility = Hidden
            , tail = None
            , xData = initFloat
            , pValue = NoPValue
            , output = ""
            , tailLimit = NoPValue
            , lastCount = Nothing
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )

-- message


type Msg  = Collect Int
          | OneNewStatistic Int
          | NewStatistics (List Float)
          | UpdateN (NumericData Int)
          | UpdateP (NumericData Float)
          | ChangeSuccessLbl (LblData)
          | UseCount
          | UseProp
          | ChangeTail Tail
          | ChangeX String
          | Reset


-- update helpers


resetX : Model -> Model
resetX model =
    { model | xData = initFloat}


resetTail : Model -> Model
resetTail model =
    { model | tail = None }


updatePValue : Model -> Model
updatePValue model =
    { model | pValue = model |> getPValue }


resetPValue : Model -> Model
resetPValue model =
    { model | pValue = NoPValue }


updateTailLimits : Model -> Model
updateTailLimits model = 
    { model | tailLimit = model |> tailLimit}
          

resetTailLimits : Model -> Model
resetTailLimits model =
    { model | tailLimit = NoPValue }


updateBinomGen : Model -> Model
updateBinomGen model =
    { model | binomGen = getBinomGen model.n model.p }


updateN : NumericData Int -> Model -> Model
updateN nData model =
    case nData.state of
        Correct ->
            { model | n = Maybe.withDefault 20 nData.val}

        _ ->
            model

updateP : NumericData Float -> Model -> Model
updateP pData model =
    case pData.state of
        Correct ->
            { model | p = Maybe.withDefault 0.25 pData.val}

        _ ->
            model

updateSuccess : LblData -> Model -> Model
updateSuccess lblData model =
    case lblData.state of
        Correct ->
            { model | successLbl = lblData.str}

        _ ->
            model

resetYs : Model -> Model
resetYs model =
    { model | ys = Dict.empty
            , trials = 0
            }


updateYs : List Float -> Model -> Model
updateYs outcomes model =
    let
        newYs = outcomes |> updateCountDict model.binomGen model.ys
        newTrials = model.trials + (List.length outcomes)
    in
        { model | ys = newYs 
                , trials = newTrials
                }

updateButtonVisibility : Model -> Model
updateButtonVisibility model =
    { model | buttonVisibility = Shown}


updateLastCount : List Float -> Model -> Model
updateLastCount ws model =
  let
    last =
      case ws of
        [] ->
          Nothing

        w :: _ ->
          w |> model.binomGen |> Just
  in
    { model | lastCount = last }



-- update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeX text ->
        ( {model | xData = model |> updateXData text}
            |> updatePValue
            |> updateTailLimits
        , Cmd.none
        )

    ChangeTail tail ->
        ( { model | tail = tail }
            |> updatePValue
            |> updateTailLimits
        , Cmd.none
        )

    Collect n ->
        ( model
        , Random.generate NewStatistics (Random.list n (Random.float 0 1))
        )


    OneNewStatistic numSuccess ->
        let
            newYs = updateY numSuccess model.ys
        in
            ( { model | ys = newYs, trials = model.trials + 1, lastCount = Just numSuccess }
            , Cmd.none
            )

    NewStatistics ws ->
        ( model 
            |> updateYs ws
            |> updateLastCount ws
            |> updatePValue
        , Cmd.none
        )

    Reset ->
        let
            newModel = model 
                        |> resetYs
                        |> resetPValue
                        |> resetTailLimits
                        |> resetX
                        |> resetTail

        in
            ( newModel
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
                            |> resetPValue
                            |> resetTailLimits
                            |> resetX
                            |> resetTail

                    _ ->
                        model
        in
            ( newModel
                |> updateBinomGen
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
                            |> resetPValue
                            |> resetTailLimits
                            |> resetX
                            |> resetTail

                    _ ->
                        model
        in
            ( newModel
                |> updateBinomGen
            , Cmd.none
            )

    ChangeSuccessLbl lblData ->
        ( model
            |> updateSuccess lblData
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
    Sub.none

-- debug views

-- resetButton : msg -> Html msg
-- resetButton onClickMsg =
--     Button.button
--         [ Button.primary
--         , Button.onClick onClickMsg
--         , Button.small
--         ]
--         [ Html.text "Reset" ]


totalCollectedTxt : Model -> Html msg
totalCollectedTxt model =
    (model.trials |> stringAndAddCommas) ++ " statistics collected" |> Html.text


-- collectButton : (Int -> msg) -> Int -> ButtonGroup.ButtonItem msg
-- collectButton toOnClick n =
--     ButtonGroup.button 
--       [ Button.primary
--       , Button.onClick (toOnClick n)
--       ] 
--       [  Html.text (changeThousandsToK n) ]

-- collectButtons : (Int -> msg) -> List Int -> Html msg
-- collectButtons toOnClick ns =
--     div []
--         [ ButtonGroup.buttonGroup
--             [ ButtonGroup.small, ButtonGroup.attrs [ style "display" "block"] ]
--             (List.map (collectButton toOnClick) ns)
--         ]

collectButtonView : Model -> Html Msg
collectButtonView model =
    collectButtonGrid 
        ( resetButton Reset)
        ( if model.buttonVisibility == Shown then 
            collectButtons Collect defaults.collectNs 
          else 
            div [] [])
        ( totalCollectedTxt model )


notEnoughTrials : Model -> Bool
notEnoughTrials model =
    model.trials < defaults.minTrialsForPValue 


    
basePValueString : Model -> String
basePValueString model =
  [ "p-value = "
  , model.pValue |> numerator
  , "/" 
  , model.trials |> stringAndAddCommas
  , "=" 
  , model.pValue |> proportion model.trials
  ] |> String.join " " --|> display


pvalueView : Model -> Html Msg
pvalueView model =
  let
        htmlGenerator isDisplayMode stringLatex =
            case isDisplayMode of
                Just True ->
                    Html.div [] [ Html.text stringLatex ]

                _ ->
                    Html.span [] [ Html.text stringLatex ]
        output = 
            if hasXError model then
              xError model
            else if notEnoughTrials model then
              errorView notEnoughTrials  ("Need at least " ++ (defaults.minTrialsForPValue |> String.fromInt) ++ " collected statistics") model
            else
              Html.div [][ Html.text (model |> basePValueString) ]--|> K.generate htmlGenerator ]
    in
        pValueGrid (pvalueButtons model) (xEntry ChangeX model) output


pValueButtonText : Tail -> String
pValueButtonText tail =
    case tail of
        Left ->
            "Left-tail"
        Right ->
            "Right-tail"
        Two ->
            "Two-tail"
        _ ->
            ""



-- pValueButton : String -> msg -> Bool -> ButtonGroup.RadioButtonItem msg
-- pValueButton txt msg toggle =
--         ButtonGroup.radioButton
--             toggle
--             [ Button.primary, Button.small, Button.onClick msg]
--             [ Html.text txt ]

pvalueButtons : Model -> Html Msg
pvalueButtons model =
    let
        tails = [Left, Right, Two]
        txts = tails |> List.map pValueButtonText
        msgs = tails |> List.map ChangeTail
        toggles = tails |> List.map (\t -> model.tail == t)
    in
        radioButtons msgs txts toggles


plotLimits  : Model -> (Float, Float)
plotLimits model =
  let
    combinedLimits = (combineLimits [ largeLimits model
                                    , xLimits model.xData model.statistic model.n
                                    , countLimits model.n model.ys
                                    ])
  in
   Maybe.withDefault (0, model.n) combinedLimits
   |> mapAll (maybeMakeProportion model.statistic model.n)

largeLimits : Model -> Maybe Limits
largeLimits model =
  let
    numSD = defaults.numSD
    mean = meanBinom model.n model.p
    sd = sdBinom model.n model.p
    low = mean - numSD*sd |> floor
    upp = mean + numSD*sd |> ceiling
    isLargeSample = model.n >= defaults.trimAt
  in
    if isLargeSample then
      Just (low, upp)
    else
      Just (0, model.n)

getLimits : Model -> Float -> (Float, Float)
getLimits model limit = 
  let
    mean = 
      case model.statistic of
        Proportion ->
          (meanBinom model.n model.p)/(toFloat model.n)

        _ ->
          meanBinom model.n model.p
        
    distToMean = Basics.abs (limit - mean)
  in
    ( mean - distToMean
    , mean + distToMean
    )
tailExpression : Float -> Model -> String
tailExpression mean model =
  case model.tailLimit of
    NoPValue ->
      "false"

    Lower l ->
      "datum.X <= " ++ (String.fromFloat l)

    Upper u ->
      "datum.X >= " ++ (String.fromFloat u)

    TwoTail l u ->
      if (mean == l) || (mean == u) then 
        "true"
      else 
        [ "datum.X <="
        , l |> String.fromFloat 
        , "|| datum.X >=" 
        , u |> String.fromFloat
        ] |> String.join " "


distPlot : Model -> Spec
distPlot model =
    let
        mean = meanBinom model.n model.p
        sd = sdBinom model.n model.p
        nFloat = toFloat model.n
        expr = model |> tailExpression mean
        isLarge = model.trials > 5000
        (xs, ys) =
          if isLarge then
            distColumns model.ys
          else
            dotColumns model.ys
        
        finalXs =
            case model.statistic of
                Proportion ->
                    xs |> List.map (\x -> x/nFloat)
                
                _ -> 
                    xs

        heights =
          if isLarge then
            ys
          else
            dotColumnHeights model.ys

        mark = if isLarge then bar else circle

        maxY = Basics.max defaults.distMinHeight (model.ys |> maxHeight |> toFloat)

        (minX, maxX) =  plotLimits model 


        d = dataFromColumns []
            << dataColumn "X" (nums finalXs)
            << dataColumn "N(X)" (nums ys)

        trans =
            transform
                << VegaLite.filter (fiExpr expr)

        encPMF =
            encoding
                << position X [ pName "X"
                              , pMType Quantitative
                              , pScale [scDomain (doNums [minX, maxX])]
                              ]
                << position Y [ pName "N(X)"
                              , pAggregate opSum
                              , pMType Quantitative
                              , pScale [scDomain (doNums [0.0, maxY])]
                              ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "N(X)", tMType Quantitative, tFormat ".0f"]
                            ]
        xAxisTitle = 
            case model.statistic of
                Count ->
                    "Count of " ++ model.successLbl

                Proportion ->
                    "Proportion of " ++ model.successLbl

                NotSelected ->
                    "X"

        selectedEnc =
            encoding
                << position X [ pName "X"
                              , pMType Quantitative
                              , pAxis [axTitle xAxisTitle]
                              ]
                << position Y [ pName "N(X)"
                              ,  pMType Quantitative 
                              , pAxis [axTitle "Frequency"]
                              ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "N(X)",  tFormat ".0f"]
                            ]
                << color [ mStr "red", mLegend []]


    in
        toVegaLite
            [ VegaLite.width defaults.distPlotWidth
            , VegaLite.height defaults.distPlotHeight
            ,
            d []
            , layer [ asSpec [ mark [], encPMF []]
                    , asSpec  [ mark [], selectedEnc [], trans []]
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
    mainGrid exampleSingleObservationView (collectButtonView model)  (pvalueView model) exampleSpinner blankSpinButton  exampleSample (debugView model) Hidden



