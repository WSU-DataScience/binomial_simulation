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
import Katex as K exposing  ( Latex
                            , human
                            , inline
                            , display
                            )


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
                      , distMinHeight : Float
                      , numSD : Float
                      , pValDigits : Int
                      , distPlotWidth : Int
                      , distPlotHeight : Int
                      }

defaults =  { n = 200
            , p = 0.25
            , trimAt = 100
            , collectNs = [10, 100, 1000, 10000, 100000]
            , minTrialsForPValue = 100
            , distMinHeight = 100.0
            , numSD = 4.0
            , pValDigits = 4
            , distPlotWidth = 700
            , distPlotHeight = 525  
            }



type alias SquareHistogram = { vs : Int
                             , ks : Int
                             , ts : Float
                             }

type PValue a = NoPValue | Lower a | Upper a | TwoTail a a 

type alias Model = { n : Int 
                   , p : Float
                   , trials : Int
                   , successLbl : String
                   , ys : Dict Int Int
                   , statistic : Statistic
                   , binomGen : (Float -> Int)
                   , buttonVisibility : Visibility
                   , tail : Tail
                   , xData : NumericData Float
                   , pValue : PValue Int
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


resetX model =
    { model | xData = initFloat}


resetTail model =
    { model | tail = None }

tailLimit model =
  case (model.xData.val, model.tail) of
    (Nothing, _) ->
      NoPValue
    
    (_, None) ->
      NoPValue

    (Just x, Left) ->
      Lower x 

    (Just x, Right) ->
      Upper x 

    (Just x, Two) ->
      let 
        mean = meanBinom model.n model.p
        sd = sdBinom model.n model.p
        distToMean = Basics.abs (x - mean)
      in
        TwoTail (mean - distToMean) (mean + distToMean)

inLower l pair =
  let 
    (cnt, freq) = pair
    cntF = cnt |> toFloat
  in
    if cntF <= l then freq else 0

inUpper u pair =
  let 
    (cnt, freq) = pair
    cntF = cnt |> toFloat
  in
    if cntF >= u then freq else 0
   
inTail : PValue Float -> (Int, Int) -> PValue Int
inTail tailLim pair =
  case tailLim of
    NoPValue ->
      NoPValue
      
    Lower l ->
      pair |> inLower l |> Lower
    
    Upper u ->
      pair |> inUpper u |> Upper

    TwoTail l u ->
      TwoTail
        (pair |> inLower l)
        (pair |> inUpper u)
      

startingPValue tail =
    case tail of
        None ->
            NoPValue

        Left ->
            Lower 0

        Right ->
            Upper 0

        Two ->
            TwoTail 0 0


addTails : PValue Int -> PValue Int -> PValue Int
addTails pval1 pval2 =
  case (pval1, pval2) of
    (Lower f1, Lower f2) ->
      Lower (f1 + f2)

    (Upper f1, Upper f2) ->
      Upper (f1 + f2)

    (TwoTail l1 u1, TwoTail l2 u2) ->
      TwoTail (l1 + l2) (u1 + u2)

    _ ->
      NoPValue

getPValue : Model -> PValue Int
getPValue model =
  case model.xData.val of
    Nothing ->
      NoPValue

    Just x ->
      model.ys
      |> Dict.toList
      |> List.map (inTail (tailLimit model))
      |> List.foldl (addTails) (startingPValue model.tail)


updatePValue : Model -> Model
updatePValue model =
    { model | pValue = model |> getPValue }

resetPValue model =
    { model | pValue = NoPValue }


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

updateSuccess lblData model =
    case lblData.state of
        Correct ->
            { model | successLbl = lblData.str}

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


    OneNewStatistic numSuccess ->
        let
            newYs = updateY numSuccess model.ys
        in
            ( { model | ys = newYs, trials = model.trials + 1 }
            , Cmd.none
            )

    NewStatistics ws ->
        ( model 
            |> updateYs ws
            |> updatePValue
        , Cmd.none
        )

    Reset ->
        let
            newModel = model 
                        |> resetYs
                        |> resetPValue
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

resetButton =
    Button.button
        [ Button.primary
        , Button.onClick Reset
        , Button.small
        ]
        [ Html.text "Reset" ]

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
        ( resetButton)
        ( if model.buttonVisibility == Shown then 
            collectButtons defaults.collectNs 
          else 
            div [] [])
        ( totalCollectedTxt model )

xEntry = entryView "" "x" 7

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

notEnoughTrials : Model -> Bool
notEnoughTrials model =
    model.trials < defaults.minTrialsForPValue 

numerator : PValue Int -> String
numerator pval =
  case pval of
    NoPValue ->
       "??"
    
    Lower l ->
      l |> stringAndAddCommas
  
    Upper u ->
      u |> stringAndAddCommas

    TwoTail l u ->
      [ "(" ++  (l |> stringAndAddCommas)
      , "+"
      , (u |> stringAndAddCommas) ++ ")"
      ] |> String.join " "

proportion : Int -> PValue Int -> String
proportion n pval =
  let
      nF = n |> toFloat
      divideByN = \i -> (toFloat i)/nF
  in
    case pval of
      NoPValue ->
        "??"
      
      Lower l ->
        l |> divideByN |> roundFloat defaults.pValDigits |> String.fromFloat
    
      Upper u ->
        u |> divideByN |> roundFloat defaults.pValDigits |> String.fromFloat

      TwoTail l u ->
        (l + u) |> divideByN |> roundFloat defaults.pValDigits |> String.fromFloat
     
    
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
            case notEnoughTrials model of
                True ->
                    errorView notEnoughTrials  ("Need at least " ++ (defaults.minTrialsForPValue |> String.fromInt) ++ " collected statistics") model

                False ->
                  Html.div [][ Html.text (model |> basePValueString) ]--|> K.generate htmlGenerator ]
    in
        pValueGrid (pvalueButtons model) (xEntry ChangeX model.xData.state ) output

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


combineDistColumns : (Int, Int) -> (List Int, List Int) -> (List Int , List Int)
combineDistColumns pair columns =
  let
    (newX, newY) = pair
    (oldXs, oldYs) = columns
  in
    (newX :: oldXs, newY :: oldYs)


distColumns : Dict Int Int -> (List Float, List Float)
distColumns yDict =
  yDict
  |> Dict.toList
  |> List.foldl combineDistColumns ([], [])
  |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToDots : (Int, Int) -> (List Int, List Int)
countPairToDots pair =
  let
    (x, cnt) = pair
    xs = List.repeat cnt x
    ys = List.range 1 cnt
  in
    (xs, ys)


combineDotColumns : (Int, Int) -> (List Int, List Int) -> (List Int, List Int)
combineDotColumns nextPair columns =
  let
    (newXs, newYs) = countPairToDots nextPair
    (oldXs, oldYs) = columns
  in
    (newXs ++ oldXs, newYs ++ oldYs)


dotColumns : Dict Int Int -> (List Float, List Float)
dotColumns yDict =
  let
    countList = Dict.toList yDict
  in
    countList
    |> List.foldl combineDotColumns ([], [])
    |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToHeights : (Int, Int) ->  List Int
countPairToHeights pair =
  let
    (_, cnt) = pair
  in
    List.repeat cnt cnt


combineHeights : (Int, Int) -> List Int -> List Int
combineHeights nextPair heights =
    (countPairToHeights nextPair) ++ heights

dotColumnHeights : Dict Int Int -> List Float
dotColumnHeights yDict = 
    yDict
    |> Dict.toList 
    |> List.foldl combineHeights [] 
    |> List.map toFloat


updateMax : (Int, Int) -> Int -> Int
updateMax pair currentMax =
  let
    ( _ , newY) = pair
  in
    Basics.max newY currentMax

maxHeight : Dict Int Int -> Int
maxHeight yDict =
  yDict
  |> Dict.toList
  |> List.foldl updateMax 0

combineTwoLimits : (Int, Int) -> (Int, Int) -> (Int, Int)
combineTwoLimits lim1 lim2 =
  let
    (min1, max1) = lim1
    (min2, max2) = lim2
  in
    ( Basics.min min1 min2
    , Basics.max max1 max2
    )


updateLimits : Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
updateLimits next current =
  case (next, current) of
    (Nothing, Nothing) ->
      Nothing

    ( _ , Nothing) ->
      next

    (Nothing, _ ) ->
      current

    (Just p1, Just p2) ->
      combineTwoLimits p1 p2
      |> Just

      
combineLimits : List (Maybe (Int, Int)) -> Maybe (Int, Int)
combineLimits = List.foldl updateLimits Nothing

updatePairLimits : (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
updatePairLimits nextPair currentMinMax =
  let
    (x, _ ) = nextPair
    xLim = (x, x)
  in
    case currentMinMax of
      Nothing ->
        Just xLim

      Just currentLim ->
        combineTwoLimits xLim currentLim
        |> Just

pairLimits : Int -> List (Int, Int) -> Maybe (Int, Int)
pairLimits n = List.foldl updatePairLimits Nothing

countLimits : Int -> Dict Int Int -> Maybe (Int, Int)
countLimits n = pairLimits n << Dict.toList

type alias Double a = (a, a)


double : a -> (a, a)
double val = (val, val)


mapBoth : (a -> b) -> (a -> b) -> Double a -> Double b
mapBoth f g doub =
  Tuple.mapBoth f g doub


mapAll : (a -> b) -> Double a -> Double b
mapAll func doub =
  Tuple.mapBoth func func doub


shiftLimits : Model -> (Int, Int) -> (Int, Int)
shiftLimits model pair =
    pair
    |> mapBoth (\n -> n - 2)  (\n -> n + 2)
    |> mapBoth (Basics.max 0) (Basics.min model.n)

xLimits : Model -> Maybe (Int, Int)
xLimits model =
  case model.xData.val of
    Nothing ->
      Nothing

    Just val ->
        case model.statistic of
            -- When proportion, this needs to be converted back to counts to align
            Proportion ->
                let
                    nFloat = model.n |> toFloat
                    count = val |> \x -> x*nFloat |> round
                in
                    count
                    |> double
                    |> shiftLimits model
                    |> Just

            _ -> 
                val
                |> double
                |> mapAll round
                |> shiftLimits model
                |> Just


maybeMakeProportion : Model -> Int -> Float
maybeMakeProportion model x =
    case model.statistic of
        Proportion ->
            (toFloat x) / (toFloat model.n)

        _ ->
            x |> toFloat


largeLimits : Model -> Maybe (Int, Int)
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

plotLimits  : Model -> (Float, Float)
plotLimits model =
  let
    numSD = defaults.numSD
    mean = meanBinom model.n model.p
    sd = sdBinom model.n model.p
    combinedLimits = (combineLimits [ largeLimits model
                                    , xLimits model
                                    , countLimits model.n model.ys
                                    ])
  in
    Maybe.withDefault (0, model.n) combinedLimits |> mapAll (maybeMakeProportion model)


distPlot : Model -> Spec
distPlot model =
    let
        mean = meanBinom model.n model.p
        sd = sdBinom model.n model.p
        nFloat = toFloat model.n
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
                        else"datum.X <= " ++ (String.fromFloat lower) ++ " || " ++
                             "datum.X >= " ++ (String.fromFloat upper)

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
    mainGrid exampleSingleObservationView (collectButtonView model)  (pvalueView model) exampleSpinner blankSpinButton  exampleSample (debugView model)



