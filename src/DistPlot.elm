module DistPlot exposing (..)

import CountDict exposing (..)
import Dict exposing (..)
import DataEntry exposing (..)


type alias Count = (Int, Int)

combineDistColumns : Count -> (List Int, List Int) -> (List Int , List Int)
combineDistColumns pair columns =
  let
    (newX, newY) = pair
    (oldXs, oldYs) = columns
  in
    (newX :: oldXs, newY :: oldYs)


distColumns : CountDict -> (List Float, List Float)
distColumns yDict =
  yDict
  |> Dict.toList
  |> List.foldl combineDistColumns ([], [])
  |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToDots : Count -> (List Int, List Int)
countPairToDots pair =
  let
    (x, cnt) = pair
    xs = List.repeat cnt x
    ys = List.range 1 cnt
  in
    (xs, ys)


combineDotColumns : Count -> (List Int, List Int) -> (List Int, List Int)
combineDotColumns nextPair columns =
  let
    (newXs, newYs) = countPairToDots nextPair
    (oldXs, oldYs) = columns
  in
    (newXs ++ oldXs, newYs ++ oldYs)


dotColumns : CountDict -> (List Float, List Float)
dotColumns yDict =
  let
    countList = Dict.toList yDict
  in
    countList
    |> List.foldl combineDotColumns ([], [])
    |> Tuple.mapBoth (List.map toFloat) (List.map toFloat)


countPairToHeights : Count ->  List Int
countPairToHeights pair =
  let
    (_, cnt) = pair
  in
    List.repeat cnt cnt


combineHeights : Count -> List Int -> List Int
combineHeights nextPair heights =
    (countPairToHeights nextPair) ++ heights

dotColumnHeights : CountDict -> List Float
dotColumnHeights yDict = 
    yDict
    |> Dict.toList 
    |> List.foldl combineHeights [] 
    |> List.map toFloat


updateMax : Count -> Int -> Int
updateMax pair currentMax =
  let
    ( _ , newY) = pair
  in
    Basics.max newY currentMax

maxHeight : CountDict -> Int
maxHeight yDict =
  yDict
  |> Dict.toList
  |> List.foldl updateMax 0

type alias Limits = (Int, Int)


combineTwoLimits : Limits -> Limits -> Limits
combineTwoLimits lim1 lim2 =
  let
    (min1, max1) = lim1
    (min2, max2) = lim2
  in
    ( Basics.min min1 min2
    , Basics.max max1 max2
    )


updateLimits : Maybe Limits -> Maybe Limits -> Maybe Limits
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

      
combineLimits : List (Maybe Limits) -> Maybe Limits
combineLimits = List.foldl updateLimits Nothing


updatePairLimits : Count -> Maybe Limits -> Maybe Limits
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


pairLimits : Int -> List Count -> Maybe Limits
pairLimits n = List.foldl updatePairLimits Nothing


countLimits : Int -> CountDict -> Maybe Limits
countLimits n = pairLimits n << Dict.toList

type alias Double a = (a, a)

double : a -> Double a
double val = (val, val)


mapBoth : (a -> b) -> (a -> b) -> Double a -> Double b
mapBoth f g doub =
  Tuple.mapBoth f g doub


mapAll : (a -> b) -> Double a -> Double b
mapAll func doub =
  Tuple.mapBoth func func doub


shiftLimits : Int -> Limits -> Limits
shiftLimits n pair =
    pair
    |> mapBoth (\i -> i - 2)  (\i -> i + 2)
    |> mapBoth (Basics.max 0) (Basics.min n)

xLimits : NumericData Float -> Statistic -> Int -> Maybe Limits
xLimits xData statistic n =
  case xData.val of
    Nothing ->
      Nothing

    Just val ->
        case statistic of
            -- When proportion, this needs to be converted back to counts to align
            Proportion ->
                let
                    nFloat = n |> toFloat
                    count = val |> \x -> x*nFloat |> round
                in
                    count
                    |> double
                    |> shiftLimits n
                    |> Just

            _ -> 
                val
                |> double
                |> mapAll round
                |> shiftLimits n
                |> Just


maybeMakeProportion : Statistic -> Int -> Int -> Float
maybeMakeProportion statistic n x =
    case statistic of
        Proportion ->
            (toFloat x) / (toFloat n)

        _ ->
            x |> toFloat


