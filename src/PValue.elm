module PValue exposing (..)

import DataEntry exposing (..)
import Display exposing (stringAndAddCommas)
import Defaults exposing (..)
import Dict exposing (..)
import Binomial exposing (..)


type Tail = Left | Right | Two | None


type PValue a = NoPValue | Lower a | Upper a | TwoTail a a 


type alias PValueData a = { a | n : Int
                              , p : Float
                              , tail : Tail
                              , xData : NumericData Float
                              , statistic : Statistic
                              , ys : Dict Int Int
                          }


maybeMakeCount : PValueData a -> Float -> Float
maybeMakeCount model x =
  case model.statistic of
    Proportion ->
        x * (toFloat model.n)

    _ ->
        x


inLower : Float -> (Int, Int) -> Int
inLower l pair =
  let 
    (cnt, freq) = pair
    cntF = cnt |> toFloat
  in
    if cntF <= l then freq else 0


inUpper : Float -> (Int, Int) -> Int
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
      

startingPValue : Tail -> PValue Int
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


tailLimitCount : PValueData a -> PValue Float
tailLimitCount model =
  case (model.xData.val, model.tail) of
    (Nothing, _) ->
      NoPValue
    
    (_, None) ->
      NoPValue

    (Just x, Left) ->
      x |> maybeMakeCount model |> Lower

    (Just x, Right) ->
      x |> maybeMakeCount model |> Upper

    (Just x, Two) ->
      let 
        mean = meanBinom model.n model.p
        sd = sdBinom model.n model.p
        xNew = x |> maybeMakeCount model
        distToMean = Basics.abs (xNew - mean)
      in
        TwoTail ((mean - distToMean) |> roundFloat 4) ((mean + distToMean) |> roundFloat 4)


tailLimitProportion : PValueData a -> PValue Float
tailLimitProportion model =
    let
        nFloat = model.n |> toFloat
        divideByN = \x -> x/nFloat
        tailLim = model |> tailLimitCount
    in
        case tailLim of
            NoPValue ->
                NoPValue

            Lower l ->
                l |> divideByN |> Lower

            Upper l ->
                l |> divideByN |> Upper

            TwoTail l u ->
                TwoTail (l |> divideByN) (u |> divideByN)


tailLimit : PValueData a -> PValue Float
tailLimit model =
    case model.statistic of
        Proportion ->
            model |> tailLimitProportion

        _ ->
            model |> tailLimitCount


getPValue : PValueData a -> PValue Int
getPValue model =
  case model.xData.val of
    Nothing ->
      NoPValue

    Just x ->
      model.ys
      |> Dict.toList
      |> List.map (inTail (tailLimitCount model))
      |> List.foldl (addTails) (startingPValue model.tail)


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
     