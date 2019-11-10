module Binomial exposing (..)

import List.Extra exposing (..)
import Array exposing (..)

meanBinom : Int -> Float -> Float
meanBinom n p = (toFloat n)*p

sdBinom : Int -> Float -> Float
sdBinom n p = ((meanBinom n p)*(1 - p))^0.5

binomLogCoef : Int -> Int -> Float
binomLogCoef n k =
    let
        nF = toFloat n
        kF = toFloat k
        ks = List.map toFloat (List.range 0 (k-1))
        terms = List.map (\i -> logBase 10 (nF - i) - logBase 10 (kF - i)) ks
    in
        List.sum terms


binomLogCoefRange : Int -> Int -> Int -> List(Float)
binomLogCoefRange n start stop =
    let
        coefStart = binomLogCoef n start
        logDiff = \k -> logBase 10 ( (toFloat n) - k + 1) - logBase 10 k
        nums = List.map toFloat (List.range (start + 1) n)
        logDiffs = List.map logDiff nums 
    in
        (List.Extra.scanl (+) coefStart logDiffs)

probRange: Int -> Float -> Int -> Int -> List(Float)
probRange n p start stop =
    let
        xs = List.map toFloat (List.range start stop)
        binomceof = binomLogCoefRange n start stop
        term = \logCoef x -> logCoef + x*(logBase 10 p) + ((toFloat n) - x)*(logBase 10 (1 - p)) 
        logProbs = List.map2 term binomceof xs
    in
        List.map (\logp -> 10^logp) logProbs


roundFloat : Int -> Float -> Float
roundFloat digits n =
   let
     div = toFloat 10^(toFloat digits)
     shifted = n*div
   in
     round shifted |> toFloat |> \x -> x/div


addAndAppend : Float -> List(Float) -> List(Float)
addAndAppend n acc =
    case List.head acc of
        Just subTotal ->
            (n + subTotal) :: acc
        Nothing ->
            [n]


cumultSuml l = List.reverse (List.foldl addAndAppend [] l)


cumultSumr l = List.foldr addAndAppend [] l


trimmedXRange trimAt n p =
    let
        mean = meanBinom n p
        sd = sdBinom n p
        minX = Basics.max 0 (round (mean - 6*sd))
        maxX = Basics.min n (round (mean + 6*sd))
    in
        if n < trimAt then (0, n) else (minX, maxX)


trimmedXs trimAt n p =
    let
        (min, max) = trimmedXRange trimAt n p
    in
        List.range min max


trimmedProbs : Int -> Int -> Float -> List Float
trimmedProbs trimAt n p =
    let
        (minX, maxX ) = trimmedXRange trimAt n p
    in
        probRange n p minX maxX

twoTailLimits : Float -> Float -> (Float, Float)
twoTailLimits mean value =
    let
        diff = if (value <= mean) then mean - value else value - mean
    in
        (mean - diff, mean + diff)

-- Components of a square histogram
type alias Bar = { i : Int
                 , pi : Float
                 , k : Int
                 , v : Float
                 }


type alias SquareHistogram = List Bar


initBar a i p = 
     { i = i
     , pi = p
     , k = i
     , v = (toFloat (i + 1)) * a
     }


initSqrHist ps =
    let
        n = List.length ps
        a = 1.0 / (toFloat n)
        ks = List.range 0 n
    in
        List.map2 (initBar a) ks ps

        
type alias SortedBars = { n : Int
                        , a : Float
                        , under : List Bar
                        , over : List Bar
                        , full : List Bar
                        }

        

emptySortedBars : Int -> SortedBars
emptySortedBars n = { n = n
                    , a = 1.0 / (toFloat n)
                    , under = []
                    , over = []
                    , full = []
                    }

processBar : Bar -> SortedBars -> SortedBars
processBar bar sortedBars =
    let
        fillHeight = sortedBars.a
    in
        if bar.pi < fillHeight then
            { sortedBars | under = bar :: sortedBars.under }
        else if bar.pi > fillHeight then
            { sortedBars | over = bar :: sortedBars.over }
        else
            { sortedBars | full = bar :: sortedBars.full }

sortBars : List Bar -> SortedBars
sortBars bars = 
    let
        n = List.length bars
    in 
        List.foldl processBar (emptySortedBars n) bars


updateBars : SortedBars -> SortedBars
updateBars bars =
    let
        small = bars.under |> List.sortBy .pi 
        big = bars.over  |> List.sortBy .pi |> List.reverse
    in
        case (small, big) of
            ([], _) ->
                bars

            (_, []) ->
                bars

            (minBar :: restUnder, maxBar :: restOver) ->
                let
                    min = { minBar | k = maxBar.i
                                   , v = (toFloat minBar.i) * bars.a + minBar.pi
                                   , pi = bars.a
                          } 
                    max = { maxBar | pi = maxBar.pi - (bars.a - minBar.pi) } 
                    full = min :: bars.full
                in
                    if max.pi < bars.a then 
                        updateBars { bars | under = max :: restUnder
                                   , over = restOver
                                   , full = full
                                   }
                    else if max.pi > bars.a then
                        updateBars { bars | under = restUnder
                                   , over = max :: restOver
                                   , full = full
                                   }
                    else
                        updateBars { bars | under = restUnder
                                   , over = restOver
                                   , full = max :: full
                                   }

fixV bar =
    { bar | v = 1.0 } 

postProcBars bars =
    (bars.under ++ bars.over ++ bars.full) |> List.sortBy .i


convertToBinomial : Float -> Array Float -> Array Int -> Int -> Float -> Int
convertToBinomial n vs ks min u =
    let
        j = u * n |> floor
        mv = vs |> Array.get j
        mk = ks |> Array.get j
    in
        case (mv, mk) of
            (Nothing, _) ->
                -1

            (_, Nothing) ->
                -1

            (Just v, Just k) ->
                if u < v then min + j else min + k

makeConvertToBinomial : Int -> List Bar -> (Float -> Int)
makeConvertToBinomial min bars =
    let
        n = bars |> List.length |> toFloat
        vs = bars |> List.map .v |> Array.fromList
        ks = bars |> List.map .k |> Array.fromList
    in
        convertToBinomial n vs ks min
         
        
        
getVArray bars =
    Array.fromList (List.map .v bars)

wrapIn left right s =
    [ left
    , s
    , right
    ] |> String.join " "


wrapList = wrapIn "[" "]"


wrapRecord = wrapIn "{" "}"


wrapTuple = wrapIn "(" ")"


initBarList = List.map2 initBar 


addTitleComma title s =
    [ title 
    , " = "
    , s 
    , ","
    ] |> String.join ""


getP bar =
    bar.pi 

barString : Bar -> String
barString bar =
    bar
    |> List.repeat 4
    |> \el -> List.Extra.andMap el [ .i >> String.fromInt >> addTitleComma "i"
                                   , .k >> String.fromInt >> addTitleComma "k"
                                   , .v >> roundFloat 5 >> String.fromFloat >> addTitleComma "v"
                                   , .pi >> roundFloat 5 >> String.fromFloat >> addTitleComma "pi"
                                   ]
    |> String.join " "
    |> wrapRecord


barListString : List Bar -> String
barListString bars =
    bars
    |> List.map barString
    |> String.join " "
    |> wrapList


sortedBarString : SortedBars -> String
sortedBarString sortedBars = 
    sortedBars
    |> List.repeat 5
    |> \rec -> List.Extra.andMap rec [ .a >> String.fromFloat >> addTitleComma "a" 
                                     , .n >> String.fromInt >> addTitleComma "n"
                                     , .under >> barListString >> addTitleComma "\n\nunderFull\n\n"
                                     , .over >> barListString >> addTitleComma "\n\noverFull\n\n"
                                     , .full >> barListString >> addTitleComma "\n\nfull\n\n"
                                     ]
    |> String.join ", "
    |> wrapRecord


listSortedBarString el =
    el
    |> List.map sortedBarString
    |> String.join ", "
    |> wrapList
