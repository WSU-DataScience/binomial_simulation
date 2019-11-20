module CountDict exposing (..)

import Dict exposing (..)

type alias CountDict = Dict Int Int 

updateY : Int -> CountDict -> CountDict
updateY x ys =
    Dict.update x updateCount ys


updateCount : Maybe Int -> Maybe Int
updateCount maybeN =
    case maybeN of
        Just n ->
            Just (n + 1)

        Nothing ->
            Just 1


updateCountDict : (Float -> Int) -> CountDict -> List Float -> CountDict
updateCountDict binomGen cnts outcomes =
  outcomes
  |> List.map binomGen
  |> List.foldl updateY cnts 