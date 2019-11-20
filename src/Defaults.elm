module Defaults exposing (..)


type alias Defaults = { n : Int 
                      , p : Float
                      , trimAt : Int
                      , collectNs : List Int
                      , minTrialsForPValue : Int
                      , distMinHeight : Float
                      , numSD : Float
                      , pValDigits : Int
                      , distPlotWidth : Float
                      , distPlotHeight : Float
                      }

defaults :  Defaults
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
