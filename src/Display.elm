module Display exposing (..)

import List.Extra exposing (..)

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


changeThousandsToK : Int -> String
changeThousandsToK n =
    let
        zeros = n
              |> toFloat
              |> logBase 10
              |> round
    in
        if zeros < 3 then (String.fromInt n) else (String.fromInt (10^(zeros - 3))) ++ "K"
        
