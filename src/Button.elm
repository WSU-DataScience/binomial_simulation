module Button exposing (..)


import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Display exposing (stringAndAddCommas, changeThousandsToK)


buttonAttrs : msg -> String -> List (Button.Option msg)
buttonAttrs msg txt = 
    [ Button.primary
    , Button.small
    , Button.onClick msg
    , Button.attrs 
        [ Html.Attributes.name txt 
        , Html.Attributes.id txt
        ]
    ] 




resetButton : msg -> Html msg
resetButton onClickMsg =
    Button.button
        (buttonAttrs onClickMsg "Reset")
        [ Html.text "Reset" ]


groupButton : msg -> String -> ButtonGroup.ButtonItem msg
groupButton msg lbl =
    ButtonGroup.button 
        (buttonAttrs msg lbl)
        [  Html.text lbl ]

buttonGroup : List msg -> List String -> Html msg
buttonGroup msgs txts =
    ButtonGroup.buttonGroup
        [ ButtonGroup.attrs [ style "display" "block"] ]
        (List.map2 groupButton msgs txts)


-- From Collect Stats


collectButtons : (Int -> msg) -> List Int -> Html msg
collectButtons toOnClick ns =
    let
        msgs = ns |> List.map toOnClick 
        txts = ns |> List.map changeThousandsToK 
    in
    div [] [ buttonGroup msgs txts ]



radioButton : String -> msg -> Bool -> ButtonGroup.RadioButtonItem msg
radioButton txt msg toggle =
        ButtonGroup.radioButton
            toggle
            (buttonAttrs msg txt)
            [ Html.text txt ]

radioButtons : List msg -> List String -> List Bool -> Html msg
radioButtons msgs txts toggles = 
    ButtonGroup.radioButtonGroup []
        (List.map3 radioButton txts msgs toggles)


-- From Animation


spinButton : msg -> String -> Html msg
spinButton msg txt =
    Button.button
        (buttonAttrs msg "spin")
        [ Html.text txt]

