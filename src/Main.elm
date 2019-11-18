port module Main exposing (..)

import Time
import Debug
import Layout exposing (..)
import DataEntry exposing (..)
import SingleObservation exposing (..)
import Spinner exposing (..)
import Animation exposing (..)
import OneSample exposing (..)
import CollectStats exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import VegaLite exposing (..)
import Bootstrap.Dropdown as Dropdown

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg = SingleObservationMsg SingleObservation.Msg 
         | SpinnerMsg Spinner.Msg
         | AnimationMsg Animation.Msg
         | SampleMsg OneSample.Msg
         | CollectMsg CollectStats.Msg

type alias Model = 
    { singleObservation : SingleObservation.Model
    , spinner : Spinner.Model
    , animation : Animation.Model
    , sample : OneSample.Model
    , collect : CollectStats.Model
    , debug : Bool
    }

initModel = { singleObservation = SingleObservation.initModel
            , spinner = Spinner.initModel
            , animation = Animation.initModel
            , sample = OneSample.initModel
            , collect = CollectStats.initModel
            , debug = False
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none )


-- update spinner model


-- The spinner needs the following information from Single Observation
--    successLbl
--    failureLbl
--    pO


updateSuccess : String -> Spinner.Model -> Spinner.Model
updateSuccess lbl model = 
  {model | successLbl = lbl }


updateFailure : String -> Spinner.Model -> Spinner.Model
updateFailure lbl model = 
  {model | failureLbl = lbl }


updateP : Maybe Float -> Spinner.Model -> Spinner.Model
updateP val model = 
  {model | p = Maybe.withDefault model.p val }


updateVisibility : Visibility -> Spinner.Model -> Spinner.Model
updateVisibility visibility model = 
        { model  | visibility = visibility}


updateSpinnerSuccess : Model -> Model
updateSpinnerSuccess model =
  case model.singleObservation.successLbl.state of
    Correct ->
      {model | spinner = updateSuccess model.singleObservation.successLbl.str model.spinner}

    _ -> 
      model

updateSampleSuccess : Model -> Model
updateSampleSuccess model =
  case model.singleObservation.successLbl.state of
    Correct ->
      let
        lbl = model.singleObservation.successLbl.str
        (newSampleModel, _) = model.sample |> OneSample.update (OneSample.ChangeSuccessLbl lbl)
      in
        {model | sample = newSampleModel}

    _ -> 
      model

updateCollectSuccess : Model -> Model
updateCollectSuccess model =
  let
    (newCollectModel, _) = model.collect |> CollectStats.update (CollectStats.ChangeSuccessLbl model.singleObservation.successLbl)
  in
    {model | collect = newCollectModel}


updateSpinnerFailure : Model -> Model
updateSpinnerFailure model =
  case model.singleObservation.failureLbl.state of
    Correct ->
      {model | spinner = updateFailure model.singleObservation.failureLbl.str model.spinner}

    _ -> 
      model


updateSampleFailure : Model -> Model
updateSampleFailure model =
  case model.singleObservation.failureLbl.state of
    Correct ->
      let
        lbl = model.singleObservation.failureLbl.str
        (newSampleModel, _) = model.sample |> OneSample.update (OneSample.ChangeFailureLbl lbl)
      in
        {model | sample = newSampleModel}

    _ -> 
      model


updateSpinnerP : Model -> Model
updateSpinnerP model =
  case model.singleObservation.pData.state of
    Correct ->
      {model | spinner = updateP model.singleObservation.pData.val model.spinner}

    _ -> 
      model

updateSampleP : Model -> Model
updateSampleP model =
  case model.singleObservation.pData.state of
    Correct ->
      let
        pData = model.singleObservation.pData
        (newSample, _) = model.sample |> OneSample.update (OneSample.ChangeP pData)
      in
        {model | sample = newSample }

    _ -> 
      model

updateSampleN : Model -> Model
updateSampleN model =
  case model.singleObservation.nData.state of
    Correct ->
      let
        nData = model.singleObservation.nData
        (newSample, _) = model.sample |> OneSample.update (OneSample.ChangeN nData)
      in
        {model | sample = newSample }

    _ -> 
      model

updateSampleStatistic model =
  let
    sampleModel = model.sample
    newSampleModel = { sampleModel | statistic = model.singleObservation.statistic}
  in
    { model | sample = newSampleModel}

updateSpinnerVisibility : Model -> Model
updateSpinnerVisibility model = 
      let
        canShow = model.singleObservation.successLbl.state == Correct 
                  && model.singleObservation.failureLbl.state == Correct
                  && model.singleObservation.pData.state      == Correct
                  && model.singleObservation.nData.state      == Correct
                  && model.singleObservation.statistic        /= NotSelected 
        vis = if canShow then Shown else Hidden
      in
        { model  | spinner = updateVisibility vis model.spinner}


updateSpinner model =
  model
  |> updateSpinnerSuccess
  |> updateSpinnerFailure
  |> updateSpinnerP
  |> updateSpinnerVisibility

updateSample model =
  model
  |> updateSampleSuccess
  |> updateSampleFailure
  |> updateSampleP
  |> updateSampleN
  |> updateSampleStatistic


updateSpinnerLocation : Float -> Spinner.Model -> Spinner.Model
updateSpinnerLocation location model =
  { model | handLocation = location }


updateLocation : Model -> Model
updateLocation model =
  let
    location = getCurrentLocation model.animation
  in
    { model | spinner = updateSpinnerLocation location model.spinner }
 


updateAnimationN : Model -> Model
updateAnimationN model =
  case model.singleObservation.nData.state of
    Correct ->
      let
        n = Maybe.withDefault 20 model.singleObservation.nData.val
        oldAnimation = model.animation
        newAnimation = { oldAnimation | n = n}
      in
        { model | animation = newAnimation}

    _ ->
      model
    
updateAnimationOff : Model -> Model
updateAnimationOff model =
  let
      n = Maybe.withDefault 20 model.singleObservation.nData.val
      turnOff = n >= CollectStats.defaults.trimAt
      oldAnimation = model.animation
      newAnimation = { oldAnimation | animationOff = turnOff }
  in
      { model | animation = newAnimation }
  
update msg model = 
  case msg of 
    -- Note: There are no singleObservation commands, 
    --       but we do need to send a OneSample command when the labels change
    SingleObservationMsg soMsg -> 
      let 
        (newModel, newCmd) = SingleObservation.update soMsg (.singleObservation model)
        (collectModel, _) =
          case soMsg of
            SingleObservation.ChangeP _ ->
              CollectStats.update (CollectStats.UpdateP newModel.pData) model.collect

            SingleObservation.ChangeN _ ->
              CollectStats.update (CollectStats.UpdateN newModel.nData) model.collect

            SingleObservation.UseCount ->
              CollectStats.update CollectStats.UseCount model.collect

            SingleObservation.UseProp ->
              CollectStats.update CollectStats.UseProp model.collect
            _ ->
              (model.collect, Cmd.none)
        finalModel = 
          { model | singleObservation = newModel, collect = collectModel} 
            |> updateSpinner
            |> updateSample
            |> updateAnimationN
            |> updateCollectSuccess
            |> updateAnimationOff
        in 
           (finalModel
           , distPlotCmd finalModel.collect
           )

    SpinnerMsg sPmsg -> 
      let (newModel, newCmd) = Spinner.update sPmsg (.spinner model)
        in ({ model | spinner = newModel } 
           , Cmd.map SpinnerMsg newCmd
           )


    AnimationMsg aMsg -> 
      let 
        (newModel, newCmd) = Animation.update aMsg (.animation model)

        sampleModel = 
          case newModel.state of
            Animation.UpdateSample _ ->
              let
                outcomes = newModel.sample
                newSample = updateSampleFromOutcome outcomes model.sample
                currentSampleModel = model.sample
              in
                { currentSampleModel | sample = newSample}

            UpdateDist _ ->
              let
                outcomes = newModel.sample
                newSample = updateSampleFromOutcome outcomes model.sample
                currentSampleModel = model.sample
              in
                { currentSampleModel | sample = newSample}

            _ ->
              model.sample

        (collectModel, _ ) = 
          case newModel.state of
            UpdateDist _ ->
              let
                numSuccess = newModel.sample 
                           |> List.map (outcome model.sample.p) 
                           |> List.sum
              in
                CollectStats.update (CollectStats.OneNewStatistic  numSuccess) model.collect

            _ ->
              (model.collect, Cmd.none)

        finalModel = 
          { model | animation = newModel, sample = sampleModel, collect = collectModel} 
            |> updateLocation

        finalCmd =
          case newModel.state of
            Animation.UpdateSample _ ->
              samplePlotCmd finalModel

            UpdateSampleNoAnimation _ ->
              samplePlotCmd finalModel

            UpdateDist _ ->
              distPlotCmd collectModel

            _ ->
              Cmd.map AnimationMsg newCmd

      in ( finalModel
         , finalCmd
         )

    SampleMsg sMsg -> 
      let 
        (newModel, _) = OneSample.update sMsg (.sample model)
        (collectModel, _ ) = 
          case sMsg of
            OneSample.UpdateSample ws ->
              let
                numSuccess = ws |> List.map (outcome model.sample.p) |> List.sum
              in
                CollectStats.update (CollectStats.OneNewStatistic  numSuccess) model.collect

            _ ->
              (model.collect, Cmd.none)
        in ({ model | sample = newModel, collect = collectModel}
           , Cmd.none
           )

    CollectMsg cMsg -> 
      let 
        (newModel, newCmd) = CollectStats.update cMsg (.collect model)
        finalCmd =
          case cMsg of
            Collect _ ->
              Cmd.map CollectMsg newCmd

            _ ->
              distPlotCmd newModel

      in ({ model | collect = newModel } 
         , finalCmd
         )


-- port

samplePlotCmd model =
  let
    n : Int
    n = model.sample.n
    sample : OneSample.Sample
    sample = model.sample.sample
  in
      samplePlotToJS (samplePlot n sample)

distPlotCmd model =
    distPlotToJS (distPlot model)

-- send samplePlot to vega
port samplePlotToJS : Spec -> Cmd msg

port distPlotToJS : Spec -> Cmd msg
-- subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    baseBatch = [ Dropdown.subscriptions model.animation.splitDropState (AnimationMsg << SplitMsg)
                , Dropdown.subscriptions model.singleObservation.pulldown (SingleObservationMsg << ChangePulldown) ]
      
  in
    
    case model.animation.state of
      NotSpinning _ ->
        Sub.batch baseBatch


      _ -> 
        Sub.batch ((Time.every 10 (AnimationMsg << Tick)) :: baseBatch)

spinButtonView model =
  if model.spinner.visibility == Shown then
      ( Html.map AnimationMsg <| spinButtonGrid model.animation )
  else 
      div [] []
-- view


debugView : Model -> Html msg
debugView model =
  if model.debug then
    div []
        [ Html.h4 [] [Html.text "Test Stuff"]
        , makeHtmlText "spinner: " (model.spinner |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "single obs: " (model.singleObservation |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "animation: " (model.animation |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "sample: " (model.sample |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "collect: " (model.collect |> Debug.toString)
        , br [] [] 
        , br [] [] 
        , makeHtmlText "countLimits: " (model.collect.ys 
                                        |> countLimits model.collect.n
                                        |> Debug.toString)
        , br [] [] 
        , makeHtmlText "xLimits: " (model.collect
                                   |> xLimits
                                   |> Debug.toString)
        , br [] [] 
        , makeHtmlText "largeLimits: " (model.collect
                                        |> largeLimits
                                        |> Debug.toString)
        , br [] [] 
        , makeHtmlText "xData: " (model.collect.xData
                                        |> Debug.toString)
        , br [] [] 
        ]
  else
    div [] []

maybeCollectView model =
  let
    show =  model.spinner.visibility == Shown
  in
    if show then
      collectButtonView model.collect
    else
      div [] []

maybePValueView model =
  let
    show =  model.spinner.visibility == Shown
  in
    if show then
      pvalueView model.collect
    else
      div [] []
    
getSpinnerModel : Model -> Spinner.Model
getSpinnerModel model =
  let
    baseSpinner =   { p = Maybe.withDefault 0.25 model.singleObservation.pData.val
                    , currentOutcome = ""
                    , handLocation = 0
                    , successLbl = model.singleObservation.successLbl.str
                    , failureLbl = model.singleObservation.failureLbl.str
                    , visibility = model.spinner.visibility
                    }
  in
    case model.animation.state of
        NotSpinning s ->
          baseSpinner |> updateWithLocation s.location

        Spinning s ->
          baseSpinner |> updateWithLocation s.location

        Paused s ->
          baseSpinner |> updateWithLocation s.location

        Animation.UpdateSample s ->
          baseSpinner |> updateWithLocation s.location

        UpdateSampleNoAnimation s ->
          baseSpinner |> updateWithLocation s.location

        UpdateDist s ->
          baseSpinner |> updateWithLocation s.location



view : Model -> Html Msg
view model =
    mainGrid 
      ( Html.map SingleObservationMsg <| singleObservationView model.singleObservation) 
      ( Html.map CollectMsg <| maybeCollectView model)
      ( Html.map CollectMsg <| maybePValueView model)
      ( Html.map SpinnerMsg <| spinnerView Spinner.initSpinnerConfig (getSpinnerModel model))
      (spinButtonView model)
      (Html.map SampleMsg <| maybeSampleView model.spinner.visibility model.sample)
      (debugView model) 


