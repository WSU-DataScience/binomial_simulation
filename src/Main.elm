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
        oldSampleModel = model.sample
        oldSample = oldSampleModel.sample
        newSample = { oldSample | successLbl = model.singleObservation.successLbl.str}
        newSampleModel = { oldSampleModel | sample = newSample }
      in
        {model | sample = newSampleModel}

    _ -> 
      model

updateSpinnerFailure : Model -> Model
updateSpinnerFailure model =
  case model.singleObservation.failureLbl.state of
    Correct ->
      {model | spinner = (updateFailure model.singleObservation.failureLbl.str model.spinner)}

    _ -> 
      model


updateSampleFailure : Model -> Model
updateSampleFailure model =
  case model.singleObservation.failureLbl.state of
    Correct ->
      let
        oldSampleModel = model.sample
        oldSample : Sample
        oldSample = oldSampleModel.sample
        newSample = { oldSample | failureLbl = model.singleObservation.failureLbl.str}
        newSampleModel = { oldSampleModel | sample = newSample }
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
      {model | sample = OneSample.updateP model.singleObservation.pData.val model.sample}

    _ -> 
      model


updateSpinnerVisibility : Model -> Model
updateSpinnerVisibility model = 
      let
        canShow = (  model.singleObservation.successLbl.state == Correct 
                  && model.singleObservation.failureLbl.state == Correct
                  && model.singleObservation.pData.state      == Correct
                  )
        vis = if canShow then Shown else Hidden
      in
        { model  | spinner = updateVisibility vis model.spinner}


updateSpinner model =
  model
  |> updateSpinnerSuccess
  |> updateSpinnerFailure
  |> updateSpinnerP
  |> updateSpinnerVisibility

updateSampleFromObs model =
  model
  |> updateSampleSuccess
  |> updateSampleFailure
  |> updateSampleP

updateSpinnerLocation : Float -> Spinner.Model -> Spinner.Model
updateSpinnerLocation location model =
  { model | handLocation = location }


updateLocation : Model -> Model
updateLocation model =
  let
    location = getCurrentLocation model.animation
  in
    { model | spinner = updateSpinnerLocation location model.spinner }
 

updateSampleFromAnim : Animation.Model -> Model -> Model
updateSampleFromAnim oldModel model =
  let
    oldOutcomes = oldModel.sample 
    newOutcomes = model.animation.sample  
    newSample =
      if oldOutcomes /= newOutcomes then
        model.sample |> updateSampleFromOutcome newOutcomes
      else
        model.sample
  in
    { model | sample = newSample }

updateAnimationN model =
  let
    n = model.sample.n
    oldAnimation = model.animation
    newAnimation = { oldAnimation | n = n}
  in
    { model | animation = newAnimation}
    

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

            _ ->
              (model.collect, Cmd.none)
        finalModel = 
          { model | singleObservation = newModel, collect = collectModel} 
            |> updateSpinner
            |> updateSampleFromObs
        in 
           (finalModel
           , samplePlotCmd finalModel
           )

    SpinnerMsg sPmsg -> 
      let (newModel, newCmd) = Spinner.update sPmsg (.spinner model)
        in ({ model | spinner = newModel } 
           , Cmd.map SpinnerMsg newCmd
           )


    AnimationMsg aMsg -> 
      let 
        (newModel, newCmd) = Animation.update aMsg (.animation model)
        newOutcomes = newModel.sample
        oldOutcomes = model.animation.sample
        outcomesChanged = newOutcomes /= oldOutcomes
        notSpinMsg = 
          case aMsg of
            Spin ->
              False

            _ ->
              True
        updatedSample = 
          if outcomesChanged then
            model.sample |> updateSampleFromOutcome newOutcomes
          else
            model.sample
        -- checking notSpinMsg assure there no Cmd collisions/loss
        -- as animiation commands only happen on Spin commands
        finalModel = 
          { model | animation = newModel, sample = updatedSample} 
            |> updateLocation
        nextCmd = if notSpinMsg && outcomesChanged then
                      samplePlotCmd finalModel
                  else
                      Cmd.map AnimationMsg newCmd
        in ( finalModel
           , nextCmd
           )

    SampleMsg sMsg -> 
      let 
        (newModel, _) = OneSample.update sMsg (.sample model)
        (animation, _) = 
          case sMsg of
            OneSample.ChangeN _ ->
               Animation.update (Animation.UpdateN newModel.nData) model.animation

            _ ->
               (model.animation, Cmd.none)

        (collectModel, _) =
          case sMsg of
            OneSample.ChangeN _ ->
              CollectStats.update (CollectStats.UpdateN newModel.nData) model.collect

            _ ->
              (model.collect, Cmd.none)
        in ({ model | sample = newModel, animation = animation, collect = collectModel} 
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
                , Dropdown.subscriptions model.collect.pulldown (CollectMsg << ChangePulldown) ]
      
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
        ]
  else
    div [] []

maybeCollectView model =
  let
    show = (model.singleObservation.pData.state == Correct
           && model.sample.nData.state == Correct
           && model.spinner.visibility == Shown)
  in
    if show then
      collectButtonView model.collect
    else
      div [] []

maybePValueView model =
  let
    show = (model.singleObservation.pData.state == Correct
           && model.sample.nData.state == Correct
           && model.spinner.visibility == Shown)
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


