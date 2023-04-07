module Main exposing (..)

-- #region Core Imports
import Browser
import Browser.Events
import Element as E
import Element.Border as Border
import Element.Font as Font
import Html as H
import Html.Attributes as A
import Time
import Task
import Browser.Dom as Dom
import Element.Background as Background
import Browser.Navigation as Nav
import Html.Events as Events
import GraphicSVG as S
import GraphicSVG.Widget as Widget
import Svg
import Svg.Attributes as SA
-- #endregion

-- #region My Imports
import Core as C
import Utils as U
import MVCore as EC
import Views exposing(..)
import Graph as GraphPage
import Home as HomePage
import Help as HelpPage
import Resources as ResourcesPage
-- #endregion

editMatrixField : C.MatrixField -> Float -> Maybe C.Matrix -> Maybe C.Matrix
editMatrixField f v ma =
  case ma of
      Just a -> Just <| C.setMatrixField f a v
      Nothing -> Just <| C.setMatrixField f C.identityMatrix v

editVectorField : C.VectorField -> Float -> Maybe C.Vector -> Maybe C.Vector
editVectorField f v ma =
  case ma of
      Just a -> Just <| C.setVectorField f a v
      Nothing -> Just <| C.setVectorField f C.zeroVector v

update : EC.Msg -> EC.Model -> ( EC.Model, Cmd EC.Msg )
update msg model = 
  let
    ti = Debug.log "Update MSG" msg
  in
  Debug.log "UpdateOccurred" <| case msg of
    EC.WindowResize w h ->
      ({ model | windowSize = Debug.log "WS" (w,h) }, Cmd.none)
    EC.Tick t ->
      (model, Cmd.none)

    EC.NewMatrixEntry ->
      { model | matrixEntries = model.matrixEntries ++ [EC.emptyMatrixEntry] } |> update (EC.UpdateEffective model.effectiveIdx)
    EC.DeleteMatrixEntry idx ->
      let
        newEffec =  if (idx == model.effectiveIdx) then
                      -1
                    else if (model.effectiveIdx < idx) then
                      model.effectiveIdx
                    else
                      model.effectiveIdx - 1
      in
      { model | matrixEntries = U.removeAt idx model.matrixEntries} |> update (EC.UpdateEffective newEffec)
    EC.EditMatrixName idx newName ->
      { model | matrixEntries = U.updateAt idx (\x -> { x | name = Just newName }) model.matrixEntries } |> update (EC.UpdateEffective model.effectiveIdx)
    EC.EditMatrixFloat idx field val ->
      { model | matrixEntries = U.updateAt  idx (\x -> { x | matrix = editMatrixField field val x.matrix }) model.matrixEntries } |> update (EC.UpdateEffective model.effectiveIdx)

    EC.NewVectorEntry ->
      ({ model | vectorEntries = model.vectorEntries ++ [EC.emptyVectorEntry] }, Cmd.none)
    EC.DeleteVectorEntry idx ->
      ({ model | vectorEntries = U.removeAt idx model.vectorEntries }, Cmd.none)
    EC.EditVectorName idx newName ->
      ({ model | vectorEntries = U.updateAt idx (\x -> { x | name = Just newName }) model.vectorEntries }, Cmd.none)
    EC.EditVectorFloat idx field val ->
      ({ model | vectorEntries = U.updateAt idx (\x -> { x | vector = editVectorField field val x.vector }) model.vectorEntries }, Cmd.none)
    
    EC.UpdateEffective idx ->
      let
        nEffec =  if (List.isEmpty model.matrixEntries || idx < 0 || idx > List.length model.matrixEntries) then
                    C.identityMatrix
                  else
                    case (U.elem idx model.matrixEntries |> .matrix) of
                        Just m -> m
                        Nothing -> C.identityMatrix
        q = Debug.log "DET" <| C.determinant nEffec
      in
      ({ model | effectiveMatrix = nEffec, effectiveIdx = idx }, Cmd.none)
    EC.ToggleOptionsModal ->
      ({ model | showOptions = not model.showOptions }, Cmd.none)
    EC.ZoomIn ->
      ({ model | gap = U.clamp 5 100 <| model.gap + 5}, Cmd.none) |> Debug.log "ZOOMIN"
    EC.ZoomOut ->
      ({ model | gap = U.clamp 5 100 <| model.gap - 5}, Cmd.none) |> Debug.log "ZOOMOUT"
    EC.ToggleGridView ->
      ({ model | showGrid = not model.showGrid }, Cmd.none)

    EC.SwitchPage page ->
      let
        path = case page of
                EC.Graph -> "/graph"
                EC.Home -> "/"
                EC.Resources -> "/resources"
                EC.Help -> "/help"
      in
      -- ({ model | page = page }, Cmd.none)
      (model, Nav.pushUrl model.navKey <| path )


    EC.ChangeUrl url ->
      case url.path of
        "/graph" -> ({ model | page = EC.Graph }, Cmd.none)
        "/resources" -> ({ model | page = EC.Resources }, Cmd.none)
        "/help" -> ({ model | page = EC.Help }, Cmd.none)
        _ -> ({ model | page = EC.Home }, Cmd.none)
    EC.RequestUrl req ->
      case req of
        Browser.Internal url -> (model, Nav.pushUrl model.navKey <| url.path)
        Browser.External url -> (model, Nav.load url)

    _ -> (model,Cmd.none)


view : EC.Model -> Browser.Document EC.Msg
view model =  case model.page of
                EC.Graph ->
                  GraphPage.view model
                EC.Home ->
                  HomePage.view model
                EC.Help ->
                  HelpPage.view model
                EC.Resources ->
                  ResourcesPage.view model


main : Program () EC.Model EC.Msg
main = Browser.application {
      init = \flags url key ->
                (
                    EC.initialModel key |> update (EC.ChangeUrl url) |> Tuple.first
                  , Task.perform
                    (
                      \vp -> EC.WindowResize (round vp.viewport.width) (round vp.viewport.height)
                    )
                    Dom.getViewport
                )
    , view = view
    , update = update
    , subscriptions = \model ->
                        Browser.Events.onResize EC.WindowResize
    , onUrlRequest = EC.RequestUrl
    , onUrlChange = EC.ChangeUrl
  }
