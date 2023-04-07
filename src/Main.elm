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
import MVCore exposing(..)
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  let
    ti = Debug.log "Update MSG" msg
  in
  Debug.log "UpdateOccurred" <| case msg of
    WindowResize w h ->
      ({ model | windowSize = Debug.log "WS" (w,h) }, Cmd.none)
    Tick t ->
      (model, Cmd.none)

    NewMatrixEntry ->
      { model | matrixEntries = model.matrixEntries ++ [emptyMatrixEntry] } |> update (UpdateEffective model.effectiveIdx)
    DeleteMatrixEntry idx ->
      let
        newEffec =  if (idx == model.effectiveIdx) then
                      -1
                    else if (model.effectiveIdx < idx) then
                      model.effectiveIdx
                    else
                      model.effectiveIdx - 1
      in
      { model | matrixEntries = U.removeAt idx model.matrixEntries} |> update (UpdateEffective newEffec)
    EditMatrixName idx newName ->
      { model | matrixEntries = U.updateAt idx (\x -> { x | name = Just newName }) model.matrixEntries } |> update (UpdateEffective model.effectiveIdx)
    EditMatrixFloat idx field val ->
      { model | matrixEntries = U.updateAt  idx (\x -> { x | matrix = editMatrixField field val x.matrix }) model.matrixEntries } |> update (UpdateEffective model.effectiveIdx)

    NewVectorEntry ->
      ({ model | vectorEntries = model.vectorEntries ++ [emptyVectorEntry] }, Cmd.none)
    DeleteVectorEntry idx ->
      ({ model | vectorEntries = U.removeAt idx model.vectorEntries }, Cmd.none)
    EditVectorName idx newName ->
      ({ model | vectorEntries = U.updateAt idx (\x -> { x | name = Just newName }) model.vectorEntries }, Cmd.none)
    EditVectorFloat idx field val ->
      ({ model | vectorEntries = U.updateAt idx (\x -> { x | vector = editVectorField field val x.vector }) model.vectorEntries }, Cmd.none)
    
    UpdateEffective idx ->
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
    ToggleOptionsModal ->
      ({ model | showOptions = not model.showOptions }, Cmd.none)
    ZoomIn ->
      ({ model | gap = U.clamp 5 100 <| model.gap + 5}, Cmd.none) |> Debug.log "ZOOMIN"
    ZoomOut ->
      ({ model | gap = U.clamp 5 100 <| model.gap - 5}, Cmd.none) |> Debug.log "ZOOMOUT"
    ToggleGridView ->
      ({ model | showGrid = not model.showGrid }, Cmd.none)

    SwitchPage page ->
      let
        path = case page of
                Graph -> "/graph"
                Home -> "/"
                Resources -> "/resources"
                Help -> "/help"
      in
      -- ({ model | page = page }, Cmd.none)
      (model, Nav.pushUrl model.navKey <| path )


    ChangeUrl url ->
      case url.path of
        "/graph" -> ({ model | page = Graph }, Cmd.none)
        "/resources" -> ({ model | page = Resources }, Cmd.none)
        "/help" -> ({ model | page = Help }, Cmd.none)
        _ -> ({ model | page = Home }, Cmd.none)
    RequestUrl req ->
      case req of
        Browser.Internal url -> (model, Nav.pushUrl model.navKey <| url.path)
        Browser.External url -> (model, Nav.load url)

    _ -> (model,Cmd.none)


view : Model -> Browser.Document Msg
view model =  case model.page of
                Graph ->
                  GraphPage.view model
                Home ->
                  HomePage.view model
                Help ->
                  HelpPage.view model
                Resources ->
                  ResourcesPage.view model


main : Program () Model Msg
main = Browser.application {
      init = \flags url key ->
                (
                    initialModel key |> update (ChangeUrl url) |> Tuple.first
                  , Task.perform
                    (
                      \vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height)
                    )
                    Dom.getViewport
                )
    , view = view
    , update = update
    , subscriptions = \model ->
                        Browser.Events.onResize WindowResize
    , onUrlRequest = RequestUrl
    , onUrlChange = ChangeUrl
  }

{-
main = Browser.document
  {
      init = \_ -> (
        initialModel,
        Task.perform (
            \vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height)
        ) Dom.getViewport
      )
    , view = view
    , update = update
    , subscriptions = \model -> Browser.Events.onResize WindowResize
  }

-}