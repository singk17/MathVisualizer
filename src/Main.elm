module Main exposing (..)

-- #region Core Imports
import Browser
import Element as E
import Element.Border as Border
import Element.Font as Font
import Html as H
import Html.Attributes as A
import Time
import Task
import Browser.Dom as Dom
import Element.Background as Background
import Browser.Events
import Html.Events as Events
import GraphicSVG as S
import GraphicSVG.Widget as Widget
import Svg
import Svg.Attributes as SA
-- #endregion

-- #region My Imports
import Core as C
import Utils as U
import Element.Events exposing (onLoseFocus)
-- #endregion


-- #region ElmLoop Declarations
type alias Model = {
      matrixEntries : List MatrixEntry
    , vectorEntries : List VectorEntry
    , windowSize : (Int,Int)
    , effectiveMatrix : C.Matrix
  }

type Msg =  WindowResize Int Int
          | Tick Float

          | NewMatrixEntry
          | DeleteMatrixEntry Int
          | EditMatrixName Int String
          | EditMatrixFloat Int C.MatrixField Float

          | NewVectorEntry
          | DeleteVectorEntry Int
          | EditVectorName Int String
          | EditVectorFloat Int C.VectorField Float

          | UpdateEffective

          | Unused

type alias MatrixEntry = {
      name: Maybe String
    , matrix: Maybe C.Matrix
  }

type alias VectorEntry = {
      name: Maybe String
    , vector: Maybe C.Vector
    , plot: Bool
  }

emptyMatrixEntry = { name = Nothing, matrix = Nothing }
emptyVectorEntry = { name = Nothing, vector = Nothing, plot = False }

initialModel : Model
initialModel = {
      windowSize = (0,0)
    , matrixEntries = []
    , vectorEntries = []
    , effectiveMatrix = C.identityMatrix
  }

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
  Debug.log "UpdateOccurred" <| case msg of
    WindowResize w h ->
      ({ model | windowSize = Debug.log "WS" (w,h) }, Cmd.none)
    Tick t ->
      (model, Cmd.none)

    NewMatrixEntry ->
      { model | matrixEntries = model.matrixEntries ++ [emptyMatrixEntry] } |> update UpdateEffective
    DeleteMatrixEntry idx ->
      { model | matrixEntries = U.removeAt idx model.matrixEntries} |> update UpdateEffective
    EditMatrixName idx newName ->
      { model | matrixEntries = U.updateAt idx (\x -> { x | name = Just newName }) model.matrixEntries } |> update UpdateEffective
    EditMatrixFloat idx field val ->
      { model | matrixEntries = U.updateAt idx (\x -> { x | matrix = editMatrixField field val x.matrix }) model.matrixEntries } |> update UpdateEffective

    NewVectorEntry ->
      ({ model | vectorEntries = model.vectorEntries ++ [emptyVectorEntry] }, Cmd.none)
    DeleteVectorEntry idx ->
      ({ model | vectorEntries = U.removeAt idx model.vectorEntries }, Cmd.none)
    EditVectorName idx newName ->
      ({ model | vectorEntries =Debug.log "UVN" <| U.updateAt idx (\x -> { x | name = Just newName }) model.vectorEntries }, Cmd.none)
    EditVectorFloat idx field val ->
      ({ model | vectorEntries = Debug.log "UVF" <| U.updateAt idx (\x -> { x | vector = editVectorField field val x.vector }) model.vectorEntries }, Cmd.none)
    
    UpdateEffective ->
      let
        nEffec =  if (List.isEmpty model.matrixEntries) then
                    C.identityMatrix
                  else
                    case (U.actHead model.matrixEntries |> .matrix) of
                        Just m -> m
                        Nothing -> C.identityMatrix
      in
      ({ model | effectiveMatrix = nEffec }, Cmd.none)
    _ -> (model,Cmd.none)

-- #endregion

-- #region Styles
flex : H.Attribute msg
flex = A.style "display" "flex"

grid : H.Attribute msg
grid = A.style "display" "grid"

row = "row"
col = "column"
flexDirection : String -> H.Attribute msg
flexDirection d = A.style "flex-direction" d
type Units =    PX
              | VH
              | VW
              | PC
              | REM
              | EM

type Direction =    All
                  | Left
                  | Right
                  | Down
                  | Top

unity : Float -> Units -> String
unity s u =
  let
    un = case u of
          PX -> "px"
          VH -> "vh"
          VW -> "vw"
          PC -> "%"
          REM -> "rem"
          EM -> "em"
  in
  (String.fromFloat s) ++ un

numStyle : String -> Float -> Units -> H.Attribute Msg
numStyle t s u = unity s u |> A.style t

height : Float -> Units -> H.Attribute Msg
height s u = numStyle "height" s u

width : Float -> Units -> H.Attribute Msg
width s u = numStyle "width" s u

margin : Float -> Units -> H.Attribute Msg
margin s u = numStyle "margin" s u

marginLeft : Float -> Units -> H.Attribute Msg
marginLeft s u = numStyle "margin-left" s u

marginRight : Float -> Units -> H.Attribute Msg
marginRight s u = numStyle "margin-right" s u

marginTop : Float -> Units -> H.Attribute Msg
marginTop s u = numStyle "margin-top" s u

marginBottom : Float -> Units -> H.Attribute Msg
marginBottom s u = numStyle "margin-bottom" s u

padding : Float -> Units -> H.Attribute Msg
padding s u = numStyle "padding" s u

paddingLeft : Float -> Units -> H.Attribute Msg
paddingLeft s u = numStyle "padding-left" s u

paddingRight : Float -> Units -> H.Attribute Msg
paddingRight s u = numStyle "padding-right" s u

paddingTop : Float -> Units -> H.Attribute Msg
paddingTop s u = numStyle "padding-top" s u

paddingBottom : Float -> Units -> H.Attribute Msg
paddingBottom s u = numStyle "padding-bottom" s u

borderStyle : Float -> Units -> String -> String -> String
borderStyle s u t c = (unity s u) ++ " " ++ t ++ " " ++ c

border : Float -> Units -> String -> String -> H.Attribute Msg
border s u t c = borderStyle s u t c |> A.style "border"

borderTop : Float -> Units -> String -> String -> H.Attribute Msg
borderTop s u t c = borderStyle s u t c |> A.style "border-top"

borderBottom : Float -> Units -> String -> String -> H.Attribute Msg
borderBottom s u t c = borderStyle s u t c |> A.style "border-bottom"

borderLeft : Float -> Units -> String -> String -> H.Attribute Msg
borderLeft s u t c = borderStyle s u t c |> A.style "border-left"

borderRight : Float -> Units -> String -> String -> H.Attribute Msg
borderRight s u t c = borderStyle s u t c |> A.style "border-right"

solid = "solid"
dotted = "dotted"
dashed = "dashed"
black = "black"
red = "red"
blue = "blue"
yellow = "yellow"
-- #endregion Styles

-- #region main view

svgTitle = "Matrix Visualizer SVG Top"
vOutput = Widget.init 500 900 svgTitle
wModel = Tuple.first vOutput
sCmd = Tuple.second vOutput

matrixEntryNode : Int -> MatrixEntry -> H.Html Msg
matrixEntryNode idx ma =
  let
    wma = ma.matrix /= Nothing
    mat = case ma.matrix of
                      Just q -> q
                      Nothing -> ((0,0),(0,0))
    nma = ma.name /= Nothing
    na = case ma.name of
          Just q -> q
          Nothing -> ""
  in
  H.div [
      flex
    , flexDirection col
    , padding 5 PX
    , A.style "background" "#E9798D"
    , margin 5 PX
  ] [
      H.div [
          flex
        , flexDirection row
        , A.style "justify-content" "space-between"
      ] [
          H.input ([
              A.size 3
            , A.type_ "text"
            , A.placeholder "Name"
            , padding 4 PX
            , Events.onInput (EditMatrixName idx)
          ] ++ (if nma then [ A.value na ] else [])) []
        , H.button [ Events.onClick <| DeleteMatrixEntry idx ] [H.text "x"]
      ]
    , H.div [
          flex
        , flexDirection row
        , marginTop 4 PX
        , A.style "justify-content" "space-between"
      ] [
          H.div [
              width 10 PX
            , A.style "align-self" "strech"
            , borderTop 3 PX solid black
            , borderBottom 3 PX solid black
            , borderLeft 6 PX solid black
          ] []
        , H.div [
              margin 4 PX
            , grid
            , A.style "grid-template-columns" "40px 40px"
            , A.style "columns" "2"
            , A.style "column-gap" "10px"
          ] <| List.map
                (
                  \(n,f) ->
                    H.input ([
                        A.type_ "number"
                      , A.placeholder n
                      , Events.onInput (U.toFloat >> EditMatrixFloat idx f)
                    ] ++ (if wma then [ A.value <| String.fromFloat <| C.getMatrixField f mat ] else []))
                    []
                )
                [ ("a",C.A), ("b",C.B), ("c",C.C), ("d",C.D) ]
        ,  H.div [
              width 10 PX
            , A.style "align-self" "strech"
            , borderTop 3 PX solid black
            , borderBottom 3 PX solid black
            , borderRight 6 PX solid black
          ] []
      ]
  ]

vectorEntryNode : Int -> VectorEntry -> H.Html Msg
vectorEntryNode idx va =
  let
    nma = va.name /= Nothing |> Debug.log "nma"
    na = Debug.log "na" <| case va.name of
            Just q -> q
            Nothing -> ""
    vma = va.vector /= Nothing |> Debug.log "vma"
    vec = Debug.log "xy" <| case va.vector of
              Just q -> q
              Nothing -> (0,0)
  in
  H.div [
      flex
    , flexDirection col
    , padding 5 PX
    , A.style "background" "#27a1b2"
    , margin 5 PX
  ] [
      H.div [
          flex
        , flexDirection row
        , A.style "justify-content" "space-between"
      ] [
          H.input ([
              A.size 3
            , A.type_ "text"
            , A.placeholder "Name"
            , padding 4 PX
            , Events.onInput (EditVectorName idx)
          ] ++ (if nma then [ A.value na ] else [])) []
        , H.button [ Events.onClick <| DeleteVectorEntry idx ] [ H.text "x" ]
      ]
    , H.div [
              A.style "background" "#7ff0ff"
            , padding 4 PX
            , flex
            , flexDirection row
            , A.style "justify-content" "space-between"
            , A.style "align-self" "center"
            , marginTop 4 PX
            , width 85 PC
          ] [
              H.span [] [
                H.text "Plot"
              ]
            , H.input [
                  A.size 1
                , A.type_ "checkbox"
                , A.style "align-self" "bottom"
                , A.checked va.plot
              ] []
          ]
    , H.div [
          flex
        , flexDirection row
        , marginTop 4 PX
        , A.style "justify-content" "center"
      ] [
          H.div [
              width 10 PX
            , A.style "align-self" "strech"
            , borderTop 3 PX solid black
            , borderBottom 3 PX solid black
            , borderLeft 6 PX solid black
          ] []
        , H.div [
              margin 4 PX
            , grid
            , A.style "grid-template-columns" "40px"
            , A.style "columns" ""
            , A.style "column-gap" "10px"
          ] <| List.map
                (
                  \(f,n) ->
                    H.input ([
                        A.type_ "number"
                      , A.placeholder n
                      , Events.onInput (U.toFloat >> EditVectorFloat idx f)
                    ] ++ (if vma then [ A.value <| String.fromFloat <| C.getVectorField f vec ] else []))
                    []
                )
              [ (C.X,"x"), (C.Y,"y") ]
        ,  H.div [
              width 10 PX
            , A.style "align-self" "strech"
            , borderTop 3 PX solid black
            , borderBottom 3 PX solid black
            , borderRight 6 PX solid black
          ] []
      ]
  ]

gap = 30

arrow : S.Shape a
arrow = S.openPolygon [ (-gap,0), (0,0), (0,-gap) ] |> S.outlined (S.solid 4) S.red |> S.rotate (degrees <| -45)

createVectorEntries : List (VectorEntry) -> C.Matrix -> S.Shape a
createVectorEntries ves eff =
  let
    actualVectors = List.filter (.vector >> (/=) Nothing) ves
  in
  List.map
    (
      \v ->
        let
          vec = U.enforceJust v.vector
          scVec = C.vecscale gap vec |> C.matvecmul eff
          theta = case (C.toPolar scVec) of C.Polar r the -> the
        in
        S.group [
            S.line (0,0) scVec |> S.outlined (S.solid 4) S.red
          , arrow |> S.rotate theta |> S.move scVec
        ]
    )
  actualVectors
  |> S.group

createHorLine : Float -> C.Matrix -> Int -> S.Shape a
createHorLine yhalf eff pos =
  let
    x = toFloat pos |> (*) gap
    p1 = (x,-yhalf) |> C.matvecmul eff
    p2 = (x,yhalf) |> C.matvecmul eff
  in
  S.line p1 p2 |> S.outlined (S.solid 2) S.black

createVerticalLine : Float -> C.Matrix -> Int -> S.Shape a
createVerticalLine xhalf eff pos =
  let
    y = toFloat pos |> (*) gap
    p1 = (-xhalf,y) |> C.matvecmul eff
    p2 = (xhalf,y) |> C.matvecmul eff
  in
  S.line p1 p2 |> S.outlined (S.solid 2) S.black

graphView : Model -> List (S.Shape a)
graphView model =
  let
    (ww,wh) = model.windowSize
    (w,h) = (ww-330,wh)
    xHalf = Debug.log "xhalf" (w // 2)
    xUH = xHalf |> toFloat |> \x -> x / gap |> ceiling |> (+) 3
    xOHalf = xUH * gap |> toFloat
    yHalf = Debug.log "yhalf" (h // 2)
    yUH = yHalf |> toFloat |> \y -> y / gap |> ceiling |> (+) 3
    yOHalf = yUH * gap |> toFloat
  in
  [
      S.makeTransparent 0.4 <| S.group <| List.map
        (createHorLine yOHalf model.effectiveMatrix)
        <| List.range (-xUH) xUH
    , createHorLine yOHalf model.effectiveMatrix 0
    , S.makeTransparent 0.4 <| S.group <| List.map
        (createVerticalLine yOHalf model.effectiveMatrix)
        <| List.range (-yUH) yUH
    , createVerticalLine xOHalf model.effectiveMatrix 0
    , createVectorEntries model.vectorEntries model.effectiveMatrix
  ]

view : Model -> Browser.Document Msg
view model = {
      title = "Matrix Visualizer"
    , body =
      let
        (w,h) = model.windowSize
        (widgetModel,_) = Widget.init (toFloat <| w - 330) (toFloat h) svgTitle
      in
      [
          H.div [
                flex
              , flexDirection row
              , height 100 VH
              , width 100 VW
              , numStyle "maxWidth" 100 VW
              , A.style "overflow" "hidden"
          ] [
                -- Matrices 
                H.div [
                    A.style "width" "fit-content"
                  , A.style "background" "#ffc0cb"
                ] [
                    H.div [
                        flex
                      , flexDirection row
                      , padding 10 PX
                      , A.style "align-content" "center"
                      , A.style "align-items" "center"
                    ] [
                        H.h1 [] [
                            H.text "Matrices"
                        ]
                      , H.button [
                            height 20 PX
                          , marginLeft 10 PX
                          , Events.onClick NewMatrixEntry
                        ] [
                          H.text "+"
                        ]
                    ]
                  , H.div [
                        flex
                      , flexDirection col
                    ] <| List.indexedMap matrixEntryNode  model.matrixEntries
                ]
              
              -- Vectors
              ,  H.div [
                    A.style "width" "fit-content"
                  , A.style "background" "#7ff0ff"
                ] [
                    H.div [
                        flex
                      , flexDirection row
                      , padding 10 PX
                      , A.style "align-content" "center"
                      , A.style "align-items" "center"
                    ] [
                        H.h1 [] [
                            H.text "Vectors"
                        ]
                      , H.button [
                            height 20 PX
                          , marginLeft 10 PX
                          , Events.onClick NewVectorEntry
                        ] [
                          H.text "+"
                        ]
                    ]
                  , H.div [
                        flex
                      , flexDirection col
                    ] <| List.indexedMap vectorEntryNode model.vectorEntries 
                ]
              -- Graph
              , H.div [
                    -- A.style "flex-grow" "1"
                    height (toFloat h) PX
                  , width (toFloat <| w - 330) PX
                  , A.style "overflow" "visible"
                ] [
                  -- TODO the whole fucking graph
                  Widget.viewCustom Widget.defaultViewOption widgetModel (graphView model)
                ]
              -- Settings Stuff
              , H.div [
                    width 40 PX
                ] [
                    H.div [
                        flex
                      , A.style "justify-content" "center"
                      , marginTop 10 PX
                    ] [
                        H.button [
                          width 100 PC
                        ] [
                          H.text "<"
                        ]
                    ]
                ]
          ]
      ]
  }
-- #endregion

-- #region main declaration
main : Program () Model Msg
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
-- #endregion

