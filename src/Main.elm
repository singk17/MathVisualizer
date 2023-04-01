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
    , effectiveIdx : Int
    , showOptions : Bool
    , showGrid : Bool
    , gap : Float
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

          | UpdateEffective Int

          | ToggleOptionsModal
          | ZoomIn
          | ZoomOut
          | GoToHome
          | GoToHelp
          | ToggleGridView

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
    , matrixEntries = [ emptyMatrixEntry ]
    , vectorEntries = [ emptyVectorEntry ]
    , effectiveMatrix = C.identityMatrix
    , effectiveIdx = -1

    , showOptions = True
    , showGrid = True
    , gap = 30
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
      ({ model | gap = U.clamp 1 100 <| model.gap + 5}, Cmd.none)
    ZoomOut ->
      ({ model | gap = U.clamp 1 100 <| model.gap - 5}, Cmd.none)
    ToggleGridView ->
      ({ model | showGrid = not model.showGrid }, Cmd.none)
    _ -> (model,Cmd.none)

-- #endregion

-- #region Styles

unselectableTags : List (H.Attribute msg)
unselectableTags = 
  A.style "-moz-user-select" "-moz-none" ::
  List.map
  (
    \x -> A.style x "none"
  )
  [
      "-khtml-user-select"
    , "-webkit-user-select"
    , "-o-user-select"
    , "user-select"
  ]

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

-- #region Icons
plusIcon : S.Color -> S.Shape a
plusIcon colour = 
    S.group [ S.rectangle 20 50 |> S.filled colour
    , S.rectangle 50 20 |> S.filled colour
    , S.circle 10 |> S.filled colour |> S.move (0,25)
    , S.circle 10 |> S.filled colour |> S.move (0,-25)
    , S.circle 10 |> S.filled colour |> S.move (25,0)
    , S.circle 10 |> S.filled colour |> S.move (-25,0)]

formulaIcon : S.Color -> S.Shape a
formulaIcon colour = S.group
  [
      S.roundedRect 25 23 2 |> S.outlined (S.solid 1) S.white
        |> S.move (0, 2)
    , S.rect 2 17 |> S.filled colour
        |> S.move (-3, 5)
    , S.rect 17 2 |> S.filled colour
      |> S.move (4.5,14)
    , S.rect 1 8 |> S.filled colour
        |> S.rotate -30
        |> S.move (-6, -3)
    , S.text "X"
        |> S.filled colour
  ]

helpIcon : S.Color -> S.Color -> S.Shape a
helpIcon colourOne colourTwo = S.group
  [ 
      S.circle 18 |> S.filled colourOne
    , S.circle 5 |> S.filled colourTwo |> S.scale 0.5 |> S.move (0,-10)
    , S.roundedRect 10 20 5 |> S.filled colourTwo |> S.scale 0.4 |> S.move (0,-2)
    , S.roundedRect 10 20 4 |> S.filled colourTwo |> S.scale 0.4 |> S.rotate (degrees -60) |> S.move (2,2)
    , S.roundedRect 10 20 4 |> S.filled colourTwo |> S.scale 0.4 |> S.rotate (degrees -30) |> S.move (6,6)
    , S.roundedRect 10 20 4 |> S.filled colourTwo |> S.scale 0.4 |> S.rotate (degrees 30) |> S.move (6,10)
    , S.roundedRect 10 20 4 |> S.filled colourTwo |> S.scale 0.4 |> S.rotate (degrees 90) |> S.move (2,12)
    , S.circle 5 |> S.filled colourTwo |> S.scale 0.5 |> S.move (-1,11)
  ]

zoomOutIcon : S.Color -> S.Shape a
zoomOutIcon colour =
  [
      S.circle 30
        |> S.filled colour
        |> S.move(20,-7)
    , S.roundedRect 10 23 5
        |> S.filled colour
        |> S.move(25,-30)
        |> S.rotate(degrees -45)

    , S.circle 25
        |> S.filled S.white
        |> S.move(20,-7)

    , S.roundedRect 10 30 5
        |> S.filled colour
        |> S.move(-10,-20)
        |> S.rotate(degrees 90)
  ] |> S.group

zoomInIcon : S.Color -> S.Shape a
zoomInIcon colour =
  [
      S.circle 30
        |> S.filled colour
        |> S.move(20,-7)
    , S.roundedRect 10 23 5
        |> S.filled colour
        |> S.move(25,-30)
        |> S.rotate(degrees -45)

    , S.circle 25
        |> S.filled S.white
        |> S.move(20,-7)

    , S.roundedRect 10 30 5
        |> S.filled colour
        |> S.move(-10,-20)
        |> S.rotate(degrees 90)
    
    , S.roundedRect 30 10 5
        |> S.filled colour
        |> S.move(-10,-20)
        |> S.rotate(degrees 90)
  ] |> S.group

deleteIcon : S.Color -> S.Shape a
deleteIcon colour = S.group
  [
      S.rect 17 25 |> S.outlined (S.solid 2) colour
    , S.rect 1 13 |> S.filled colour
        |> S.move (-3,0)
    , S.rect 1 13 |> S.filled colour
        |> S.move (3,0)
    , S.rect 17 2 |> S.filled colour
        |> S.rotate 25
        |> S.move (0,15.5)
    , S.rect 14 2 |> S.filled colour
        |> S.rotate 25
        |> S.move (0,16.5)
  ]

homeIcon : S.Color -> S.Color -> S.Shape a
homeIcon colourOne colourTwo = S.group
  [
      S.circle 18 |> S.filled colourOne
    , S.square 10 |> S.filled colourTwo |> S.scale 1.5 |> S.move (0,-2)
    , S.triangle 10 |> S.filled colourTwo |> S.scale 0.75 |> S.rotate (degrees 90) |> S.move (0,10) |> S.scaleY 0.7 |> S.scaleX 1.75
  ]

gridIcon : S.Color -> S.Shape a
gridIcon color = S.group [
      S.line (0,-10) (0,10) |> S.outlined (S.solid 1) color
    , S.line (5, -10) (5,10) |> S.outlined (S.solid 1) color
    , S.line (-5, -10) (-5,10) |> S.outlined (S.solid 1) color
    
    , S.line (-10,0) (10,0) |> S.outlined (S.solid 1) color
    , S.line (-10,5) (10,5) |> S.outlined (S.solid 1) color
    , S.line (-10,-5) (10,-5) |> S.outlined (S.solid 1) color
  ]

noGridIcon : S.Color -> S.Color -> S.Shape a
noGridIcon crcolor gcolor = S.group [
        gridIcon gcolor
      , S.line (-10,-10) (10,10) |> S.outlined (S.solid 4) crcolor
      , S.line (-10,10) (10,-10) |> S.outlined (S.solid 4) crcolor
  ]

{-
cross colour = plus colour |> rotate (degrees 45)

pluss c1 c2 = group [plus c1, plus c2 |> scaleX 0.9 |> scaleY 0.9]
crosss c1 c2 = group [cross c1, cross c2 |> scaleX 0.9 |> scaleY 0.9]
-}


-- #endregion

iconModel = Widget.init 50 50 "IconTop" |> Tuple.first

createIcon : S.Shape Msg -> H.Html Msg
createIcon s = Widget.view iconModel [s]

iconStyles = [ width 25 PX, height 25 PX ]

svgTitle = "Matrix Visualizer SVG Top"
vOutput = Widget.init 500 900 svgTitle
wModel = Tuple.first vOutput
sCmd = Tuple.second vOutput

inputSectionWidth = 190
entryNodeWidth = 135
optionsBtnWidth = 40
notgraphsize = 2 * inputSectionWidth + optionsBtnWidth


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
    , width entryNodeWidth PX
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
    nma = va.name /= Nothing
    na = case va.name of
            Just q -> q
            Nothing -> ""
    vma = va.vector /= Nothing
    vec = case va.vector of
              Just q -> q
              Nothing -> (0,0)
  in
  H.div [
      flex
    , flexDirection col
    , padding 5 PX
    , A.style "background" "#27a1b2"
    , margin 5 PX
    , width entryNodeWidth PX
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

createMatrixSelectionOptions : Int -> List MatrixEntry -> List (H.Html Msg)
createMatrixSelectionOptions selected entries = 
  List.indexedMap
    ( \idx v ->
        H.option [
            A.value <| String.fromInt idx
          , A.selected <| selected == idx
        ]
        [ H.text <| U.enforceJust v.name ]
    )
  <| List.filter (.name >> (/=) Nothing) <| List.filter (.matrix >> (/=) Nothing) entries

arrow : Float -> S.Shape a
arrow gap = S.openPolygon [ (-gap,0), (0,0), (0,-gap) ] |> S.outlined (S.solid 4) S.red |> S.rotate (degrees <| -45)

createVectorEntries : Model -> S.Shape a
createVectorEntries model =
  let
    actualVectors = List.filter (.vector >> (/=) Nothing) model.vectorEntries
  in
  List.map
    (
      \v ->
        let
          vec = U.enforceJust v.vector
          scVec = C.vecscale model.gap vec |> C.matvecmul model.effectiveMatrix
          theta = case (C.toPolar scVec) of C.Polar r the -> the
          nam = if v.name == Nothing then "" else U.enforceJust v.name
          head = if ((==) (0,0) <| scVec) then S.circle 6 |> S.filled S.red else arrow 10 |> S.rotate theta |> S.move scVec
        in
        S.group [
            S.line (0,0) scVec |> S.outlined (S.solid 4) S.red
          , head
          , S.text nam |> S.size  20|> S.filled S.red |> S.move scVec
        ]
    )
  actualVectors
  |> S.group

createHorLine : Float -> C.Matrix -> Float -> S.Shape a
createHorLine yhalf eff x =
  let
    p1 = (x,-yhalf) |> C.matvecmul eff
    p2 = (x,yhalf) |> C.matvecmul eff
  in
  S.line p1 p2 |> S.outlined (S.solid 2) S.black

createVerticalLine : Float -> C.Matrix -> Float -> S.Shape a
createVerticalLine xhalf eff y =
  let
    p1 = (-xhalf,y) |> C.matvecmul eff
    p2 = (xhalf,y) |> C.matvecmul eff
  in
  S.line p1 p2 |> S.outlined (S.solid 2) S.black

graphView : Model -> List (S.Shape a)
graphView model =
  let
    (ww,wh) = model.windowSize
    (w,h) = (ww - notgraphsize,wh)

    -- y axis S.lines
    ((a,c),(b,d)) = model.effectiveMatrix
    mx = c/a
    my = b/d

    stooo = (\x -> x ^ 2 ) <| U.actMax <| List.map abs [ a, b, c, d ]

    xHalf = (w // 2)
    fXHalf = xHalf |> toFloat
    xUH = xHalf |> toFloat |> \x -> x / model.gap |> (*) stooo |> ceiling |> (+) 30 |> Debug.log "NUMX"
    xOHalf = xUH |> toFloat |> (*) model.gap
    yHalf = (h // 2)
    fYHalf = toFloat yHalf
    yUH = yHalf |> toFloat |> \y -> y / model.gap |> (*) stooo |> ceiling |> (+) 30 |> Debug.log "NUMY"
    yOHalf = yUH |> toFloat |> (*) model.gap
  in
  (
    if model.showGrid then
      [
          S.makeTransparent 0.4 <| S.group <| List.map
          (toFloat >> (*) model.gap >> createHorLine yOHalf model.effectiveMatrix)
          <| List.range (-xUH) xUH
        , S.makeTransparent 0.4 <| S.group <| List.map
          (toFloat >> (*) model.gap >> createVerticalLine yOHalf model.effectiveMatrix)
          <| List.range (-yUH) yUH
      ]
    else
      []
  ) ++
  [

      createHorLine yOHalf model.effectiveMatrix 0
    , createVerticalLine xOHalf model.effectiveMatrix 0
    , createVectorEntries model
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
                -- #region Input stuff
                H.div [
                    flex
                  , flexDirection col
                ] [
                    H.div [
                        flex
                      , flexDirection row
                      , A.style "height" "calc(100vh - 150px)"
                    ] [
                        -- #region Matrix
                        H.div [
                            width inputSectionWidth PX
                          , A.style "max-height" "100%"
                            --A.style "width" "190px"
                          , A.style "background" "#ffc0cb"
                          , padding 10 PX
                          , flex
                          , flexDirection col
                        ] [
                            H.div [
                                flex
                              , flexDirection row
                              , padding 10 PX
                              , A.style "align-content" "center"
                              , A.style "align-items" "center"
                              , A.style "justify-content" "space-between"
                            ] [
                                H.h1 [] [
                                    H.text "Matrices"
                                ]
                              , H.button [
                                    height 20 PX
                                  , marginLeft 10 PX
                                  , Events.onClick NewMatrixEntry
                                ] [
                                  Widget.view iconModel [ plusIcon S.red ]
                                ]
                            ]
                          , H.div [
                                flex
                              , flexDirection col
                              , A.style "overflow-y" "auto"
                            ] <| List.indexedMap matrixEntryNode  model.matrixEntries
                        ]
                      -- #endregion

                      -- #region Vectors
                      ,  H.div [
                            width inputSectionWidth PX
                          , A.style "max-height" "100%"
                            --A.style "width" "fit-content"
                          , A.style "background" "#7ff0ff"
                          , padding 10 PX
                          , flex
                          , flexDirection col
                        ] [
                            H.div [
                                flex
                              , flexDirection row
                              , padding 10 PX
                              , A.style "align-content" "center"
                              , A.style "align-items" "center"
                              , A.style "justify-content" "space-between"
                            ] [
                                H.h1 [] [
                                    H.text "Vectors"
                                ]
                              , H.button [
                                    height 20 PX
                                  , marginLeft 10 PX
                                  , Events.onClick NewVectorEntry
                                ] [
                                  Widget.view iconModel [ plusIcon S.red ]
                                ]
                            ]
                          , H.div [
                                flex
                              , flexDirection col
                              , A.style "overflow-y" "auto"
                            ] <| List.indexedMap vectorEntryNode model.vectorEntries 
                        ]
                      -- #endregion
                    ]
                  , H.div [
                        A.style "background" "#ccaabb"
                      , width 100 PC
                      , A.style "max-width" "100%"
                      , A.style "flex" "1"
                    ] [
                        H.div ([
                            margin 10 PX
                          , flex
                          , flexDirection col
                          , A.style "justify-content" "center"
                          , A.style "align-items" "center"
                        ] ++ unselectableTags) [
                              H.h3 [ A.style "text-align" "center" ] [
                                H.text "Effective Matrix:"
                              ]

                            , H.div [] [
                                H.select [
                                    width 120 PX
                                  , Events.onInput (String.toInt >> U.enforceJust >> UpdateEffective)
                                  , A.style "font-size" "18px"
                                ]
                                <|  H.option [ A.value "-1" ] [ H.text "---" ]
                                    :: createMatrixSelectionOptions model.effectiveIdx model.matrixEntries
                              ]
                        ]
                    ]
                ]
                -- #endregion

                -- #region Graph
              , H.div [
                    -- A.style "flex-grow" "1"
                    height (toFloat h) PX
                  , width (toFloat <| w - notgraphsize) PX
                  , A.style "overflow" "visible"
                ] [
                  -- TODO the whole fucking graph
                  Widget.viewCustom Widget.defaultViewOption widgetModel (graphView model)
                ]
                -- #endregion

                -- #region Settings Stuff
              , H.div [
                    width optionsBtnWidth PX
                ] [
                    H.div [
                        flex
                      , A.style "justify-content" "center"
                      , marginTop 10 PX
                    ] (
                        if model.showOptions then
                          [
                              H.div [
                                  A.style "position" "fixed"
                                , A.style "top" "20px"
                                , A.style "right" "10px"
                                , width 50 PX
                                , height 440 PX
                                , flex
                                , flexDirection col
                                , A.style "justify-content" "space-between"
                              ]
                              [
                                  H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                    , Events.onClick ToggleOptionsModal
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, S.rect 30 5 |>  S.filled S.blue ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                    , Events.onClick ZoomIn
                                  ] [
                                    createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, zoomInIcon S.blue |> S.scale 0.4 |> S.move (-7,4) ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                    , Events.onClick ZoomOut
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, zoomOutIcon S.blue |> S.scale 0.4 |> S.move (-7,4) ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, helpIcon S.white S.blue |> S.move (-1,0) ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, homeIcon S.white S.blue ]
                                  ]

                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width 50 PX
                                    , height 50 PX
                                    , Events.onClick ToggleGridView
                                  ] [
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, (if model.showGrid then gridIcon S.blue else noGridIcon S.red S.blue) ]
                                  ]
                              ]
                          ]
                        else
                          [
                            H.button [
                                width 100 PC
                              , Events.onClick ToggleOptionsModal
                            ] [
                              H.text "<"
                            ]
                          ]
                      )
                ]
                -- #endregion
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

