module Graph exposing (..)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as Events

import GraphicSVG as S
import GraphicSVG.Widget as Widget 

import Core as C
import MVCore as EC
import Views exposing(..)
import Utils as U

-- #region main view

svgTitle = "Matrix Visualizer SVG Top"
vOutput = Widget.init 500 900 svgTitle
wModel = Tuple.first vOutput
sCmd = Tuple.second vOutput

inputSectionWidth = 190
entryNodeWidth = 135
optionsBtnWidth = 40
notgraphsize = 2 * inputSectionWidth + optionsBtnWidth


matrixEntryNode : Int -> EC.MatrixEntry -> H.Html EC.Msg
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
            , Events.onInput (EC.EditMatrixName idx)
          ] ++ (if nma then [ A.value na ] else [])) []
        , H.button [ Events.onClick <| EC.DeleteMatrixEntry idx ] [H.text "x"]
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
                      , Events.onInput (U.toFloat >> EC.EditMatrixFloat idx f)
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

vectorEntryNode : Int -> EC.VectorEntry -> H.Html EC.Msg
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
            , Events.onInput (EC.EditVectorName idx)
          ] ++ (if nma then [ A.value na ] else [])) []
        , H.button [ Events.onClick <| EC.DeleteVectorEntry idx ] [ H.text "x" ]
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
                      , Events.onInput (U.toFloat >> EC.EditVectorFloat idx f)
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

createMatrixSelectionOptions : Int -> List EC.MatrixEntry -> List (H.Html EC.Msg)
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

createVectorEntries : EC.Model -> S.Shape a
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

graphView : EC.Model -> List (S.Shape a)
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
    mUH = U.actMax [ xUH, yUH ]
    mOHalf = U.actMax [ xOHalf, yOHalf ]
  in
  (
    if model.showGrid then
      [
          S.makeTransparent 0.4 <| S.group <| List.map
          (toFloat >> (*) model.gap >> createHorLine mOHalf model.effectiveMatrix)
          <| List.range (-mUH) mUH
        , S.makeTransparent 0.4 <| S.group <| List.map
          (toFloat >> (*) model.gap >> createVerticalLine mOHalf model.effectiveMatrix)
          <| List.range (-mUH) mUH
      ]
    else
      []
  ) ++
  [

      createHorLine yOHalf model.effectiveMatrix 0
    , createVerticalLine xOHalf model.effectiveMatrix 0
    , createVectorEntries model
  ]

view : EC.Model -> Browser.Document EC.Msg
view model = {
      title = "Matrix Visualizer"
    , body =
      let
        (w,h) = model.windowSize
        (widgetModel,_) = Widget.init (toFloat <| w - 330) (toFloat h) svgTitle
        toolsH =  if h > 500 then
                    440
                  else if h > 330 then
                    300
                  else if h > 280 then
                    250
                  else
                    180

        iS =  if h > 500 then
                50
              else if h > 330 then
                40
              else if h > 280 then
                35
              else
                25
        aTop =  if h > 500 then
                20
              else if h > 330 then
                13
              else if h > 20 then
                8
              else
                8

        aRight =  if h > 500 then
                    10
                  else if h > 330 then
                    8
                  else if h > 280 then
                    6
                  else
                    6
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
                                  , Events.onClick EC.NewMatrixEntry
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
                                  , Events.onClick EC.NewVectorEntry
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
                                  , Events.onInput (String.toInt >> U.enforceJust >> EC.UpdateEffective)
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
                                , numStyle "top" aTop PX
                                , numStyle "right" aRight PX
                                , width iS PX
                                , height toolsH PX
                                , flex
                                , flexDirection col
                                , A.style "justify-content" "space-between"
                                --, A.style "transform" <| "scale(" ++ (String.fromFloat toolsScale) ++ ")"
                              ]
                              [
                                  H.div [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    , Events.onClick EC.ToggleOptionsModal
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, collapseIcon S.blue ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    , Events.onClick EC.ZoomIn
                                  ] [
                                    createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, zoomInIcon S.blue |> S.scale 0.4 |> S.move (-7,4) ]
                                  ]
                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    , Events.onClick EC.ZoomOut
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, zoomOutIcon S.blue |> S.scale 0.4 |> S.move (-7,4) ]
                                  ]
                                , H.a [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    --, Events.onClick (SwitchPage Help)
                                    , A.href "/help"
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, helpIcon S.white S.blue |> S.move (-1,0) ]
                                  ]
                                , H.a [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    --, Events.onClick (SwitchPage Home)
                                    , A.href "/"
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, homeIcon S.white S.blue ]
                                  ]

                                , H.div [
                                      A.style "cursor" "pointer"
                                    , width iS PX
                                    , height iS PX
                                    , Events.onClick EC.ToggleGridView
                                  ] [
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, (if model.showGrid then gridIcon S.blue else noGridIcon S.red S.blue) ]
                                  ]
                              ]
                          ]
                        else
                          [
                            H.div [
                                      A.style "cursor" "pointer"
                                    , width (iS-15) PX
                                    , height (iS-15) PX
                                    , Events.onClick EC.ToggleOptionsModal
                                  ] [ 
                                      createIcon <| S.group [ S.circle 23 |> S.filled S.white |> S.addOutline (S.solid 2.2) S.blue, collapseIcon S.blue |> S.rotate (degrees 180) ]
                                  ]
                          ]
                      )
                ]
                -- #endregion
          ]
      ]
  }
