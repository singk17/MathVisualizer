module Main exposing (..)

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
import Browser.Events as Events
import GraphicSVG as Svg
import List exposing (all)
import String exposing (right)
-- import GraphicSVG.Widget as Widget


type alias Model = {}

type Msg =  WindowResize Int Int
          | Tick Float

initialModel : Model
initialModel = {}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    WindowResize w h ->
      (model, Cmd.none)
    Tick t ->
      (model, Cmd.none)

-- Styles
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
-- /end Styles

view : Model -> Browser.Document Msg
view model = {
      title = "Matrix Visualizer"
    , body = [
          H.div [
                flex
              , flexDirection row
              , height 100 VH
              , width 100 VW
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
                        ] [
                          H.text "+"
                        ]
                    ]
                  , H.div [
                        flex
                      , flexDirection col
                      , padding 5 PX
                      , A.style "background" "#E9798D"
                      , margin 5 PX
                    ] [
                        H.div [] [
                          H.input [
                              A.size 3
                            , A.type_ "text"
                            , A.placeholder "Name"
                          ] []
                        ]
                      , H.div [
                            flex
                          , flexDirection row
                          , marginTop 4 PX
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
                            ] [
                                  H.input [
                                      A.type_ "number"
                                    , A.placeholder "a"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "b"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "c"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "d"
                                  ] []
                            ]
                          ,  H.div [
                                width 10 PX
                              , A.style "align-self" "strech"
                              , borderTop 3 PX solid black
                              , borderBottom 3 PX solid black
                              , borderRight 6 PX solid black
                            ] []
                        ]
                    ]
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
                        ] [
                          H.text "+"
                        ]
                    ]
                  , H.div [
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
                            H.input [
                                A.size 3
                              , A.type_ "text"
                              , A.placeholder "Name"
                            ] []
                          , H.div [
                                A.style "background" "#7ff0ff"
                              , padding 4 PX
                            ] [
                                H.span [] [
                                  H.text "Plot"
                                ]
                              , H.input [
                                    A.size 1
                                  , A.type_ "checkbox"
                                  , A.style "align-self" "bottom"
                                ] []
                            ]
                        ]
                      , H.div [
                            flex
                          , flexDirection row
                          , marginTop 4 PX
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
                            ] [
                                  H.input [
                                      A.type_ "number"
                                    , A.placeholder "a"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "b"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "c"
                                  ] []
                                , H.input [
                                      A.type_ "number"
                                    , A.placeholder "d"
                                  ] []
                            ]
                          ,  H.div [
                                width 10 PX
                              , A.style "align-self" "strech"
                              , borderTop 3 PX solid black
                              , borderBottom 3 PX solid black
                              , borderRight 6 PX solid black
                            ] []
                        ]
                    ]
                ]
              -- Graph
              , H.div [
                    width 100 PC
                  , A.style "background" yellow
                ] [
                  -- TODO the whole fucking graph
                ]
              -- Settings Stuff
              , H.div [
                    width 40 PX
                  , A.style "background" yellow
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
    , subscriptions = \model -> Events.onResize WindowResize
  }