module Main exposing (..)

import Html
import Browser
import Element as E
import Element.Border as Border
import Element.Font as Font
import Time
import Task
import Browser.Dom as Dom
import Element.Background as Background
import Browser.Events as Browser
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import GraphicSVG.Widget as Widget

type alias Model =
    { time : Float
    , width : Int
    , height : Int
    , widgetModel : Widget.Model 
    , state : State
    , lines : List (Shape Msg)
    , forTable : List (String,String,String)
    , bubbles : List (String, (Color, (Float,Float)))
    }

type State
    = Animating     -- make up your own states
    | NotAnimating

type Msg
    = WindowResize Int Int -- update the size of the window
    | Tick Float

initW = Widget.init 300 100 "gsvgTop"

initialModel : Model
initialModel =
    { time = 0
    , width = 600
    , height = 1024
    , widgetModel = Tuple.first initW
    , state = NotAnimating
    , lines =   [ line (0,0) (20,20) |> outlined (solid 1) black
                , line (20,20) (40,10) |> outlined (solid 1) black
                ]
    , forTable = [("a","b","c"),("1","2","3")]
    , bubbles = [("Bob",(red,(0,0)))
                ,("Bill",(yellow,(20,20)))
                ,("Billi",(orange,(40,10)))
                ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ({ model | width = width, height = height }
            , Cmd.none)
        Tick t ->
            ({ model | time = t }, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    if model.width < 550 then
        phoneView model 
    else
        tabletView model 

phoneView : Model -> Browser.Document Msg
phoneView model =
        { title = "My Elm UI + Widget App"
        , body =    [E.layout []
                        (E.text "Hello phone!")
                    ]
        }

tabletView : Model -> Browser.Document Msg
tabletView model =
    let
        headerAttrs =
            [ Font.bold
            , Font.color <| E.rgb255 0 0 255
            , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , Border.color <| E.rgb255 0 0 255
            ]
    in
        { title = "My Elm UI + Widget App"
        , body = [E.layout
                    [Font.family
                        [ Font.monospace
                        ]
                    ]
                    (
                    E.column
                        [ E.centerX
                        , E.alignTop
                        , E.width E.fill
                        , E.spacing 10
                        , E.padding 5
                        , Border.width 2
                        , Border.rounded 6
                        , Border.color <| E.rgb255 0 0 255
                        ]
                        [ E.el [ E.width E.fill , E.height <| E.px (model.width // 3)]
                           <| E.row 
                            [ E.height E.fill]
                            [ E.html (Widget.view model.widgetModel (model.lines ++ List.map mkBubble model.bubbles))
                            ]
                        , E.el [ E.width E.fill ]
                           <| E.row [ E.width E.fill, E.height <| E.px 16]
                            [ E.el ((E.width <| E.px 52) :: headerAttrs) <| E.text "Col0"
                            , E.el ((E.width <| E.px 384) :: headerAttrs) <| E.text "Col1"
                            , E.el ((E.width <| E.fill) :: headerAttrs) <| E.text "Col2"
                            ]

                        -- workaround for a bug: it's necessary to wrap `table` in an `el`
                        -- to get table height attribute to apply
                        , E.el [ E.width E.fill ] <|
                            E.indexedTable
                                [ E.width E.fill
                                , E.height <| E.px ((model.height - model.width // 3) - 50)
                                , E.scrollbarY
                                , E.spacing 2
                                ]
                                { data = model.forTable
                                , columns =
                                    [   { header = E.none
                                        , width = E.px 50
                                        , view = \ idx (col1,_,_) ->
                                                    E.column
                                                        [ E.width (E.px 100)
                                                        , E.height <| E.px 40
                                                        , Background.color (clrs E.rgb255 idx)
                                                        ]
                                                        [E.text col1
                                                        ]
                                        }
                                    ,   { header = E.none
                                        , width = E.px 380
                                        , view =
                                                \ idx (_,col2,_) ->
                                                    E.column
                                                        [ E.width (E.fill)
                                                        , E.height <| E.px 40
                                                        , Background.color (clrs E.rgb255 idx)
                                                        ]
                                                        [ E.text col2
                                                        ]
                                        }
                                    ,   { header = E.none
                                        , width = E.fill
                                        , view =
                                                \ idx (_,_,col3) ->
                                                    E.column
                                                        [ E.width (E.px 100)
                                                        , E.height <| E.px 40
                                                        , Font.size 13
                                                        , Background.color (clrs E.rgb255 idx)
                                                        ]
                                                        [ E.text col3
                                                        ]
                                        }
                                    ]
                                }
                        ]
                    )
                ]
        }

mkBubble (key,(clr,pos)) = 
        [ roundedRect 18 7 3 |> filled clr |> move (0,1.75)
        , text key |> fixedwidth |> centered |> size 6 |> filled black
        ]
        |> group
        |> move pos

clrs mkClr idx = 
    case idx of 
        0 -> mkClr 0xff 0x83 0x89
        1 -> mkClr 0xff 0x7e 0xb6
        2 -> mkClr 0xbe 0x95 0xff
        3 -> mkClr 0x78 0xa9 0xff
        4 -> mkClr 0x33 0xb1 0xff
        5 -> mkClr 0x08 0xbd 0xba
        6 -> mkClr 0x42 0xbe 0x65
        7 -> mkClr 250 235 113
        _ -> if idx < 0 then clrs mkClr (-idx) else clrs mkClr (idx - 8)

main : Program () Model Msg
main =
    Browser.document
        { init = \ flags -> (initialModel, Task.perform ( \ vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height)) Dom.getViewport)
        , view = view
        , update = update
        , subscriptions = \ model -> case model.state of 
                                        NotAnimating -> Browser.onResize WindowResize
                                        Animating ->
                                            Sub.batch 
                                                [ Browser.onResize WindowResize
                                                , Browser.onAnimationFrame (Time.posixToMillis 
                                                                            >> toFloat 
                                                                            >> (\ t -> 0.001 * t ) 
                                                                            >> Tick)
                                                ]
        }
