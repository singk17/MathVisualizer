module Help exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as Events
import Browser

import GraphicSVG as S

import Views exposing(..)
import MVCore exposing (..)

script : List String
script = [
    "Click on the Start Graphing button to launch the matrix visualizer.",
    "Use the + button to add a matrix/vector. You may click this button to add as many matrices and vectors you would like to visualize at a time. Use the x to delete matrices and vectors.",
    "Click on the blank boxes in the matrices and vectors to type in the desired values. You can also click the up and down arrows.",
    "For help with transformations, click on the ? on the side to access transformation",
    "Use on the + button to zoom in on the graph and - button to zoom out.",
    "Use the - button to minimize the icons on the side.",
    "Use the < button to maximize the icons on the side.",
    "Use the # button to make the grid appear or disappear.",
    "Use the HOME button to navigate to the home page."
  ]


view : Model -> Browser.Document Msg
view model = {
      title = "Matrix Visualizer"
    , body = 
      let
          (w,h) = model.windowSize
          iS =  if h > 750 then
                  100
                else if h > 500 then
                  70
                else
                  50
      in
      [
          H.div [
              A.style "background" "#95525E"
            , height 100 VH
            , width 100 VW
            , numStyle "maxWidth" 100 VW
            , A.style "overflow" "hidden"
            , flex
            , flexDirection col
          ] [
              H.div [
                    margin 20 PX
                  , A.style "background" "#F4F4F4"
                  , A.style "flex" "1"
                  , numStyle "border-radius" 10 PX
                  , flex
                  , flexDirection col
                  , A.style "justify-content" "space-between"
                ] [
                    H.h1 [
                        A.style "color" "#0099cc"
                      , A.style "font-size" "8vh"
                      , A.style "text-align" "center"
                    ] [
                      H.text "Help"
                    ]
                  
                  , H.div [
                      flex
                    , flexDirection row
                    , A.style "justify-content" "center"
                    , A.style "font-size" "2vh"

                    ] [
                      H.div [
                          A.style "border" "solid #0099cc 5px"
                        , numStyle "border-radius" 30 PX
                        , paddingLeft 20 PX
                        , paddingRight 20 PX

                      ] [
                        H.ol [
                            width 70 VW
                          , padding 20 PX
                          
                        ]
                          <| List.map
                              (
                                \t ->
                                  H.li [] [
                                    H.text t
                                  ]
                              )
                            script
                      ]
                    ]
                  , H.div [
                        flex
                      , flexDirection row
                      , A.style "justify-content" "end"
                    ] [
                        H.div [
                            width iS PX
                          , height iS PX
                          , Events.onClick (SwitchPage Home)
                          , A.style "cursor" "pointer"
                        ] [
                          createIcon <| S.group [ S.circle 20 |> S.filled S.white |> S.addOutline (S.solid 3) S.pink, homeIcon S.white S.pink ]
                        ]
                    ]
                ]
          ]
      ]
  }