module Resources exposing (..)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as Events

import GraphicSVG as S

import MVCore exposing(..)
import Views exposing(..)

links : List (String,String)
links = [
      (
        "Introductions to transformations | Transformations | Geometry | Khan Academy",
        "https://www.youtube.com/watch?v=XiAoUDfrar0"
      )
    , (
        "Linear Transformations and matrices | Chapter 3, Essence of linear algebra",
        "https://www.youtube.com/watch?v=kYB8IZa5AuE"
      )
  ]


view : Model -> Browser.Document Msg
view model = {
      title = "Matrix Visualizer"
    , body = 
        let
          (w,h) = model.windowSize
          imgVH = if h > 750 then
                    40
                  else if h > 500 then
                    30
                  else
                    40
          iS =  if h > 750 then
                  100
                else if h > 500 then
                  70
                else
                  50
          ulPadd =  if h > 750 then
                      20
                    else if h > 500 then
                      15
                    else
                      10
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
                      H.text "Resources"
                    ]
                  
                  , H.img [
                        A.src "https://cdn.discordapp.com/attachments/1039403273528037427/1093952969436831904/main-qimg-aa6d589704a8b62b6d3170ea35bdf27b.png"
                      , height imgVH VH
                      , A.style "width" "auto"
                      , A.style "object-fit" "contain"
                    ] []


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
                        H.ul [
                            width 70 VW
                          , padding ulPadd PX
                        ]
                          <| List.map
                              (
                                \(t,l) ->
                                  H.li [] [
                                    H.a [
                                      A.href l
                                    ] [
                                      H.text t
                                    ]
                                  ]
                              )
                            links
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
