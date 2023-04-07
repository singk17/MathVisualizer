module Home exposing (..)

import Html as H
import Html.Attributes as A
import Browser

import GraphicSVG as S

import MVCore as EC
import Views exposing(..)

view : EC.Model -> Browser.Document EC.Msg
view model = {
      title = "Matrix Visualizer"
    , body =
      let
        (w_,h_) = model.windowSize
        (w,h) = (toFloat w_, toFloat h_)
        iS =  if h < 550 then
                40
              else
                100
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
              ] [
                    H.h1 [
                        A.style "color" "#0099cc"
                      , A.style "font-size" "8vh"
                      , A.style "text-align" "center"
                    ] [
                      H.text "Matrix Visualizer"
                    ]

                    , H.div [
                          flex
                        , flexDirection row
                        , A.style "justify-content" "center"
                      ] [
                        H.img [
                            A.src "https://cdn.discordapp.com/attachments/1039403273528037427/1093957649470587000/image.png"
                          , height 40 VH
                          , A.style "object-fit" "contain"
                        ] []
                      ]

                    , H.div [
                        flex
                      , flexDirection col
                      , A.style "justify-content" "center"
                      , A.style "flex" "1"
                      , A.style "align-content" "center"
                    ] [

                        H.div [
                            flex
                          , flexDirection row
                          , A.style "justify-content" "space-evenly"
                        ] [

                            H.div [
                                height 100 PC
                              , flex
                              , flexDirection col
                              , A.style "justify-content" "center"
                            ] [
                              H.a [
                                  width iS PX
                                , height iS PX
                                , A.style "cursor" "pointer"
                                , A.href "/help"
                                --, Events.onClick (EC.SwitchPage EC.Help)
                              ] [
                                  createIcon <| S.group <| [ S.circle 20 |> S.filled S.white |> S.addOutline (S.solid 3) S.pink, helpIcon S.white S.pink |> S.move (-1.3,0) ]
                              ]
                            ]

                          , H.div [
                              A.style "align-content" "center"
                              , flex
                              , flexDirection col
                              , A.style "justify-content" "center"
                            ] [
                                H.a [
                                  A.href "/graph"
                                ] [
                                  H.button [
                                      A.style "background-color" "#ff69b4"
                                    , A.style "border" "none"
                                    , A.style "color" "#ffffff"
                                    , ( if h > 550 then
                                          A.style "font-size" "17px"
                                        else
                                          A.style "font-size" "10px"
                                      )
                                    , ( if h > 550 then
                                          A.style "padding" "10px 50px"
                                        else
                                          A.style "padding" "2px 10px"
                                      )
                                    , margin 10 PX
                                    , numStyle "border-radius" 20 PX
                                    , A.style "cursor" "pointer"
                                  ] [
                                    H.h3 [
                                    ] [
                                      H.text "Start Graphing"
                                    ]
                                  ]
                              ]
                            ]

                          , H.div [
                                height 100 PC
                              , flex
                              , flexDirection col
                              , A.style "justify-content" "center"
                            ] [
                              H.div [
                                  width iS PX
                                , height iS PX
                                , A.style "cursor" "pointer"
                                --, Events.onClick (SwitchPage Resources)
                              ] [
                                  H.a [
                                      A.href "/resources"
                                    , A.style "text-decoration" "none"
                                    , A.style "color" "inherit"
                                  ] [
                                    createIcon <| S.group [ S.circle 20 |> S.filled S.white |> S.addOutline (S.solid 3) S.pink, iIcon S.pink |> S.move (0,-12) ]
                                  ]
                              ]
                            ]
                        ]
                    ]
              ]
          ]
      ]
  }