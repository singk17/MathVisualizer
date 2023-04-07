module Views exposing(..)

import Html as H
import Html.Attributes as A

import GraphicSVG as S
import GraphicSVG.Widget as Widget

import MVCore exposing(..)

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
      -- S.circle 18 |> S.filled colourOne
      S.circle 5 |> S.filled colourTwo |> S.scale 0.5 |> S.move (0,-10)
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
      --S.circle 18 |> S.filled colourOne
     S.square 10 |> S.filled colourTwo |> S.scale 1.5 |> S.move (0,-2)
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

iIcon : S.Color -> S.Shape a
iIcon color = S.group [
      S.circle 3 |> S.filled color |> S.move (0,22)
    , S.rectangle 4 20 |> S.filled color |> S.move (0,6)
  ]

collapseIcon : S.Color -> S.Shape a
collapseIcon color = S.group [
      S.openPolygon [ (sqrt 2 |> (*) -8, sqrt 2 |> (*) 8), (0,0), (sqrt 2 |> (*) -8, sqrt 2 |> (*) -8) ]
        |> S.outlined (S.solid 4) S.blue
        |> S.move (6,0)
--      S.rect 20 5 |> S.filled color |> S.rotate (degrees -45) |> S.move (0,5)
--    , S.rect 20 5 |> S.filled color |> S.rotate (degrees 45) |> S.move (0,-5)
  ]

iconModel = Widget.init 50 50 "IconTop" |> Tuple.first

createIcon : S.Shape Msg -> H.Html Msg
createIcon s = Widget.view iconModel [s]

iconStyles = [ width 25 PX, height 25 PX ]

-- #endregion
