module Core exposing (..)

type alias Vector = (Float,Float)
type alias Matrix = (Vector, Vector)


type MatrixField = A | B | C | D
type VectorField = X | Y

type Polar = Polar Float Float

identityMatrix : Matrix
identityMatrix = ((1,0),(0,1))

zeroVector : Vector
zeroVector = (0,0)

type Quad = I | II | III | IV | None | YU | YD | XL | XR

whatQuad : Vector -> Quad
whatQuad (x,y) =
  if (x == 0 && y == 0) then
    None
  else if (x == 0 && y > 0) then
    YU
  else if (x == 0 && y < 0) then
    YD
  else if (x > 0 && y == 0) then
    XR
  else if (x < 0 && y == 0) then
    XL
  else if (x > 0 && y > 0) then
    I
  else if (x < 0 && y > 0) then
    II
  else if (x < 0 && y < 0) then
    III
  else
    IV

toPolar : Vector -> Polar
toPolar (x,y) =
  let
    r = sqrt <| x ^ 2 + y ^ 2
    theta = y / x |> atan |> (+) (if x < 0 then pi else 0)
  in
  Polar r theta

setMatrixField : MatrixField -> Matrix -> Float -> Matrix
setMatrixField f ((a,c),(b,d)) v =
  case f of
    A -> ((v,c),(b,d))
    B -> ((a,c),(v,d))
    C -> ((a,v),(b,d))
    D -> ((a,c),(b,v))

getMatrixField : MatrixField -> Matrix -> Float
getMatrixField f ((a,c),(b,d)) =
  case f of
      A -> a
      B -> b
      C -> c
      D -> d

setVectorField : VectorField -> Vector -> Float -> Vector
setVectorField f (x,y) v =
  case f of
      X -> (v,y)
      Y -> (x,v)

getVectorField : VectorField -> Vector -> Float
getVectorField f (x,y) =
  case f of
      X -> x
      Y -> y

matrixI : Matrix -> Vector
matrixI (i,j) = i

matrixJ : Matrix -> Vector
matrixJ (i,j) = j

matmatmul : Matrix -> Matrix -> Matrix
matmatmul ((a,c),(b,d)) ((e,g),(f,h)) =
  ((e*a+g*b,f*a+h*b),(e*c+g*d,f*c+h*d))

matadd : Matrix -> Matrix -> Matrix
matadd ((a,c),(b,d)) ((e,g),(f,h)) =
  ((a+e,b+f),(c+g,d+h))

matscale : Float -> Matrix -> Matrix
matscale s ((a,c),(b,d)) = ((s*a,s*b),(s*c,s*d))

matvecmul : Matrix -> Vector -> Vector
matvecmul ((a,c),(b,d)) (x,y) =
  (a*x+b*y, c*x+d*y)

vecadd : Vector -> Vector -> Vector
vecadd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

vecsub : Vector -> Vector -> Vector
vecsub (x1,y1) (x2,y2) = (x1-x2,y1-y2)


vecscale : Float -> Vector -> Vector
vecscale s (x,y) = (s*x, s*y)

cRotMat : Float -> Matrix
cRotMat rad =
  let
    c = cos rad
    s = sin rad
  in
  ((c,-s), (s,c))

