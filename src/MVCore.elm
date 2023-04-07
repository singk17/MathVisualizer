module MVCore exposing (..)

import Browser.Navigation as Nav
import Url exposing (Url)
import Browser

import Core as C


type PageState =    Home
                  | Graph
                  | Resources
                  | Help

type alias Model = {
      matrixEntries : List MatrixEntry
    , vectorEntries : List VectorEntry
    , windowSize : (Int,Int)
    , effectiveMatrix : C.Matrix
    , effectiveIdx : Int
    , showOptions : Bool
    , showGrid : Bool
    , gap : Float
    , page: PageState


    , navKey : Nav.Key
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

          | SwitchPage PageState


          | ChangeUrl Url
          | RequestUrl Browser.UrlRequest

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

initialModel : Nav.Key -> Model
initialModel key = {
      windowSize = (0,0)
    , matrixEntries = [ emptyMatrixEntry ]
    , vectorEntries = [ emptyVectorEntry ]
    , effectiveMatrix = C.identityMatrix
    , effectiveIdx = -1

    , showOptions = True
    , showGrid = True
    , gap = 30
    , page = Resources

    , navKey = key
  }
