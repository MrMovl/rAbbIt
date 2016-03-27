module RabbitModel (..) where

import Html.Attributes as Attr
import Random


-------------------- CSS --------------------------


pageStyleAttribute =
  Attr.style pageStyle


pageStyle =
  [ ( "background-color", "chartreuse" )
  , ( "width", toString fieldSize ++ "px" )
  , ( "height", toString fieldSize ++ "px" )
  ]

instructionsStyle = 
  Attr.style 
    [ ("margin-left", toString (fieldSize + cellSize) ++ "px")
    , ("position", "absolute")
    , ("top", "0")
    ]

resultStyle =
  pageStyle
    ++ [ ( "opacity", "0.9" )
       , ( "font-size", "52" )
       , ( "text-align", "center" )
       , ( "padding-top", toString (fieldSize / 2) ++ "px" )
       ]
    |> Attr.style


homeStyle =
  let
    margin =
      toString cellSize ++ "px"

    size =
      toString cellSize ++ "px"
  in
    Attr.style
      [ ( "background-color", "silver" )
      , ( "position", "absolute" )
      , ( "margin-left", margin )
      , ( "margin-top", margin )
      , ( "width", size )
      , ( "height", size )
      ]



-------------- Model --------------


type alias Model =
  { ai : AI
  , home : Coordinate
  , rabbits : List Rabbit
  , walls : List Wall
  , fences : List Fence
  , score : Score
  , seed : Random.Seed
  , turns : Int
  , state : State
  }


type alias AI =
  { position : Coordinate
  , hasCargo : Cargo
  }


type alias Rabbit =
  { position : Coordinate
  , moveDirection : Direction
  }


type Direction
  = RightUp
  | Right
  | RightDown
  | Down
  | LeftDown
  | Left
  | LeftUp
  | Up


type alias Cargo =
  Bool


type alias Score =
  Int


type alias Coordinate =
  ( Int, Int )


type alias Wall =
  { position : Coordinate
  , orientation : Orientation
  }


type alias Fence =
  { position : Coordinate
  , orientation : Orientation
  , age : Int
  }


type Orientation
  = TopBottom
  | LeftRight
  | LeftTop
  | LeftBottom
  | TopRight
  | BottomRight
  | Cross
  | LeftTopRight
  | TopRightBottom
  | RightBottomLeft
  | BottomLeftTop


type State
  = Pause
  | End
  | Running


type alias Actions =
  ( ( Int, Int ), Bool, Bool )



---------------------- Constants -----------------------


cellSize =
  25

dimensions = 
  25


fieldSize =
  dimensions * cellSize


maxTurns =
  300


bunnyBounciness =
  2


fenceLifetime =
  200


fps =
  7


wallColor =
  "black"


rabbitColor =
  "white"


aiColor =
  "red"


scoreColor =
  "lawngreen"


fenceColor =
  "brown"


grassColor =
  "chartreuse"


winningString =
  "Yay, you won! These are your points: "



-------------------- Initial Values ---------------


port currentTime : Int
initialModel =
  { ai = initialAi
  , home = ( 1, 1 )
  , rabbits = initialRabbits
  , walls = outerWalls
  , fences = []
  , score = 0
  , seed = Random.initialSeed currentTime
  , turns = 0
  , state = Pause
  }

outerWalls = List.append corners straightWalls

corners =
  [ { position = ( 0, dimensions - 1 ), orientation = LeftBottom }
  , { position = ( dimensions - 1, 0 ), orientation = TopRight }
  , { position = ( dimensions - 1, dimensions - 1 ), orientation = LeftTop }
  , { position = ( 0, 0 ), orientation = BottomRight }
  ]


straightWalls =
  [ { position = ( 1, 0 ), orientation = LeftRight }
  , { position = ( 2, 0 ), orientation = LeftRight }
  , { position = ( 3, 0 ), orientation = LeftRight }
  , { position = ( 4, 0 ), orientation = LeftRight }
  , { position = ( 5, 0 ), orientation = LeftRight }
  , { position = ( 6, 0 ), orientation = LeftRight }
  , { position = ( 7, 0 ), orientation = LeftRight }
  , { position = ( 8, 0 ), orientation = LeftRight }
  , { position = ( 9, 0 ), orientation = LeftRight }
  , { position = ( 10, 0 ), orientation = LeftRight }
  , { position = ( 11, 0 ), orientation = LeftRight }
  , { position = ( 12, 0 ), orientation = LeftRight }
  , { position = ( 13, 0 ), orientation = LeftRight }
  , { position = ( 14, 0 ), orientation = LeftRight }
  , { position = ( 15, 0 ), orientation = LeftRight }
  , { position = ( 16, 0 ), orientation = LeftRight }
  , { position = ( 17, 0 ), orientation = LeftRight }
  , { position = ( 18, 0 ), orientation = LeftRight }
  , { position = ( 19, 0 ), orientation = LeftRight }
  , { position = ( 20, 0 ), orientation = LeftRight }
  , { position = ( 21, 0 ), orientation = LeftRight }
  , { position = ( 22, 0 ), orientation = LeftRight }
  , { position = ( 23, 0 ), orientation = LeftRight }
  , { position = ( 24, 1 ), orientation = TopBottom }
  , { position = ( 24, 2 ), orientation = TopBottom }
  , { position = ( 24, 3 ), orientation = TopBottom }
  , { position = ( 24, 4 ), orientation = TopBottom }
  , { position = ( 24, 5 ), orientation = TopBottom }
  , { position = ( 24, 6 ), orientation = TopBottom }
  , { position = ( 24, 7 ), orientation = TopBottom }
  , { position = ( 24, 8 ), orientation = TopBottom }
  , { position = ( 24, 9 ), orientation = TopBottom }
  , { position = ( 24, 10 ), orientation = TopBottom }
  , { position = ( 24, 11 ), orientation = TopBottom }
  , { position = ( 24, 12 ), orientation = TopBottom }
  , { position = ( 24, 13 ), orientation = TopBottom }
  , { position = ( 24, 14 ), orientation = TopBottom }
  , { position = ( 24, 15 ), orientation = TopBottom }
  , { position = ( 24, 16 ), orientation = TopBottom }
  , { position = ( 24, 17 ), orientation = TopBottom }
  , { position = ( 24, 18 ), orientation = TopBottom }
  , { position = ( 24, 19 ), orientation = TopBottom }
  , { position = ( 24, 20 ), orientation = TopBottom }
  , { position = ( 24, 21 ), orientation = TopBottom }
  , { position = ( 24, 22 ), orientation = TopBottom }
  , { position = ( 24, 23 ), orientation = TopBottom }
  , { position = ( 23, 24 ), orientation = LeftRight }
  , { position = ( 22, 24 ), orientation = LeftRight }
  , { position = ( 21, 24 ), orientation = LeftRight }
  , { position = ( 20, 24 ), orientation = LeftRight }
  , { position = ( 19, 24 ), orientation = LeftRight }
  , { position = ( 18, 24 ), orientation = LeftRight }
  , { position = ( 17, 24 ), orientation = LeftRight }
  , { position = ( 16, 24 ), orientation = LeftRight }
  , { position = ( 15, 24 ), orientation = LeftRight }
  , { position = ( 14, 24 ), orientation = LeftRight }
  , { position = ( 13, 24 ), orientation = LeftRight }
  , { position = ( 12, 24 ), orientation = LeftRight }
  , { position = ( 11, 24 ), orientation = LeftRight }
  , { position = ( 10, 24 ), orientation = LeftRight }
  , { position = ( 9, 24 ), orientation = LeftRight }
  , { position = ( 8, 24 ), orientation = LeftRight }
  , { position = ( 7, 24 ), orientation = LeftRight }
  , { position = ( 6, 24 ), orientation = LeftRight }
  , { position = ( 5, 24 ), orientation = LeftRight }
  , { position = ( 4, 24 ), orientation = LeftRight }
  , { position = ( 3, 24 ), orientation = LeftRight }
  , { position = ( 2, 24 ), orientation = LeftRight }
  , { position = ( 1, 24 ), orientation = LeftRight }
  , { position = ( 0, 23 ), orientation = TopBottom }
  , { position = ( 0, 22 ), orientation = TopBottom }
  , { position = ( 0, 21 ), orientation = TopBottom }
  , { position = ( 0, 20 ), orientation = TopBottom }
  , { position = ( 0, 19 ), orientation = TopBottom }
  , { position = ( 0, 18 ), orientation = TopBottom }
  , { position = ( 0, 17 ), orientation = TopBottom }
  , { position = ( 0, 16 ), orientation = TopBottom }
  , { position = ( 0, 15 ), orientation = TopBottom }
  , { position = ( 0, 14 ), orientation = TopBottom }
  , { position = ( 0, 13 ), orientation = TopBottom }
  , { position = ( 0, 12 ), orientation = TopBottom }
  , { position = ( 0, 11 ), orientation = TopBottom }
  , { position = ( 0, 10 ), orientation = TopBottom }
  , { position = ( 0, 9 ), orientation = TopBottom }
  , { position = ( 0, 8 ), orientation = TopBottom }
  , { position = ( 0, 7 ), orientation = TopBottom }
  , { position = ( 0, 6 ), orientation = TopBottom }
  , { position = ( 0, 5 ), orientation = TopBottom }
  , { position = ( 0, 4 ), orientation = TopBottom }
  , { position = ( 0, 3 ), orientation = TopBottom }
  , { position = ( 0, 2 ), orientation = TopBottom }
  , { position = ( 0, 1 ), orientation = TopBottom }
  ]


initialAi =
  { position = ( 1, 1 )
  , hasCargo = False
  }


initialRabbits =
  [ { position = ( 10, 10 ), moveDirection = LeftUp }
  , { position = ( 11, 10 ), moveDirection = RightUp }
  , { position = ( 11, 11 ), moveDirection = RightDown }
  , { position = ( 10, 11 ), moveDirection = LeftDown }
  ]
