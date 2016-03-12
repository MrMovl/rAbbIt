module Rabbit where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Time

-------------- Model --------------

type alias Model =
    { ai : AI
    , sheeps : List Sheep
    , walls : List Wall
    , fences : List Fence
    , score : Score
    }

type alias AI =
    { position : Coordinate
    , hasCargo : Cargo
    }

type alias Sheep =
    { position: Coordinate
    , moveDirection: Direction
    }

type Direction = Left | Right | Up | Down

type alias Cargo = Bool

type alias Score = Int

type FieldType = Empty | Blocked | ContainsSheep

type alias Coordinate = { x: Int, y: Int }

type alias Wall =
    { coordinate : Coordinate
    , orientation : Orientation
    }

type alias Fence =
    { coordinate : Coordinate
    , orientation : Orientation
    , age : Time.Time
    }

type Orientation =
    TopBottom
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

type Action =
    NoOp
    | Start
    | Pause
    | PlaceBlock

-------------- Model --------------

main = Signal.map (view actions.address) model

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


update : Action -> Model -> Model
update action model =
    case action of
        NoOp    -> model
        _       -> model

view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div [] []

-------------------- Initial Values ---------------

initialModel =
    { ai = initialAi
    , sheeps = initialSheep
    , walls = outerWalls
    , fences = []
    , score = 0
    }

outerWalls =
    [ { coordinate = { x = 0, y = 0 }, orientation = BottomRight }
    , { coordinate = { x = 1, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 2, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 3, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 4, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 5, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 6, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 7, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 8, y = 0 }, orientation = LeftRight }
    , { coordinate = { x = 9, y = 0 }, orientation = LeftBottom }
    , { coordinate = { x = 9, y = 1 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 2 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 3 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 4 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 5 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 6 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 7 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 8 }, orientation = TopBottom }
    , { coordinate = { x = 9, y = 9 }, orientation = LeftTop }
    , { coordinate = { x = 8, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 7, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 6, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 5, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 4, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 3, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 2, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 1, y = 9 }, orientation = LeftRight }
    , { coordinate = { x = 0, y = 9 }, orientation = TopRight }
    , { coordinate = { x = 0, y = 8 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 7 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 6 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 5 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 4 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 3 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 2 }, orientation = TopBottom }
    , { coordinate = { x = 0, y = 1 }, orientation = TopBottom }
    ]

initialAi = 
    { position = { x = 1, y = 1 }
    , hasCargo = False
    }

initialSheep =
    [ { position = { x = 4, y = 4 }, moveDirection = Right }
    , { position = { x = 4, y = 5 }, moveDirection = Up }
    , { position = { x = 5, y = 5 }, moveDirection = Left }
    , { position = { x = 5, y = 4 }, moveDirection = Down }
    ]

-------------------- CSS --------------------------


cellStyle =
    Attr.style 
        [ ( "width", "50px" )
        , ( "height", "50px" )
        , ( "border", "1px dotted silver" )
        ]

blockedCellStyle =
    Attr.style
        [ ( "background-color", "black" )
        , ( "border", "1px solid black" )
        ]

emptyCellStyle =
    Attr.style
        [ ( "background-color", "chartreuse" )
        ]

aiCellStyle =
    Attr.style
        [ ( "background-color", "red" )
        ]

