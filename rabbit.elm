module Rabbit where

import Html
import Html.Attributes as Attr
import Html.Events as Events

-------------- Model --------------

type alias Model =
    { ai : AI
    , sheeps : List Sheep
    , field : Field
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

type alias Cell =
    { coordinate: Coordinate
    , fieldType: FieldType
    }

type FieldType = Empty | Blocked | ContainsSheep

type alias Coordinate = { x: Int, y: Int }

type alias Field =
    { cells: List Cell
    }

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

initialModel =
    { ai = initialAi
    , sheeps = []
    , field = emptyField
    , score = 0
    }

emptyField =
    { cells = [ cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8, cell9 ]
    }

initialAi = 
    { position = { x = 1, y = 1 }
    , hasCargo = False
    }

update : Action -> Model -> Model
update action model =
    case action of
        NoOp    -> model
        _       -> model

view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div [] []

cell1 =
    { coordinate = { x = 0, y = 0 }
    , fieldType = Blocked
    }

cell2 =
    { coordinate = { x = 1, y = 0 }
    , fieldType = Blocked
    }

cell3 =
    { coordinate = { x = 2, y = 0 }
    , fieldType = Blocked
    }

cell4 =
    { coordinate = { x = 0, y = 1 }
    , fieldType = Blocked
    }

cell5 =
    { coordinate = { x = 1, y = 1 }
    , fieldType = Empty
    }

cell6 =
    { coordinate = { x = 2, y = 1 }
    , fieldType = Blocked
    }

cell7 =
    { coordinate = { x = 0, y = 2 }
    , fieldType = Blocked
    }

cell8 =
    { coordinate = { x = 1, y = 2 }
    , fieldType = Blocked
    }

cell9 =
    { coordinate = { x = 2, y = 2 }
    , fieldType = Blocked
    }


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

