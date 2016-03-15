module Rabbit where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Time

-------------- Model --------------

type alias Model =
    { ai : AI
    , home : Coordinate
    , sheep : List Sheep
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
    { position : Coordinate
    , orientation : Orientation
    }

type alias Fence =
    { position : Coordinate
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
        NoOp    -> move model
        _       -> move model

move : Model -> Model
move model =
    let
        ( newAi, newScore ) = if model.ai.hasCargo then goHome model else findSheep model
        newSheep = List.map moveSheep model.sheep
    in
        { ai = newAi
        , home = model.home
        , sheep = newSheep
        , walls = model.walls
        , fences = model.fences
        , score = newScore
        }

goHome : Model -> ( AI, Score )
goHome model =
    if model.ai.position.x == model.home.x && model.ai.position.y == model.home.y
    then dropCargo model
    else moveCloserToHome model

dropCargo : Model -> ( AI, Score )
dropCargo model =
    ( { position = model.ai.position, hasCargo = False }, model.score + 1 )

moveCloserToHome : Model -> ( AI, Score )
moveCloserToHome model =
    let
        newPosition =   if model.position.x > model.position.y 
                        then { x = model.position.x - 1, y = model.position.y } 
                        else { x = model.position.x, y = model.position.y - 1 }
    in
        ( { position = newPosition, hasCargo = model.ai.hasCargo }, model.score )

findSheep : Model -> (AI, Score)
findSheep model =
    let
        --closestSheep = pickClosestSheep model.ai model.sheep
        --closestSheepPosition = closestSheep.position
        closestSheep = List.head model.sheep |> Maybe.withDefault { position = { x = 1, y = 1 }, moveDirection = Up }
        closestSheepPosition = closestSheep.position
        newPosition = moveToward model.ai closestSheepPosition  
    in
        ( { position = newPosition, hasCargo = model.ai.hasCargo }, model.score )

moveToward : AI -> Coordinate -> Coordinate
moveToward ai position =
    let
        newX = if ai.x == position.x then ai.x else moveSingleToward ai.x position.x
        newY = if ai.y == position.y then ai.y else moveSingleToward ai.y position.y
    in
        { x = newX, y = newY }

moveSingleToward : Int -> Int -> Int
moveSingleToward a b =
    if a > b then a - 1 else a + 1

--pickClosestSheep : AI -> List Sheep -> Coordinate
--pickClosestSheep ai sheep =
--    let
--        pickCloseTo = pickClosest ai
--    in
--        List.foldr pickCloseTo [] sheep

moveSheep : Sheep -> Sheep
moveSheep sheep = sheep

view : Signal.Address Action -> Model -> Html.Html
view address model =
    let
        subDivs = List.concat   [ drawElements model.walls wallColor
                                , drawElements model.fences fenceColor
                                , drawElements model.sheep sheepColor
                                , drawElement model.ai aiColor
                                , [ Html.div [ homeStyle ] [] ]
                                ]
    in
        Html.div 
            [ pageStyle ] 
            subDivs

--drawElements : List a -> String -> Html.Html
drawElements elements color =
    let
        drawColoredCell = drawCell color
    in
        List.map drawColoredCell elements

drawElement element color =
    [ drawCell color element ]

drawCell : String -> { b | position : { a | x : number, y : number' } } -> Html.Html
drawCell color element =
    let
        position = element.position
        leftMargin = cellSize * position.x |> toString
        leftMarginString = leftMargin ++ "px"
        topMargin = cellSize * position.y |> toString
        topMarginString = topMargin ++ "px"
        size = toString cellSize ++ "px"
        style = Attr.style 
                    [ ( "background-color", color )
                    , ( "position", "absolute" )
                    , ( "margin-left", leftMarginString )
                    , ( "margin-top", topMarginString )
                    , ( "width", size )
                    , ( "height", size )
                    ]
    in
        Html.div [ style ] []


-------------------- Initial Values ---------------

initialModel =
    { ai = initialAi
    , home = { x = 1, y = 1 }
    , sheep = initialSheep
    , walls = outerWalls
    , fences = []
    , score = 0
    }

outerWalls =
    [ { position = { x = 0, y = 0 }, orientation = BottomRight }
    , { position = { x = 1, y = 0 }, orientation = LeftRight }
    , { position = { x = 2, y = 0 }, orientation = LeftRight }
    , { position = { x = 3, y = 0 }, orientation = LeftRight }
    , { position = { x = 4, y = 0 }, orientation = LeftRight }
    , { position = { x = 5, y = 0 }, orientation = LeftRight }
    , { position = { x = 6, y = 0 }, orientation = LeftRight }
    , { position = { x = 7, y = 0 }, orientation = LeftRight }
    , { position = { x = 8, y = 0 }, orientation = LeftRight }
    , { position = { x = 9, y = 0 }, orientation = LeftRight }
    , { position = { x = 10, y = 0 }, orientation = LeftRight }
    , { position = { x = 11, y = 0 }, orientation = LeftRight }
    , { position = { x = 12, y = 0 }, orientation = LeftRight }
    , { position = { x = 13, y = 0 }, orientation = LeftRight }
    , { position = { x = 14, y = 0 }, orientation = LeftRight }
    , { position = { x = 15, y = 0 }, orientation = LeftRight }
    , { position = { x = 16, y = 0 }, orientation = LeftRight }
    , { position = { x = 17, y = 0 }, orientation = LeftRight }
    , { position = { x = 18, y = 0 }, orientation = LeftRight }
    , { position = { x = 19, y = 0 }, orientation = LeftBottom }
    , { position = { x = 19, y = 1 }, orientation = TopBottom }
    , { position = { x = 19, y = 2 }, orientation = TopBottom }
    , { position = { x = 19, y = 3 }, orientation = TopBottom }
    , { position = { x = 19, y = 4 }, orientation = TopBottom }
    , { position = { x = 19, y = 5 }, orientation = TopBottom }
    , { position = { x = 19, y = 6 }, orientation = TopBottom }
    , { position = { x = 19, y = 7 }, orientation = TopBottom }
    , { position = { x = 19, y = 8 }, orientation = TopBottom }
    , { position = { x = 19, y = 9 }, orientation = TopBottom }
    , { position = { x = 19, y = 10 }, orientation = TopBottom }
    , { position = { x = 19, y = 11 }, orientation = TopBottom }
    , { position = { x = 19, y = 12 }, orientation = TopBottom }
    , { position = { x = 19, y = 13 }, orientation = TopBottom }
    , { position = { x = 19, y = 14 }, orientation = TopBottom }
    , { position = { x = 19, y = 15 }, orientation = TopBottom }
    , { position = { x = 19, y = 16 }, orientation = TopBottom }
    , { position = { x = 19, y = 17 }, orientation = TopBottom }
    , { position = { x = 19, y = 18 }, orientation = TopBottom }
    , { position = { x = 19, y = 19 }, orientation = LeftTop }
    , { position = { x = 18, y = 19 }, orientation = LeftRight }
    , { position = { x = 17, y = 19 }, orientation = LeftRight }
    , { position = { x = 16, y = 19 }, orientation = LeftRight }
    , { position = { x = 15, y = 19 }, orientation = LeftRight }
    , { position = { x = 14, y = 19 }, orientation = LeftRight }
    , { position = { x = 13, y = 19 }, orientation = LeftRight }
    , { position = { x = 12, y = 19 }, orientation = LeftRight }
    , { position = { x = 11, y = 19 }, orientation = LeftRight }
    , { position = { x = 10, y = 19 }, orientation = LeftRight }
    , { position = { x = 9, y = 19 }, orientation = LeftRight }
    , { position = { x = 8, y = 19 }, orientation = LeftRight }
    , { position = { x = 7, y = 19 }, orientation = LeftRight }
    , { position = { x = 6, y = 19 }, orientation = LeftRight }
    , { position = { x = 5, y = 19 }, orientation = LeftRight }
    , { position = { x = 4, y = 19 }, orientation = LeftRight }
    , { position = { x = 3, y = 19 }, orientation = LeftRight }
    , { position = { x = 2, y = 19 }, orientation = LeftRight }
    , { position = { x = 1, y = 19 }, orientation = LeftRight }
    , { position = { x = 0, y = 19 }, orientation = TopRight }
    , { position = { x = 0, y = 18 }, orientation = TopBottom }
    , { position = { x = 0, y = 17 }, orientation = TopBottom }
    , { position = { x = 0, y = 16 }, orientation = TopBottom }
    , { position = { x = 0, y = 15 }, orientation = TopBottom }
    , { position = { x = 0, y = 14 }, orientation = TopBottom }
    , { position = { x = 0, y = 13 }, orientation = TopBottom }
    , { position = { x = 0, y = 12 }, orientation = TopBottom }
    , { position = { x = 0, y = 11 }, orientation = TopBottom }
    , { position = { x = 0, y = 10 }, orientation = TopBottom }
    , { position = { x = 0, y = 9 }, orientation = TopBottom }
    , { position = { x = 0, y = 8 }, orientation = TopBottom }
    , { position = { x = 0, y = 7 }, orientation = TopBottom }
    , { position = { x = 0, y = 6 }, orientation = TopBottom }
    , { position = { x = 0, y = 5 }, orientation = TopBottom }
    , { position = { x = 0, y = 4 }, orientation = TopBottom }
    , { position = { x = 0, y = 3 }, orientation = TopBottom }
    , { position = { x = 0, y = 2 }, orientation = TopBottom }
    , { position = { x = 0, y = 1 }, orientation = TopBottom }
    ]

initialAi = 
    { position = { x = 1, y = 1 }
    , hasCargo = False
    }

initialSheep =
    [ { position = { x = 9, y = 9 }, moveDirection = Right }
    , { position = { x = 9, y = 12 }, moveDirection = Up }
    , { position = { x = 12, y = 12 }, moveDirection = Left }
    , { position = { x = 12, y = 9 }, moveDirection = Down }
    ]

-------------------- CSS --------------------------

pageStyle =
    Attr.style
        [ ( "background-color", "chartreuse" )
        , ( "width", toString fieldSize ++ "px" )
        , ( "height", toString fieldSize ++ "px" )
        ]

homeStyle =
    let
        margin = toString cellSize ++ "px"
        size = toString cellSize ++ "px"
    in
        Attr.style
            [ ( "background-color", "silver" )
            , ( "position", "absolute" )
            , ( "margin-left", margin )
            , ( "margin-top", margin )
            , ( "width", size )
            , ( "height", size )
            ]

cellSize = 25
fieldSize = 20 * cellSize

wallColor = "black"
sheepColor = "white"
aiColor = "red"
fenceColor = "brown"
grassColor = "chartreuse"