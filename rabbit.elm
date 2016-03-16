module Rabbit where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Time
import Random

-------------- Model --------------

type alias Model =
    { ai : AI
    , home : Coordinate
    , sheep : List Sheep
    , walls : List Wall
    , fences : List Fence
    , score : Score
    , seed : Random.Seed
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

type alias Coordinate = ( Int, Int )

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
  Signal.foldp update initialModel ( Signal.sampleOn ticker actions.signal )

--ticker : Signal Time.Time
ticker = Time.every ( Time.second / 4 )

update : Action -> Model -> Model
update action model =
    case action of
        NoOp    -> move model
        _       -> move model

move : Model -> Model
move model =
    let
        ( newAi, newScore, newSheep ) = 
            if model.ai.hasCargo 
            then goHome model 
            else findSheep model
    in
        { ai = newAi
        , home = model.home
        , sheep = newSheep
        , walls = model.walls
        , fences = model.fences
        , score = newScore
        , seed = model.seed
        }

goHome : Model -> ( AI, Score, List Sheep )
goHome model =
    if model.ai.position == model.home
    then dropCargo model
    else moveCloserToHome model

dropCargo : Model -> ( AI, Score, List Sheep )
dropCargo model =
    let
        newSheep = List.map moveSheep model.sheep
    in
        ( { position = model.ai.position, hasCargo = False }, model.score + 1, newSheep )

moveCloserToHome : Model -> ( AI, Score, List Sheep )
moveCloserToHome model =
    let
        ( x, y ) = model.ai.position
        newPosition =   if x > y 
                        then ( x - 1, y )
                        else ( x, y - 1 )
        newSheep = List.map moveSheep model.sheep
    in
        ( { position = newPosition, hasCargo = model.ai.hasCargo }, model.score, newSheep )

findSheep : Model -> (AI, Score, List Sheep)
findSheep model =
    let
        closestSheep = pickClosestSheep model.ai model.sheep
        closestSheepPosition = closestSheep.position
        newPosition = moveToward model.ai.position closestSheepPosition
        ( newSheep, cargo ) = 
            if model.ai.position == closestSheepPosition 
            then pickUpSheep model.sheep closestSheep 
            else ( List.map moveSheep model.sheep, False )
    in
        if List.length model.sheep > 0
        then ( { position = newPosition, hasCargo = cargo }, model.score, newSheep )
        else ( model.ai, model.score, model.sheep )

pickClosestSheep : AI -> List Sheep -> Sheep
pickClosestSheep ai sheep =
    let
        comparer = compareAndSave ai.position
    in
        List.foldr comparer [] sheep |> List.head |> Maybe.withDefault { position = ( 999, 999 ), moveDirection = Left }

compareAndSave target sheep acc =
    let
        closest = List.head acc |> Maybe.withDefault { position = ( 999, 999 ), moveDirection = Left }
        distanceToCurrent = euclidianDistance target sheep.position
        distanceToSaved = euclidianDistance target closest.position
    in
        if distanceToCurrent < distanceToSaved
        then [ sheep ]
        else acc

distanceToSheep x sheep =
    euclidianDistance x sheep.position

pickUpSheep allSheep closest =
    ( List.filter ( (/=) closest ) allSheep, True )

moveToward : Coordinate -> Coordinate -> Coordinate
moveToward aiPosition position =
    let
        ( aiX, aiY ) = aiPosition
        ( x, y ) = position
        xDistance = aiX - x |> abs 
        yDistance = aiY - y |> abs
        newX = if aiX == x then aiX else moveSingleToward aiX x
        newY = if aiY == y then aiY else moveSingleToward aiY y
    in
        if xDistance > yDistance
        then ( ( moveSingleToward aiX x ), aiY )
        else ( aiX, ( moveSingleToward aiY y ) )

moveSingleToward : Int -> Int -> Int
moveSingleToward a b =
    if a > b then a - 1 else a + 1

--moveSheep : Model -> Sheep
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

drawCell color element =
    let
        ( x, y ) = element.position
        leftMargin = cellSize * x |> toString
        leftMarginString = leftMargin ++ "px"
        topMargin = cellSize * y |> toString
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

euclidianDistance : Coordinate -> Coordinate -> Float
euclidianDistance (p1, p2) (q1, q2) =
  (q1 - p1)^2 + (q2 - p2)^2 |> toFloat |> sqrt

-------------------- Initial Values ---------------

initialModel =
    { ai = initialAi
    , home = ( 1, 1 )
    , sheep = initialSheep
    , walls = outerWalls
    , fences = []
    , score = 0
    , seed = Random.initialSeed 42
    }

outerWalls =
    [ { position = ( 0, 0 ), orientation = BottomRight }
    , { position = ( 1, 0 ), orientation = LeftRight }
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
    , { position = ( 19, 0 ), orientation = LeftBottom }
    , { position = ( 19, 1 ), orientation = TopBottom }
    , { position = ( 19, 2 ), orientation = TopBottom }
    , { position = ( 19, 3 ), orientation = TopBottom }
    , { position = ( 19, 4 ), orientation = TopBottom }
    , { position = ( 19, 5 ), orientation = TopBottom }
    , { position = ( 19, 6 ), orientation = TopBottom }
    , { position = ( 19, 7 ), orientation = TopBottom }
    , { position = ( 19, 8 ), orientation = TopBottom }
    , { position = ( 19, 9 ), orientation = TopBottom }
    , { position = ( 19, 10 ), orientation = TopBottom }
    , { position = ( 19, 11 ), orientation = TopBottom }
    , { position = ( 19, 12 ), orientation = TopBottom }
    , { position = ( 19, 13 ), orientation = TopBottom }
    , { position = ( 19, 14 ), orientation = TopBottom }
    , { position = ( 19, 15 ), orientation = TopBottom }
    , { position = ( 19, 16 ), orientation = TopBottom }
    , { position = ( 19, 17 ), orientation = TopBottom }
    , { position = ( 19, 18 ), orientation = TopBottom }
    , { position = ( 19, 19 ), orientation = LeftTop }
    , { position = ( 18, 19 ), orientation = LeftRight }
    , { position = ( 17, 19 ), orientation = LeftRight }
    , { position = ( 16, 19 ), orientation = LeftRight }
    , { position = ( 15, 19 ), orientation = LeftRight }
    , { position = ( 14, 19 ), orientation = LeftRight }
    , { position = ( 13, 19 ), orientation = LeftRight }
    , { position = ( 12, 19 ), orientation = LeftRight }
    , { position = ( 11, 19 ), orientation = LeftRight }
    , { position = ( 10, 19 ), orientation = LeftRight }
    , { position = ( 9, 19 ), orientation = LeftRight }
    , { position = ( 8, 19 ), orientation = LeftRight }
    , { position = ( 7, 19 ), orientation = LeftRight }
    , { position = ( 6, 19 ), orientation = LeftRight }
    , { position = ( 5, 19 ), orientation = LeftRight }
    , { position = ( 4, 19 ), orientation = LeftRight }
    , { position = ( 3, 19 ), orientation = LeftRight }
    , { position = ( 2, 19 ), orientation = LeftRight }
    , { position = ( 1, 19 ), orientation = LeftRight }
    , { position = ( 0, 19 ), orientation = TopRight }
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

initialSheep =
    [ { position = ( 9, 9 ), moveDirection = Right }
    , { position = ( 9, 12 ), moveDirection = Up }
    , { position = ( 12, 12 ), moveDirection = Left }
    , { position = ( 12, 9 ), moveDirection = Down }
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