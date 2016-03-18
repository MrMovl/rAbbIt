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

type Direction = RightUp | Right | RightDown | Down | LeftDown | Left | LeftUp | Up

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
        , seed = createNewSeed model.seed
        }

createNewSeed seed =
    let
        ( _, newSeed ) = Random.generate ( Random.int 0 10000 ) seed
    in
        newSeed

goHome : Model -> ( AI, Score, List Sheep )
goHome model =
    if model.ai.position == model.home
    then dropCargo model
    else moveCloserToHome model

dropCargo : Model -> ( AI, Score, List Sheep )
dropCargo model =
    let
        newSheep = randomSheepMover model.sheep model.seed
    in
        ( { position = model.ai.position, hasCargo = False }, model.score + 1, newSheep )

randomSheepMover sheep seed =
    let
        mover = moveSheep seed
    in
        List.map mover sheep


moveCloserToHome : Model -> ( AI, Score, List Sheep )
moveCloserToHome model =
    let
        ai = model.ai
        ( x, y ) = ai.position
        newPosition =   if x > y 
                        then ( x - 1, y )
                        else ( x, y - 1 )
        newSheep = randomSheepMover model.sheep model.seed
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
            else ( randomSheepMover model.sheep model.seed, False )
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

moveSheep : Random.Seed -> Sheep -> Sheep
moveSheep seed sheep = 
    let 
        ( changeDirection, nextSeed ) = Random.generate ( Random.int 0 10 ) seed
        direction = if changeDirection < 2 then createRandomDirection nextSeed else sheep.moveDirection
    in
        progressSheep sheep.position direction

progressSheep position direction =
    let
        ( x, y ) = position
        newPosition = case direction of
            Up          -> ( x    , y - 1 )
            RightUp     -> ( x + 1, y - 1 )
            Right       -> ( x + 1, y     )
            RightDown   -> ( x + 1, y + 1 )
            Down        -> ( x    , y + 1 )
            LeftDown    -> ( x - 1, y + 1 )
            Left        -> ( x - 1, y     )
            LeftUp      -> ( x - 1, y - 1 )
        correctedPosition = checkForCollision newPosition
    in
        Sheep newPosition direction

checkForCollision position =
    position |> horizontalCorrection |> verticalCorrection

horizontalCorrection ( x, y ) =
    let
        x1 = if x < 0 then 0 else x
        x2 = if x1 >= gridSize then gridSize else x1
    in
        ( x2, y )

verticalCorrection ( x, y ) =
    let
        y1 = if y < 0 then 0 else y
        y2 = if y1 >= gridSize then gridSize else y1
    in
        ( x, y2 )

createRandomDirection seed =
    let
        ( directionPair, _ ) = Random.generate randomDirectionPair seed
    in
        pairToDirection directionPair

createNewSheep seed =
    let
        ( spawnPoint, newSeed ) = Random.generate randomCoordinate seed
        ( directionPair, _ ) = Random.generate randomDirectionPair seed
        startingDirection = pairToDirection directionPair
    in
        Sheep ( spawnPoint ) ( startingDirection )

pairToDirection pair =
    case pair of
        (  0, -1 ) -> Up
        (  1, -1 ) -> RightUp
        (  1,  0 ) -> Right
        (  1,  1 ) -> RightDown
        (  0,  1 ) -> Down
        ( -1,  1 ) -> LeftDown
        ( -1,  0 ) -> Left
        ( -1, -1 ) -> LeftUp
        (  _,  _ ) -> RightDown

---------------------------- View --------------------------------------

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

-------------------- Helper ---------------

euclidianDistance : Coordinate -> Coordinate -> Float
euclidianDistance (p1, p2) (q1, q2) =
    (q1 - p1)^2 + (q2 - p2)^2 |> toFloat |> sqrt

randomCoordinate = Random.pair ( Random.int 0 24 ) ( Random.int 0 24 )

randomDirectionPair = Random.pair ( Random.int -1 1 ) ( Random.int -1 1 )

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
    , { position = ( 19, 0 ), orientation = LeftRight }
    , { position = ( 20, 0 ), orientation = LeftRight }
    , { position = ( 21, 0 ), orientation = LeftRight }
    , { position = ( 22, 0 ), orientation = LeftRight }
    , { position = ( 23, 0 ), orientation = LeftRight }
    , { position = ( 24, 0 ), orientation = LeftBottom }
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
    , { position = ( 24, 24 ), orientation = LeftTop }
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
    , { position = ( 0, 24 ), orientation = TopRight }
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

initialSheep =
    [ { position = ( 10, 10 ), moveDirection = LeftUp    }
    , { position = ( 11, 10 ), moveDirection = RightUp   }
    , { position = ( 11, 11 ), moveDirection = RightDown }
    , { position = ( 10, 11 ), moveDirection = LeftDown  }
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
fieldSize = 25 * cellSize
gridSize = 25

wallColor = "black"
sheepColor = "white"
aiColor = "red"
fenceColor = "brown"
grassColor = "chartreuse"