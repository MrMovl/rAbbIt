module Rabbit (..) where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Time
import Random
import Keyboard
import Mouse

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

type alias Actions = ((Int, Int), Bool, Bool)



-------------- Model --------------

-- We only need this for the initial randomicity seed
port currentTime : Int

main =
  Signal.map view model


model : Signal Model
model =
  Signal.foldp update initialModel (Signal.map3 (,,) (Signal.sampleOn (Time.fps 10) Mouse.position) Mouse.isDown Keyboard.space)


update : Actions -> Model -> Model
update actions model =
  let
    (_, _, space) = actions
  in
    case model.state of
      Pause -> if space then checkWinSituation model actions else model
      End -> if space then checkWinSituation initialModel actions else model
      Running -> if space then pauseGame model else checkWinSituation model actions


pauseGame : Model -> Model
pauseGame model = Model model.ai model.home model.rabbits model.walls model.fences model.score model.seed model.turns Pause


checkWinSituation : Model -> Actions -> Model
checkWinSituation model actions =
  let
    newState = if model.turns > maxTurns then End else Running
  in
    case newState of
      Running -> move model actions
      End -> Model model.ai model.home model.rabbits model.walls model.fences model.score model.seed model.turns End
      Pause -> model


move : Model -> Actions -> Model
move model actions =
  let
    (_, _, space) = actions
    ( newAi, newScore, newRabbits, newFences ) =
      if model.ai.hasCargo then
        goHome model actions
      else
        findRabbit model actions
  in
    { ai = newAi
    , home = model.home
    , rabbits = newRabbits
    , walls = model.walls
    , fences = newFences
    , score = newScore
    , seed = createNewSeed model.seed
    , turns = model.turns + 1
    , state = Running
    }


createNewSeed seed =
  let
    ( _, newSeed ) =
      Random.generate (Random.int 0 42) seed
  in
    newSeed


goHome : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
goHome model actions =
  if model.ai.position == model.home then
    dropCargo model actions
  else
    moveCloserToHome model actions


addFence : List Fence -> Actions -> List Fence
addFence fences ((x, y), mouseClicked, _) =
  let
    newFences = if mouseClicked then Fence (x, y) TopBottom 10 :: fences else fences
  in
    ageFences newFences

ageFences : List Fence -> List Fence
ageFences fences =
  List.map ageSingleFence fences |> List.filter (\fence -> fence.age > 0)

ageSingleFence : Fence -> Fence
ageSingleFence {position, orientation, age} =
  Fence position orientation (age - 1)

dropCargo : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
dropCargo model actions =
  let
    newFences = addFence model.fences actions

    newbornRabbit =
      createNewRabbit model.seed

    newRabbits =
      moveRabbits model.rabbits model.seed |> (::) newbornRabbit

    newScore =
      model.score + 1
  in
    ( { position = model.ai.position, hasCargo = False }, newScore, newRabbits, newFences )


moveRabbits : List Rabbit -> Random.Seed -> List Rabbit
moveRabbits rabbits seed =
  randomRabbitMover rabbits seed |> List.map progressRabbit


randomRabbitMover : List Rabbit -> Random.Seed -> List Rabbit
randomRabbitMover rabbits seed =
  let
    positions =
      List.map .position rabbits

    ( newDirections, secondSeed ) =
      Random.generate (directionListCreator (List.length rabbits)) seed

    newRabbits =
      List.map pairToDirection newDirections |> List.map2 Rabbit positions

    ( changeDirectionList, _ ) =
      Random.generate (Random.list (List.length rabbits) (Random.int 0 10)) secondSeed
  in
    List.map3 (,,) changeDirectionList newRabbits rabbits |> List.map pickRabbit


pickRabbit : ( Int, Rabbit, Rabbit ) -> Rabbit
pickRabbit ( x, newRabbit, oldRabbit ) =
  if x < volatilityThreshold then
    newRabbit
  else
    oldRabbit

progressRabbit : Rabbit -> Rabbit
progressRabbit rabbit =
  let
    ( x, y ) =
      rabbit.position

    newPosition =
      case rabbit.moveDirection of
        Up ->
          ( x, y - 1 )

        RightUp ->
          ( x + 1, y - 1 )

        Right ->
          ( x + 1, y )

        RightDown ->
          ( x + 1, y + 1 )

        Down ->
          ( x, y + 1 )

        LeftDown ->
          ( x - 1, y + 1 )

        Left ->
          ( x - 1, y )

        LeftUp ->
          ( x - 1, y - 1 )

    correctedPosition =
      checkForCollision newPosition
  in
    Rabbit correctedPosition rabbit.moveDirection


checkForCollision : Coordinate -> Coordinate
checkForCollision position =
  position |> horizontalCorrection |> verticalCorrection


horizontalCorrection : Coordinate -> Coordinate
horizontalCorrection ( x, y ) =
  let
    x1 =
      if x < 1 then
        1
      else
        x

    x2 =
      if x1 >= gridSize - 1 then
        gridSize - 2
      else
        x1
  in
    ( x2, y )


verticalCorrection : Coordinate -> Coordinate
verticalCorrection ( x, y ) =
  let
    y1 =
      if y < 1 then
        1
      else
        y

    y2 =
      if y1 >= gridSize - 1 then
        gridSize - 2
      else
        y1
  in
    ( x, y2 )


directionListCreator count =
  randomDirectionPairGenerator |> Random.list count


moveCloserToHome : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
moveCloserToHome model actions =
  let
    newFences = addFence model.fences actions

    ( x, y ) =
      model.ai.position

    newPosition =
      if x > y then
        ( x - 1, y )
      else
        ( x, y - 1 )

    newRabbits =
      moveRabbits model.rabbits model.seed
  in
    ( { position = newPosition, hasCargo = model.ai.hasCargo }, model.score, newRabbits, newFences )


findRabbit : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
findRabbit model actions =
  let
    newFences = addFence model.fences actions

    closestRabbit =
      pickClosestRabbit model.ai model.rabbits

    closestRabbitPosition =
      closestRabbit.position

    newAiPosition =
      moveToward model.ai.position closestRabbitPosition

    ( updatedPosition, (newRabbits, cargo) ) =
      if model.ai.position == closestRabbitPosition then
        ( model.ai.position, (pickUpRabbit model.rabbits closestRabbit))
      else
        ( newAiPosition, (moveRabbits model.rabbits model.seed, False) )
  in
    ( { position = updatedPosition, hasCargo = cargo }, model.score, newRabbits, newFences )




pickClosestRabbit : AI -> List Rabbit -> Rabbit
pickClosestRabbit ai rabbits =
  let
    comparer =
      compareAndSave ai.position
  in
    List.foldr comparer [] rabbits |> List.head |> Maybe.withDefault { position = ( 999, 999 ), moveDirection = Left }


compareAndSave : Coordinate -> Rabbit -> List Rabbit -> List Rabbit
compareAndSave target rabbit acc =
  let
    closest =
      List.head acc |> Maybe.withDefault { position = ( 999, 999 ), moveDirection = Left }

    distanceToCurrent =
      euclidianDistance target rabbit.position

    distanceToSaved =
      euclidianDistance target closest.position
  in
    if distanceToCurrent < distanceToSaved then
      [ rabbit ]
    else
      acc


pickUpRabbit : List Rabbit -> Rabbit -> ( List Rabbit, Bool )
pickUpRabbit rabbits closest =
  ( List.filter ((/=) closest) rabbits, True )


moveToward : Coordinate -> Coordinate -> Coordinate
moveToward aiPosition position =
  let
    ( aiX, aiY ) =
      aiPosition

    ( x, y ) =
      position

    xDistance =
      aiX - x |> abs

    yDistance =
      aiY - y |> abs

    newX =
      if aiX == x then
        aiX
      else
        moveSingleToward aiX x

    newY =
      if aiY == y then
        aiY
      else
        moveSingleToward aiY y
  in
    if xDistance > yDistance then
      ( (moveSingleToward aiX x), aiY )
    else
      ( aiX, (moveSingleToward aiY y) )


moveSingleToward : Int -> Int -> Int
moveSingleToward a b =
  if a > b then
    a - 1
  else
    a + 1


createRandomDirection : Random.Seed -> Direction
createRandomDirection seed =
  let
    ( directionPair, _ ) =
      Random.generate randomDirectionPairGenerator seed
  in
    pairToDirection directionPair

createNewRabbit : Random.Seed -> Rabbit
createNewRabbit seed =
  let
    ( spawnPoint, newSeed ) =
      Random.generate randomCoordinateGenerator seed

    ( directionPair, _ ) =
      Random.generate randomDirectionPairGenerator seed

    startingDirection =
      pairToDirection directionPair
  in
    Rabbit (spawnPoint) (startingDirection)


pairToDirection : (Int, Int) -> Direction
pairToDirection pair =
  case pair of
    ( 0, -1 ) ->
      Up

    ( 1, -1 ) ->
      RightUp

    ( 1, 0 ) ->
      Right

    ( 1, 1 ) ->
      RightDown

    ( 0, 1 ) ->
      Down

    ( -1, 1 ) ->
      LeftDown

    ( -1, 0 ) ->
      Left

    ( -1, -1 ) ->
      LeftUp

    ( _, _ ) ->
      RightDown



---------------------------- View --------------------------------------


view : Model -> Html.Html
view model =
  if model.turns > maxTurns then
    drawEnd model.score
  else
    drawScene model


drawEnd score =
  Html.div [ resultScreen ] [ toString score |> (++) winningString |> Html.text ]


drawScene model =
  let
    subDivs =
      List.concat
        [ toString model.score |> drawScore
        , drawElements model.walls wallColor
        , drawElements model.fences fenceColor
        , drawElements model.rabbits rabbitColor
        , drawElement model.ai aiColor
        , [ Html.div [ homeStyle ] [] ]
        ]
  in
    Html.div
      [ pageStyleAttribute ]
      subDivs


drawScore score =
  let
    size =
      toString cellSize ++ "px"

    style =
      Attr.style
        [ ( "background-color", scoreColor )
        , ( "position", "absolute" )
        , ( "margin-left", "0px" )
        , ( "margin-top", "0px" )
        , ( "width", size )
        , ( "height", size )
        ]
  in
    [ Html.div [ style ] [ Html.text score ] ]


drawElements elements color =
  let
    drawColoredCell =
      drawCell color
  in
    List.map drawColoredCell elements


drawElement element color =
  [ drawCell color element ]


drawCell color element =
  let
    ( x, y ) =
      element.position

    leftMargin =
      cellSize * x |> toString

    leftMarginString =
      leftMargin ++ "px"

    topMargin =
      cellSize * y |> toString

    topMarginString =
      topMargin ++ "px"

    size =
      toString cellSize ++ "px"

    style =
      Attr.style
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
euclidianDistance ( p1, p2 ) ( q1, q2 ) =
  (q1 - p1) ^ 2 + (q2 - p2) ^ 2 |> toFloat |> sqrt


randomCoordinateGenerator =
  Random.pair (Random.int 1 23) (Random.int 1 23)


randomDirectionPairGenerator =
  Random.pair (Random.int -1 1) (Random.int -1 1)



-------------------- Initial Values ---------------


initialModel =
  { ai = initialAi
  , home = ( 1, 1 )
  , rabbits = initialRabbits
  , walls = outerWalls
  , fences = []
  , score = 0
  , seed = Random.initialSeed currentTime
  , turns = 0
  , state = Running
  }


outerWalls =
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


initialRabbits =
  [ { position = ( 10, 10 ), moveDirection = LeftUp }
  , { position = ( 11, 10 ), moveDirection = RightUp }
  , { position = ( 11, 11 ), moveDirection = RightDown }
  , { position = ( 10, 11 ), moveDirection = LeftDown }
  ]


winningString =
  "Yay, you won with this many points: "



-------------------- CSS --------------------------


pageStyleAttribute =
  Attr.style pageStyle


pageStyle =
  [ ( "background-color", "chartreuse" )
  , ( "width", toString fieldSize ++ "px" )
  , ( "height", toString fieldSize ++ "px" )
  ]


resultScreen =
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


cellSize =
  25


fieldSize =
  25 * cellSize


gridSize =
  25


maxTurns =
  300


volatilityThreshold =
  2


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
