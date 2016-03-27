module Rabbit (..) where

import Time
import Keyboard
import Mouse
import Random
import RabbitView exposing (view)
import RabbitModel exposing (..)


-------------- Model --------------
-- We only need this for the initial randomicity seed


main =
  Signal.map view game


game : Signal Model
game =
  (Signal.map3 (,,) Mouse.position Mouse.isDown Keyboard.space) |> Signal.sampleOn (Time.fps fps) |> Signal.foldp update initialModel 


update : Actions -> Model -> Model
update actions model =
  let
    ( _, _, space ) =
      actions
  in
    case model.state of
      Pause ->
        if space then
          checkWinSituation model actions
        else
          model

      End ->
        if space then
          checkWinSituation initialModel actions
        else
          model

      Running ->
        if space then
          pauseGame model
        else
          checkWinSituation model actions


pauseGame : Model -> Model
pauseGame model =
  Model model.ai model.home model.rabbits model.walls model.fences model.score model.seed model.turns Pause


checkWinSituation : Model -> Actions -> Model
checkWinSituation model actions =
  let
    newState =
      if model.turns > maxTurns then
        End
      else
        Running
  in
    case newState of
      Running ->
        move model actions

      End ->
        Model model.ai model.home model.rabbits model.walls model.fences model.score model.seed model.turns End

      Pause ->
        model


move : Model -> Actions -> Model
move model actions =
  let
    ( _, _, space ) =
      actions

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
addFence fences ( ( x, y ), mouseClicked, _ ) =
  let
    newFences =
      if mouseClicked then
        Fence ( x // cellSize, y // cellSize ) TopBottom fenceLifetime :: fences
      else
        fences
  in
    ageFences newFences


ageFences : List Fence -> List Fence
ageFences fences =
  List.map ageSingleFence fences |> List.filter (\fence -> fence.age > 0)


ageSingleFence : Fence -> Fence
ageSingleFence { position, orientation, age } =
  Fence position orientation (age - 1)


dropCargo : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
dropCargo model actions =
  let
    newFences =
      addFence model.fences actions

    newbornRabbit =
      createNewRabbit model.seed

    newRabbits =
      moveRabbits model.fences model.walls model.rabbits model.seed |> (::) newbornRabbit

    newScore =
      model.score + 1
  in
    ( { position = model.ai.position, hasCargo = False }, newScore, newRabbits, newFences )


moveRabbits : List Fence -> List Wall -> List Rabbit -> Random.Seed -> List Rabbit
moveRabbits fences walls rabbits seed =
  let
    rabbitProgression =
      progressRabbit fences walls
  in
    randomRabbitMover rabbits seed |> List.map rabbitProgression


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
  if x < bunnyBounciness then
    newRabbit
  else
    oldRabbit


progressRabbit : List Fence -> List Wall -> Rabbit -> Rabbit
progressRabbit fences walls rabbit =
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
      checkForCollision fences walls newPosition rabbit.position
  in
    Rabbit correctedPosition rabbit.moveDirection


checkForCollision : List Fence -> List Wall -> Coordinate -> Coordinate -> Coordinate
checkForCollision fences walls newPosition oldPosition =
  let
    isNoCollision =
      \element -> element.position /= newPosition
  in
    if List.all isNoCollision fences && List.all isNoCollision walls then
      newPosition
    else
      oldPosition


directionListCreator count =
  randomDirectionPairGenerator |> Random.list count


moveCloserToHome : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
moveCloserToHome model actions =
  let
    newFences =
      addFence model.fences actions

    ( x, y ) =
      model.ai.position

    newPosition =
      if x > y then
        ( x - 1, y )
      else
        ( x, y - 1 )

    newRabbits =
      moveRabbits model.fences model.walls model.rabbits model.seed
  in
    ( { position = newPosition, hasCargo = model.ai.hasCargo }, model.score, newRabbits, newFences )


findRabbit : Model -> Actions -> ( AI, Score, List Rabbit, List Fence )
findRabbit model actions =
  let
    newFences =
      addFence model.fences actions

    closestRabbit =
      pickClosestRabbit model.ai model.rabbits

    closestRabbitPosition =
      closestRabbit.position

    newAiPosition =
      moveToward model.ai.position closestRabbitPosition

    ( updatedPosition, ( newRabbits, cargo ) ) =
      if model.ai.position == closestRabbitPosition then
        ( model.ai.position, (pickUpRabbit model.rabbits closestRabbit) )
      else
        ( newAiPosition, ( moveRabbits model.fences model.walls model.rabbits model.seed, False ) )
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
        moveNumberCloser aiX x

    newY =
      if aiY == y then
        aiY
      else
        moveNumberCloser aiY y
  in
    if xDistance > yDistance then
      ( (moveNumberCloser aiX x), aiY )
    else
      ( aiX, (moveNumberCloser aiY y) )


moveNumberCloser : Int -> Int -> Int
moveNumberCloser a b =
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



-------------------- Helper ---------------


pairToDirection : ( Int, Int ) -> Direction
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


euclidianDistance : Coordinate -> Coordinate -> Float
euclidianDistance ( p1, p2 ) ( q1, q2 ) =
  (q1 - p1) ^ 2 + (q2 - p2) ^ 2 |> toFloat |> sqrt


randomCoordinateGenerator =
  Random.pair (Random.int 1 23) (Random.int 1 23)


randomDirectionPairGenerator =
  Random.pair (Random.int -1 1) (Random.int -1 1)
