module RabbitView (..) where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import RabbitModel exposing (..)


---------------------------- View --------------------------------------


view : Model -> Html.Html
view model =
  if model.turns > maxTurns then
    drawEnd model.score
  else
    drawScene model

instructions : Html.Html
instructions = 
  Html.ul [] 
  [ Html.li [] [ Html.text "The red box is an AI frantically trying to catch fluffy bunnies and bring them home (top left corner)." ]
  , Html.li [] [ Html.text "The white boxes are the bunnies chaotically frolicking through the meddow (green)." ]
  , Html.li [] [ Html.text "You can help the AI by clicking on the field and placing fences. These can constrict the bunny movement (for a while)." ]
  , Html.li [] [ Html.text "The more bunnies you two catch the more points you get when the game is over." ]
  , Html.li [] [ Html.text "Press Space to pause or unpause the game." ]
  ]


drawEnd score =
  Html.div [ resultStyle ] [ toString score |> (++) winningString |> Html.text ]


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
    Html.div []
      [ Html.div [ pageStyleAttribute ] subDivs
      , Html.div [ instructionsStyle ] [ instructions ]
      ]


drawScore score =
  let
    size =
      toString cellSize ++ "px"

    style =
      Attr.style
        [ ("z-index", "10")
        , ("vertical-align", "middle")
        , ( "background-color", "black" )
        , ( "position", "absolute" )
        , ( "margin-left", "0px" )
        , ( "margin-top", "0px" )
        , ( "width", size )
        , ( "height", size )
        , ("color", "white")
        , ("text-align", "center")
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
