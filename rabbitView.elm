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
