module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    )

import Browser.Navigation as Nav
import Game
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | ShowBoard Game.BoardId


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map ShowBoard (s "board" </> Game.boardIdUrlParser)
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                ShowBoard boardId ->
                    [ "board", Game.showBoardId boardId ]
    in
    "#/" ++ String.join "/" pieces
