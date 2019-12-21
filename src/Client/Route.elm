module Client.Route exposing (..)

import Json.Decode exposing (oneOf)
import Json.Decode.Extra
import Server.HTTP exposing (Method, Request)
import Url
import Url.Parser exposing ((</>), (<?>), map, s)
import Url.Parser.Query


type Page
    = NotFound
    | Homepage
    | Login


{-| This section is the equivalent of common web frameworks' "routes"
-}
router : Url.Parser.Parser (Page -> b) b
router =
    Url.Parser.oneOf
        [ map Homepage Url.Parser.top
        , map Login (s "login")
        ]


pagePath : Page -> String
pagePath page =
    case page of
        NotFound ->
            "/"

        Homepage ->
            "/"

        Login ->
            "/login"
