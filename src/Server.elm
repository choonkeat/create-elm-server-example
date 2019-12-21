module Server exposing (Flags, Msg(..), init, main, subscriptions, update)

{-| Example server side code by <https://github.com/choonkeat/create-elm-server>
-}

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Process
import Server.Framework
import Server.HTTP exposing (Method(..), Request, StatusCode(..))
import Server.Handler
import Server.Route exposing (Endpoint)
import Server.Websocket exposing (WebsocketEvent(..))
import Task exposing (Task)
import Templates
import Time
import Types exposing (..)
import Types.Auto
import Url.Parser exposing ((</>))


main =
    Server.Framework.instance
        -- Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions

        -- Extension
        , routeHttpRequest = routeHttpRequest
        , updateFromClient = updateFromClient
        , serverMsgEncoder = Types.Auto.encodeTypesMsgFromServer
        , clientMsgDecoder = Types.Auto.decodeTypesMsgFromClient
        , headerDecoder = headerDecoder
        , urlRouter = Server.Route.router
        , errorEncoder = Json.Encode.string
        }


type alias Flags =
    {}


type alias ServerState =
    { greeting : String
    }


type Msg
    = Msg


init : Flags -> ( ServerState, Cmd Msg )
init flags =
    let
        serverState =
            { greeting = "Hello world" }

        cmd =
            Cmd.none
    in
    ( serverState, cmd )


update : Msg -> ServerState -> ( ServerState, Cmd Msg )
update msg serverState =
    case msg of
        Msg ->
            ( serverState, Cmd.none )


routeHttpRequest : ( Server.HTTP.Method, RequestContext, Maybe Endpoint ) -> Request -> ServerState -> ( ServerState, Cmd Msg )
routeHttpRequest ( method, ctx, endpoint ) request serverState =
    case Debug.log "routeHttpRequest" ( method, ctx, endpoint ) of
        ( GET, _, Just (Server.Route.Status s) ) ->
            ( serverState, Server.Handler.writeResponse StatusOK (Debug.toString ( ctx, s )) Json.Encode.null request )

        ( GET, _, _ ) ->
            let
                encodedRequestContextString =
                    Json.Encode.encode 0 (Types.Auto.encodeTypesRequestContext ctx)
                        -- doubly encoded json string since Elm Flags cannot be Custom Type
                        |> Json.Encode.string
                        |> Json.Encode.encode 0

                htmlString =
                    Templates.clientIndexHtml
                        -- See Client.init `requestContextJson`
                        |> String.replace "requestContextJson: null" ("requestContextJson: " ++ encodedRequestContextString)

                htmlHeader =
                    Json.Encode.object [ ( "Content-Type", Json.Encode.string "text/html; charset=utf-8" ) ]
            in
            ( serverState, Server.Handler.writeResponse StatusOK htmlString htmlHeader request )

        _ ->
            -- anything else, 404
            ( serverState, Server.Handler.writeResponse StatusNotFound (Server.HTTP.pathOf request) Json.Encode.null request )


subscriptions : ServerState -> Sub Msg
subscriptions serverState =
    Sub.none


headerDecoder : Json.Decode.Decoder RequestContext
headerDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Cookied (Json.Decode.field "cookie" Json.Decode.string)
        , Json.Decode.succeed Anonymous
        ]


{-| Upside of this return value type, is that Server.Framework can ensure the client is replied to:
with the value of `Maybe MsgFromServer`. If we left it for app developer to call `sendToClient`, and
there's a loophole in logic where it isn't called, then the http request will hang.

BUT the downside is, now we can't do some Cmd/Task and reply later...

-}
updateFromClient : RequestContext -> MsgFromClient -> ServerState -> ( ServerState, Task String MsgFromServer )
updateFromClient ctx clientMsg serverState =
    case clientMsg of
        AskGreeting ->
            ( serverState
              -- immediately reply with our current value
            , Task.succeed (CurrentGreeting serverState.greeting)
            )

        SetGreeting newGreeting ->
            let
                replyTask =
                    Time.now
                        |> Task.map (\now -> "Received: " ++ newGreeting ++ " at " ++ String.fromInt (Time.posixToMillis now))
                        |> Task.map (\ackMessage -> CurrentGreeting ackMessage)
            in
            ( { serverState | greeting = newGreeting }
            , replyTask
            )
