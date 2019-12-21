module Server.Framework exposing (httpEndpoint, instance)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import Server.HTTP exposing (StatusCode(..))
import Server.Handler
import Server.Websocket exposing (WebsocketEvent(..))
import Task
import Url
import Url.Parser


httpEndpoint =
    "/api/clientMsg2MaybeServerMsg"


type FrameworkMsg msg x serverMsg
    = AppMsg msg
    | OnHttpRequest Server.HTTP.Request
    | OnWebsocketEvent (Result Json.Decode.Error Server.Websocket.WebsocketEvent)
    | ReplyHttpClient Server.HTTP.Request (Result x serverMsg)
    | ReplyWebsocketClient Server.Websocket.Key Server.Websocket.Connection (Result x serverMsg)


type alias FrameworkModel a header =
    { -- storing state, but not doing anything with it at the moment
      websockets : Dict Server.Websocket.Key { conn : Server.Websocket.Connection, headers : header }

    -- our app's model goes here
    , appModel : a
    }


instance :
    -- Platform.worker
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg

    -- Extension
    , routeHttpRequest : ( Server.HTTP.Method, header, Maybe endpoint ) -> Server.HTTP.Request -> model -> ( model, Cmd msg )
    , updateFromClient : header -> clientMsg -> model -> ( model, Task x serverMsg )
    , serverMsgEncoder : serverMsg -> Json.Encode.Value
    , clientMsgDecoder : Json.Decode.Decoder clientMsg
    , headerDecoder : Json.Decode.Decoder header
    , errorEncoder : x -> Json.Encode.Value
    , urlRouter : Url.Parser.Parser (endpoint -> endpoint) endpoint
    }
    -> Program flags (FrameworkModel model header) (FrameworkMsg msg x serverMsg)
instance cfg =
    let
        sendToClient key conn result =
            Server.Websocket.send conn
                key
                (Json.Encode.encode 0
                    (encodeResultResult
                        cfg.errorEncoder
                        cfg.serverMsgEncoder
                        result
                    )
                )
    in
    Platform.worker
        { init = init cfg.init
        , update =
            update cfg.update
                cfg.updateFromClient
                cfg.routeHttpRequest
                sendToClient
                cfg.serverMsgEncoder
                cfg.clientMsgDecoder
                cfg.headerDecoder
                cfg.errorEncoder
                cfg.urlRouter
        , subscriptions = subscriptions cfg.subscriptions
        }


init : (flags -> ( model, Cmd msg )) -> flags -> ( FrameworkModel model header, Cmd (FrameworkMsg msg x serverMsg) )
init appInit flags =
    let
        ( appModel, appCmd ) =
            appInit flags

        model =
            FrameworkModel Dict.empty appModel
    in
    ( model, Cmd.map AppMsg appCmd )


update :
    (msg -> model -> ( model, Cmd msg ))
    -> (header -> clientMsg -> model -> ( model, Task x serverMsg ))
    -> (( Server.HTTP.Method, header, Maybe endpoint ) -> Server.HTTP.Request -> model -> ( model, Cmd msg ))
    -> (Server.Websocket.Key -> Server.Websocket.Connection -> Result x serverMsg -> Cmd (FrameworkMsg msg x serverMsg))
    -> (serverMsg -> Json.Encode.Value)
    -> Json.Decode.Decoder clientMsg
    -> Json.Decode.Decoder header
    -> (x -> Json.Encode.Value)
    -> Url.Parser.Parser (endpoint -> endpoint) endpoint
    -> FrameworkMsg msg x serverMsg
    -> FrameworkModel model header
    -> ( FrameworkModel model header, Cmd (FrameworkMsg msg x serverMsg) )
update appUpdate updateFromClient routeHttpRequest sendToClient serverMsgEncoder clientMsgDecoder headerDecoder errorEncoder urlRouter msg model =
    -- all msg `Cmd msg` gets transformed to `Cmd (FrameworkMsg msg x serverMsg)`
    case msg of
        AppMsg m ->
            case appUpdate m model.appModel of
                ( newAppModel, appCmd ) ->
                    ( { model | appModel = newAppModel }, Cmd.map AppMsg appCmd )

        OnHttpRequest request ->
            let
                endpoint =
                    Server.HTTP.urlOf request
                        |> Url.fromString
                        |> Maybe.andThen (Url.Parser.parse urlRouter)

                maybeHeader =
                    Server.HTTP.headersOf request
                        |> Json.Decode.decodeValue headerDecoder
                        |> Result.toMaybe

                clientMsgResult =
                    Json.Decode.decodeString clientMsgDecoder (Server.HTTP.bodyOf request)
            in
            case ( maybeHeader, clientMsgResult, Server.HTTP.pathOf request == httpEndpoint ) of
                ( Just context, Ok clientmsg, True ) ->
                    let
                        ( newAppModel, updateTask ) =
                            updateFromClient context clientmsg model.appModel

                        replyCmd =
                            Task.attempt (ReplyHttpClient request) updateTask
                    in
                    ( { model | appModel = newAppModel }
                    , replyCmd
                    )

                ( Just context, _, _ ) ->
                    let
                        triplet =
                            ( Server.HTTP.methodOf request, context, endpoint )
                    in
                    case routeHttpRequest triplet request model.appModel of
                        ( newAppModel, appCmd ) ->
                            ( { model | appModel = newAppModel }, Cmd.map AppMsg appCmd )

                ( _, err, _ ) ->
                    -- invalid http request, just reject; no hard feelings
                    ( model, Server.Handler.writeResponse StatusInternalServerError (Debug.toString err) Json.Encode.null request )

        OnWebsocketEvent eventResult ->
            eventResult
                |> Result.mapError (Debug.log "OnWebsocketEvent")
                |> Result.map (routeWebsocketRequest updateFromClient sendToClient clientMsgDecoder headerDecoder model)
                |> Result.withDefault ( model, Cmd.none )

        ReplyHttpClient request serverMsgResult ->
            ( model
            , Server.Handler.writeResponse StatusOK
                (Json.Encode.encode 0 (encodeResultResult errorEncoder serverMsgEncoder serverMsgResult))
                (Json.Encode.object [ ( "Content-Type", Json.Encode.string "application/json" ) ])
                request
            )

        ReplyWebsocketClient key conn serverMsgResult ->
            ( model
            , sendToClient key conn serverMsgResult
            )


subscriptions : (model -> Sub msg) -> FrameworkModel model header -> Sub (FrameworkMsg msg x serverMsg)
subscriptions appSubscription model =
    Sub.batch
        [ Sub.map AppMsg (appSubscription model.appModel)
        , Server.Handler.handleRequest OnHttpRequest
        , Server.Websocket.handleWebsocket OnWebsocketEvent
        ]


routeWebsocketRequest :
    (header -> clientMsg -> model -> ( model, Task x serverMsg ))
    -> (Server.Websocket.Key -> Server.Websocket.Connection -> Result x serverMsg -> Cmd (FrameworkMsg msg x serverMsg))
    -> Json.Decode.Decoder clientMsg
    -> Json.Decode.Decoder header
    -> FrameworkModel model header
    -> Server.Websocket.WebsocketEvent
    -> ( FrameworkModel model header, Cmd (FrameworkMsg msg x serverMsg) )
routeWebsocketRequest updateFromClient sendToClient clientMsgDecoder headerDecoder model event =
    case Debug.log "WebsocketEvent" event of
        Open conn key rawHeaders ->
            case Json.Decode.decodeValue headerDecoder rawHeaders of
                Ok headers ->
                    let
                        newWebsockets =
                            Dict.insert key { conn = conn, headers = headers } model.websockets

                        _ =
                            Debug.log "connected" ( Dict.size newWebsockets, headers )
                    in
                    ( { model | websockets = newWebsockets }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "invalid headers" err
                    in
                    ( model, Cmd.none )

        Message conn key payload ->
            let
                clientMsgResult =
                    Json.Decode.decodeValue (Json.Decode.at [ "utf8Data" ] Json.Decode.string) payload
                        |> Result.andThen (Json.Decode.decodeString clientMsgDecoder)
            in
            case ( Dict.get key model.websockets, clientMsgResult ) of
                ( Just { headers }, Ok clientmsg ) ->
                    let
                        ( newAppModel, updateTask ) =
                            updateFromClient headers clientmsg model.appModel

                        replyCmd =
                            Task.attempt (ReplyWebsocketClient key conn) updateTask
                    in
                    ( { model | appModel = newAppModel }
                    , replyCmd
                    )

                invalidWsRequest ->
                    let
                        _ =
                            Debug.log "ignored websocket message" invalidWsRequest
                    in
                    ( model, Cmd.none )

        Close conn key reasonCode description ->
            let
                newWebsockets =
                    Dict.remove key model.websockets

                _ =
                    Debug.log "connected" (Dict.size newWebsockets)
            in
            ( { model | websockets = newWebsockets }, Cmd.none )



--


{-| encoder following the style of Auto
-}
encodeResultResult : (x -> Json.Encode.Value) -> (a -> Json.Encode.Value) -> Result.Result x a -> Json.Encode.Value
encodeResultResult encodex encodea result =
    case result of
        Err x ->
            Json.Encode.list identity [ Json.Encode.string "Result.Err", encodex x ]

        Ok a ->
            Json.Encode.list identity [ Json.Encode.string "Result.Ok", encodea a ]
