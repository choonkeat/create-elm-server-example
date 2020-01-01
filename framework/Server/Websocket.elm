port module Server.Websocket exposing (Connection, Headers, Key, WebsocketEvent(..), handleWebsocket, send)

import Json.Decode
import Json.Encode


type alias Key =
    String


type alias Connection =
    Json.Encode.Value


type alias Payload =
    Json.Encode.Value


type alias Headers =
    Json.Encode.Value


type WebsocketEvent
    = Open Connection Key Headers
    | Message Connection Key Payload
    | Close Connection Key Int (Maybe String)


handleWebsocket : (Result Json.Decode.Error WebsocketEvent -> msg) -> Sub msg
handleWebsocket msgTag =
    onWebsocketEvent (Json.Decode.decodeValue decodeWebsocketEvent >> msgTag)


decodeWebsocketEvent : Json.Decode.Decoder WebsocketEvent
decodeWebsocketEvent =
    Json.Decode.oneOf
        [ Json.Decode.map3 Open
            (Json.Decode.field "open" Json.Decode.value)
            (Json.Decode.field "key" Json.Decode.string)
            (Json.Decode.field "headers" Json.Decode.value)
        , Json.Decode.map3 Message
            (Json.Decode.field "message" Json.Decode.value)
            (Json.Decode.field "key" Json.Decode.string)
            (Json.Decode.field "payload" Json.Decode.value)
        , Json.Decode.map4 Close
            (Json.Decode.field "close" Json.Decode.value)
            (Json.Decode.field "key" Json.Decode.string)
            (Json.Decode.field "reasonCode" Json.Decode.int)
            (Json.Decode.field "description" (Json.Decode.maybe Json.Decode.string))
        ]


send : Connection -> Key -> String -> Cmd msg
send connection key body =
    let
        message =
            Json.Encode.object
                [ ( "key", Json.Encode.string key )
                , ( "connection", connection )
                , ( "body", Json.Encode.string body )
                ]
    in
    writeWs message


port onWebsocketEvent : (Json.Encode.Value -> msg) -> Sub msg


port writeWs : Json.Encode.Value -> Cmd msg
