module Client.Framework exposing (instance)

import Browser
import Browser.Navigation
import Client.Websocket
import Html
import Http
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import Server.Framework exposing (httpEndpoint)
import Url


type FrameworkMsg a
    = AppMsg a
    | WebSocketConnected Int
    | WebSocketReceive String


instance :
    -- Browser.application
    { init : flags -> Url.Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg

    -- Extension
    , updateFromServer : serverMsg -> model -> ( model, Cmd msg )
    , clientMsgEncoder : clientMsg -> Json.Encode.Value
    , serverMsgDecoder : Json.Decode.Decoder serverMsg
    , errorDecoder : Json.Decode.Decoder x
    }
    -> ( Program flags model (FrameworkMsg msg), clientMsg -> Task Http.Error (Result x serverMsg) )
instance cfg =
    let
        sendToServer m =
            -- Client.Websocket.send (Json.Encode.encode 0 (cfg.clientMsgEncoder m))
            Http.task
                { method = "POST"
                , headers = []
                , url = httpEndpoint
                , body = Http.jsonBody (cfg.clientMsgEncoder m)
                , resolver = Http.stringResolver (serverMsgFromResponse (decodeResultResult cfg.errorDecoder cfg.serverMsgDecoder))
                , timeout = Just 60000
                }

        app =
            Browser.application
                { init = init cfg.init
                , view = view cfg.view
                , update = update cfg.update cfg.updateFromServer cfg.serverMsgDecoder
                , subscriptions = subscriptions cfg.subscriptions
                , onUrlRequest = cfg.onUrlRequest >> AppMsg
                , onUrlChange = cfg.onUrlChange >> AppMsg
                }
    in
    ( app, sendToServer )


serverMsgFromResponse : Json.Decode.Decoder serverMsg -> (Http.Response String -> Result Http.Error serverMsg)
serverMsgFromResponse serverMsgDecoder resp =
    case resp of
        Http.GoodStatus_ metadata s ->
            Json.Decode.decodeString serverMsgDecoder s
                |> Result.mapError (\err -> Http.BadBody (metadata.statusText ++ " " ++ Json.Decode.errorToString err))

        Http.BadStatus_ metadata s ->
            Json.Decode.decodeString serverMsgDecoder s
                |> Result.mapError (\err -> Http.BadBody (metadata.statusText ++ " " ++ Json.Decode.errorToString err))

        Http.BadUrl_ s ->
            Err (Http.BadUrl s)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError


init : (flags -> Url.Url -> Browser.Navigation.Key -> ( model, Cmd msg )) -> flags -> Url.Url -> Browser.Navigation.Key -> ( model, Cmd (FrameworkMsg msg) )
init appInit flags urlUrl navKey =
    let
        ( model, appCmd ) =
            appInit flags urlUrl navKey
    in
    ( model, Cmd.map AppMsg appCmd )


view : (model -> Browser.Document msg) -> model -> Browser.Document (FrameworkMsg msg)
view appView model =
    let
        { title, body } =
            appView model
    in
    { title = title, body = List.map (Html.map AppMsg) body }


update :
    (msg -> model -> ( model, Cmd msg ))
    -> (serverMsg -> model -> ( model, Cmd msg ))
    -> Json.Decode.Decoder serverMsg
    -> FrameworkMsg msg
    -> model
    -> ( model, Cmd (FrameworkMsg msg) )
update appUpdate updateFromServer serverMsgDecoder msg model =
    Tuple.mapSecond (Cmd.map AppMsg) <|
        -- all msg `Cmd msg` gets transformed to `Cmd (FrameworkMsg msg)`
        case msg of
            AppMsg m ->
                appUpdate m model

            WebSocketConnected t ->
                ( model, Cmd.none )

            WebSocketReceive str ->
                Json.Decode.decodeString serverMsgDecoder str
                    |> Result.mapError (Debug.log "updateFromServer: err")
                    |> Result.map (\m -> updateFromServer (Debug.log "updateFromServer" m) model)
                    |> Result.withDefault ( model, Cmd.none )


subscriptions : (model -> Sub msg) -> model -> Sub (FrameworkMsg msg)
subscriptions appSubscription model =
    Sub.batch
        [ Sub.map AppMsg (appSubscription model)
        , Client.Websocket.listen WebSocketConnected WebSocketReceive
        ]



--


{-| encoder following the style of Auto
-}
decodeResultResult : Json.Decode.Decoder x -> Json.Decode.Decoder a -> Json.Decode.Decoder (Result.Result x a)
decodeResultResult decodex decodea =
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Result.Err" ->
                        Json.Decode.map Err (Json.Decode.index 1 decodex)

                    "Result.Ok" ->
                        Json.Decode.map Ok (Json.Decode.index 1 decodea)

                    _ ->
                        Json.Decode.fail ("Unexpected Result.Result: " ++ word)
            )
