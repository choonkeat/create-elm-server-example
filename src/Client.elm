module Client exposing (main)

import Browser
import Browser.Navigation
import Client.Framework
import Client.Route exposing (Page(..))
import Client.Webpush
import Client.Websocket
import Html exposing (Html, a, button, div, form, input, pre, text)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time
import Types exposing (..)
import Types.Auto
import Url
import Url.Parser


instance =
    Client.Framework.instance
        -- Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange

        -- Extension
        , updateFromServer = updateFromServer
        , clientMsgEncoder = Types.Auto.encodeTypesMsgFromClient
        , serverMsgDecoder = Types.Auto.decodeTypesMsgFromServer
        , errorDecoder = Json.Decode.string
        }


main =
    -- Given `(main, sendToServer) = Client.Framework.instance { ... }`
    Tuple.first instance


{-| Clients send messages to Server with this
-}
sendToServer : MsgFromClient -> Task Http.Error (Result String MsgFromServer)
sendToServer =
    -- Given `(main, sendToServer) = Client.Framework.instance { ... }`
    Tuple.second instance


type alias Flags =
    { requestContextJson : Maybe String
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , page : Client.Route.Page

    --
    , requestContext : RequestContext
    , greeting : String
    , serverGreeting : String
    }


type Msg
    = OnMsgFromServer (Result Http.Error (Result String MsgFromServer))
      --
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | OnInputGreeting String
    | OnSubmitGreeting String


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( page, pageCmd ) =
            pageCmdFromUrl url

        cmd =
            Task.attempt OnMsgFromServer (sendToServer AskGreeting)

        initRequestContext =
            -- see Server.elm `requestContextJson`
            Maybe.withDefault "" flags.requestContextJson
                |> Json.Decode.decodeString Types.Auto.decodeTypesRequestContext
                |> Result.withDefault Anonymous

        model =
            { navKey = navKey
            , page = page
            , requestContext = initRequestContext
            , greeting = ""
            , serverGreeting = ""
            }
    in
    ( Debug.log "booted" model, Cmd.batch [ cmd, pageCmd ] )


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ text ("Server says: " ++ model.serverGreeting)
        , form [ onSubmit (OnSubmitGreeting model.greeting) ]
            [ input [ onInput OnInputGreeting, value model.greeting ] []
            , button [ type_ "submit" ] [ text "Submit" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMsgFromServer result ->
            case result of
                Ok (Ok serverMsg) ->
                    updateFromServer serverMsg model

                Ok (Err appErr) ->
                    ( { model | greeting = Debug.toString appErr }, Cmd.none )

                Err httpErr ->
                    ( { model | greeting = Debug.toString httpErr }, Cmd.none )

        OnUrlRequest (Browser.Internal urlUrl) ->
            -- will trigger OnUrlChange
            ( model, Browser.Navigation.pushUrl model.navKey (Url.toString urlUrl) )

        OnUrlRequest (Browser.External urlString) ->
            -- will leave our app
            ( model, Browser.Navigation.load urlString )

        OnUrlChange urlUrl ->
            case pageCmdFromUrl urlUrl of
                ( newPage, pageCmd ) ->
                    ( { model | page = newPage }, pageCmd )

        OnInputGreeting s ->
            ( { model | greeting = s }, Cmd.none )

        OnSubmitGreeting s ->
            ( { model | greeting = "" }, Task.attempt OnMsgFromServer (sendToServer (SetGreeting s)) )


updateFromServer : MsgFromServer -> Model -> ( Model, Cmd Msg )
updateFromServer serverMsg model =
    case serverMsg of
        CurrentGreeting s ->
            ( { model | serverGreeting = s }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


pageCmdFromUrl : Url.Url -> ( Client.Route.Page, Cmd Msg )
pageCmdFromUrl urlUrl =
    case Url.Parser.parse Client.Route.router urlUrl of
        Nothing ->
            ( Client.Route.NotFound, Cmd.none )

        Just newPage ->
            ( newPage, Cmd.none )
