module Types.Auto exposing (..)

{- this file is generated by <https://github.com/choonkeat/elm-auto-encoder-decoder> do not modify manually


imports: Set.fromList []
importResolver_: Dict.fromList [("Anonymous","Types.Anonymous"),("AskGreeting","Types.AskGreeting"),("Bool","Bool"),("Cookied String","Types.Cookied String"),("CurrentGreeting String","Types.CurrentGreeting String"),("Dict","Dict.Dict"),("Float","Float"),("Int","Int"),("List","List"),("List.List","List"),("Maybe","Maybe"),("Maybe.Maybe","Maybe"),("MsgFromClient","Types.MsgFromClient"),("MsgFromServer","Types.MsgFromServer"),("RequestContext","Types.RequestContext"),("Set","Set.Set"),("SetGreeting String","Types.SetGreeting String"),("String","String"),("String.String","String")]
file.knownTypes: Dict.fromList [("MsgFromClient",ElmCustomType { constructors = [CustomTypeConstructor "AskGreeting",CustomTypeConstructor "SetGreeting String"], name = TypeName "MsgFromClient" [] }),("MsgFromServer",ElmCustomType { constructors = [CustomTypeConstructor "CurrentGreeting String"], name = TypeName "MsgFromServer" [] }),("RequestContext",ElmCustomType { constructors = [CustomTypeConstructor "Cookied String",CustomTypeConstructor "Anonymous"], name = TypeName "RequestContext" [] })]

--}

import Dict
import Set
import Types exposing (..)
import Set
import Dict
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


encodeString : String -> Json.Encode.Value
encodeString =
    Json.Encode.string


encodeInt : Int -> Json.Encode.Value
encodeInt =
    Json.Encode.int


encodeFloat : Float -> Json.Encode.Value
encodeFloat =
    Json.Encode.float


encodeBool : Bool -> Json.Encode.Value
encodeBool =
    Json.Encode.bool


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe encodera value =
    Maybe.map encodera value
        |> Maybe.withDefault Json.Encode.null


encodeList : (a -> Json.Encode.Value) -> List a -> Json.Encode.Value
encodeList =
    Json.Encode.list


encodeSetSet : (comparable -> Json.Encode.Value) -> Set.Set comparable -> Json.Encode.Value
encodeSetSet encoder =
    Set.toList >> encodeList encoder


encodeDictDict : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Dict.Dict a b -> Json.Encode.Value
encodeDictDict keyEncoder =
    Json.Encode.dict (\k -> Json.Encode.encode 0 (keyEncoder k))


--


decodeString : Json.Decode.Decoder String
decodeString =
    Json.Decode.string


decodeInt : Json.Decode.Decoder Int
decodeInt =
    Json.Decode.int


decodeFloat : Json.Decode.Decoder Float
decodeFloat =
    Json.Decode.float


decodeBool : Json.Decode.Decoder Bool
decodeBool =
    Json.Decode.bool


decodeMaybe : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
decodeMaybe =
    Json.Decode.maybe


decodeList : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeList =
    Json.Decode.list


decodeSetSet : Json.Decode.Decoder comparable -> Json.Decode.Decoder (Set.Set comparable)
decodeSetSet =
    Json.Decode.list >> Json.Decode.map Set.fromList


decodeDictDict : Json.Decode.Decoder comparable -> Json.Decode.Decoder b -> Json.Decode.Decoder (Dict.Dict comparable b)
decodeDictDict keyDecoder valueDecoder =
    Json.Decode.dict valueDecoder
    |> Json.Decode.map (\dict ->
        Dict.foldl (\string v acc ->
            case Json.Decode.decodeString keyDecoder string of
                Ok k ->
                    Dict.insert k v acc
                Err _ ->
                    acc
        ) Dict.empty dict
    )



encodeTypesRequestContext : Types.RequestContext  -> Json.Encode.Value
encodeTypesRequestContext value =
    -- ElmCustomType { constructors = [CustomTypeConstructor "Cookied String",CustomTypeConstructor "Types.Anonymous"], name = TypeName "Types.RequestContext" [] }
    case value of
        Types.Anonymous  -> Json.Encode.list identity [ encodeString "Types.Anonymous"]
        Types.Cookied arg0 -> Json.Encode.list identity [ encodeString "Types.Cookied", encodeString arg0]


encodeTypesMsgFromServer : Types.MsgFromServer  -> Json.Encode.Value
encodeTypesMsgFromServer value =
    -- ElmCustomType { constructors = [CustomTypeConstructor "CurrentGreeting String"], name = TypeName "Types.MsgFromServer" [] }
    case value of
        Types.CurrentGreeting arg0 -> Json.Encode.list identity [ encodeString "Types.CurrentGreeting", encodeString arg0]


encodeTypesMsgFromClient : Types.MsgFromClient  -> Json.Encode.Value
encodeTypesMsgFromClient value =
    -- ElmCustomType { constructors = [CustomTypeConstructor "Types.AskGreeting",CustomTypeConstructor "SetGreeting String"], name = TypeName "Types.MsgFromClient" [] }
    case value of
        Types.SetGreeting arg0 -> Json.Encode.list identity [ encodeString "Types.SetGreeting", encodeString arg0]
        Types.AskGreeting  -> Json.Encode.list identity [ encodeString "Types.AskGreeting"]


decodeTypesRequestContext : Json.Decode.Decoder (Types.RequestContext )
decodeTypesRequestContext  =
    -- ElmCustomType { constructors = [CustomTypeConstructor "Cookied String",CustomTypeConstructor "Types.Anonymous"], name = TypeName "Types.RequestContext" [] }
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Types.Anonymous" -> Json.Decode.succeed Types.Anonymous
                    "Types.Cookied" -> Json.Decode.succeed Types.Cookied |> Json.Decode.Pipeline.custom (Json.Decode.index 1 decodeString)
                    _ ->
                        Json.Decode.fail ("Unexpected Types.RequestContext: " ++ word)
            )
            

decodeTypesMsgFromServer : Json.Decode.Decoder (Types.MsgFromServer )
decodeTypesMsgFromServer  =
    -- ElmCustomType { constructors = [CustomTypeConstructor "CurrentGreeting String"], name = TypeName "Types.MsgFromServer" [] }
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Types.CurrentGreeting" -> Json.Decode.succeed Types.CurrentGreeting |> Json.Decode.Pipeline.custom (Json.Decode.index 1 decodeString)
                    _ ->
                        Json.Decode.fail ("Unexpected Types.MsgFromServer: " ++ word)
            )
            

decodeTypesMsgFromClient : Json.Decode.Decoder (Types.MsgFromClient )
decodeTypesMsgFromClient  =
    -- ElmCustomType { constructors = [CustomTypeConstructor "Types.AskGreeting",CustomTypeConstructor "SetGreeting String"], name = TypeName "Types.MsgFromClient" [] }
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Types.SetGreeting" -> Json.Decode.succeed Types.SetGreeting |> Json.Decode.Pipeline.custom (Json.Decode.index 1 decodeString)
                    "Types.AskGreeting" -> Json.Decode.succeed Types.AskGreeting
                    _ ->
                        Json.Decode.fail ("Unexpected Types.MsgFromClient: " ++ word)
            )
            