module Types exposing (..)

{-| run `npx elm-auto-encoder-decoder src/Types.elm` to generate simple
encoder/decoders for this file in `src/Auto/Types.elm`
-}


{-| All messages that Client can send to Server
-}
type MsgFromClient
    = AskGreeting
    | SetGreeting String


{-| All messages that Server can ~send to~ reply Client
-}
type MsgFromServer
    = CurrentGreeting String


{-| Http headers will be parsed into a RequestContext
Failure to parse means error; keep an always successful scenario, e.g. `Anonymous`
-}
type RequestContext
    = Cookied String
      -- e.g. `LoggedIn JWTData`
    | Anonymous
