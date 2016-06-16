module Presence exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Debug
import Dict exposing (Dict)


-- Phoenix.Presence types


type alias PresenceState =
    Dict String PresenceStateMetaWrapper


type alias PresenceStateMetaWrapper =
    { metas : List PresenceStateMetaValue }


type alias PresenceStateMetaValue =
    { phx_ref : String
    , online_at : String
    , device : String
    }


type alias PresenceDiff =
    { leaves : PresenceState
    , joins : PresenceState
    }



-- Our model will track a list of messages and the text for our new message to
-- send.  We only support chatting in a single channel for now.


type alias ChatMessage =
    { user : String
    , body : String
    }


type alias Model =
    { newMessage : String
    , messages : List ChatMessage
    , username : String
    , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
    , phxPresences : PresenceState
    }


type Msg
    = SetNewMessage String
    | JoinChannel
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SendMessage
    | ReceiveChatMessage JE.Value
    | SetUsername String
    | ConnectSocket
    | HandlePresenceState JE.Value
    | HandlePresenceDiff JE.Value


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , username = ""
    , phxSocket = Nothing
    , phxPresences = Dict.empty
    }


socketServer : String -> String
socketServer username =
    "ws://localhost:4000/socket/websocket?username=" ++ username


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket username =
    Phoenix.Socket.init (socketServer username)
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "room:lobby" ReceiveChatMessage
        |> Phoenix.Socket.on "presence_state" "room:lobby" HandlePresenceState
        |> Phoenix.Socket.on "presence_diff" "room:lobby" HandlePresenceDiff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewMessage string ->
            { model | newMessage = string } ! []

        JoinChannel ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        channel =
                            Phoenix.Channel.init "room:lobby"

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.join channel modelPhxSocket
                    in
                        ( { model | phxSocket = Just phxSocket }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        PhoenixMsg msg ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.update msg modelPhxSocket
                    in
                        ( { model | phxSocket = Just phxSocket }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        SendMessage ->
            case model.phxSocket of
                Nothing ->
                    model ! []

                Just modelPhxSocket ->
                    let
                        payload =
                            (JE.object [ ( "body", JE.string model.newMessage ) ])

                        push' =
                            Phoenix.Push.init "new:msg" "room:lobby"
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push' modelPhxSocket
                    in
                        ( { model
                            | newMessage = ""
                            , phxSocket = Just phxSocket
                          }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        ReceiveChatMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    { model | messages = model.messages ++ [ chatMessage ] } ! []

                Err error ->
                    model ! []

        SetUsername username ->
            { model | username = username } ! []

        ConnectSocket ->
            { model | phxSocket = Just (initPhxSocket model.username) } ! []

        HandlePresenceState raw ->
            case JD.decodeValue presenceStateDecoder raw of
                Ok presenceState ->
                    let
                        _ =
                            Debug.log "PresenceState" presenceState
                    in
                        model ! []

                Err error ->
                    let
                        _ =
                            Debug.log "Error" error
                    in
                        model ! []

        HandlePresenceDiff raw ->
            case JD.decodeValue presenceDiffDecoder raw of
                Ok presenceDiff ->
                    let
                        _ =
                            Debug.log "PresenceDiff" presenceDiff
                    in
                        model ! []

                Err error ->
                    let
                        _ =
                            Debug.log "Error" error
                    in
                        model ! []


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.object2 ChatMessage
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)


presenceDiffDecoder : JD.Decoder PresenceDiff
presenceDiffDecoder =
    JD.object2 PresenceDiff
        ("leaves" := presenceStateDecoder)
        ("joins" := presenceStateDecoder)


presenceStateDecoder : JD.Decoder PresenceState
presenceStateDecoder =
    JD.dict presenceStateMetaWrapperDecoder


presenceStateMetaWrapperDecoder : JD.Decoder PresenceStateMetaWrapper
presenceStateMetaWrapperDecoder =
    JD.object1 PresenceStateMetaWrapper
        ("metas" := JD.list presenceStateMetaDecoder)


presenceStateMetaDecoder : JD.Decoder PresenceStateMetaValue
presenceStateMetaDecoder =
    JD.object3 PresenceStateMetaValue
        ("phx_ref" := JD.string)
        ("online_at" := JD.string)
        ("device" := JD.string)


viewMessage : ChatMessage -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


lobbyManagementView : Html Msg
lobbyManagementView =
    button [ onClick JoinChannel ] [ text "Join lobby" ]


messageListView : Model -> Html Msg
messageListView model =
    div [ class "messages" ]
        (List.map viewMessage model.messages)


messageInputView : Model -> Html Msg
messageInputView model =
    form [ onSubmit SendMessage ]
        [ input [ placeholder "Message...", onInput SetNewMessage, value model.newMessage ] [] ]


chatInterfaceView : Model -> Html Msg
chatInterfaceView model =
    div []
        [ lobbyManagementView
        , messageListView model
        , messageInputView model
        ]


setUsernameView : Html Msg
setUsernameView =
    form [ onSubmit ConnectSocket ]
        [ input [ onInput SetUsername, placeholder "Enter a username" ] [] ]


view : Model -> Html Msg
view model =
    case model.phxSocket of
        Nothing ->
            setUsernameView

        _ ->
            chatInterfaceView model


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.phxSocket of
        Nothing ->
            Sub.none

        Just phxSocket ->
            Phoenix.Socket.listen phxSocket PhoenixMsg


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


syncState : PresenceState -> PresenceState -> PresenceState
syncState newState state =
    newState


syncDiff : PresenceDiff -> PresenceState -> PresenceState
syncDiff diff state =
    let
        mergeLeaves : PresenceState -> String -> PresenceStateMetaWrapper -> PresenceStateMetaWrapper
        mergeLeaves leaves key currentMetaWrapper =
            case Dict.get key leaves of
                Nothing ->
                    currentMetaWrapper

                Just leaveValues ->
                    let
                        leftRefs =
                            List.map .phx_ref leaveValues.metas
                    in
                        currentMetaWrapper.metas
                            |> List.filter
                                (\metaValue ->
                                    not (List.any (\phx_ref -> metaValue.phx_ref == phx_ref) leftRefs)
                                )
                            |> PresenceStateMetaWrapper

        mergeJoins : PresenceState -> PresenceState -> PresenceState
        mergeJoins left right =
            let
                inBoth : comparable -> PresenceStateMetaWrapper -> PresenceStateMetaWrapper -> PresenceState -> PresenceState
                inBoth key leftValue rightValue acc =
                    acc |> Dict.insert key (PresenceStateMetaWrapper (leftValue.metas ++ rightValue.metas))
            in
                Dict.merge Dict.insert
                    inBoth
                    Dict.insert
                    left
                    right
                    Dict.empty
    in
        state
            |> mergeJoins diff.joins
            |> Dict.map (mergeLeaves diff.leaves)
            |> Dict.filter (\_ metaWrapper -> metaWrapper.metas /= [])
