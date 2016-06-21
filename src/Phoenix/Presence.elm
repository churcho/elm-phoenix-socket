module Phoenix.Presence
    exposing
        ( PresenceState
        , PresenceStateMetaWrapper
        , PresenceStateMetaValue
        , PresenceDiff
        , syncState
        , syncDiff
        , presenceStateDecoder
        , presenceDiffDecoder
        )

import Dict exposing (Dict)
import Json.Decode as JD exposing ((:=))


-- TYPES


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



-- Json Decoders


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



-- API


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
                merge Dict.insert
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


merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            Dict.foldl stepState ( Dict.toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers
