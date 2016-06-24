module Tests exposing (..)

import ElmTest exposing (..)
import Phoenix.Presence
    exposing
        ( list
        , listDefault
        , syncState
        , syncDiff
        , PresenceState
        , PresenceDiff
        , PresenceStateMetaValue
        , PresenceStateMetaWrapper
        )
import Dict exposing (Dict)


-- FIXTURES -------------------------------------


sampleMeta : PresenceStateMetaValue
sampleMeta =
    PresenceStateMetaValue "1" "sometime" "browser"


u1PresenceStateMetaWrapper : PresenceStateMetaWrapper
u1PresenceStateMetaWrapper =
    { metas =
        [ sampleMeta
        ]
    }


u1PresenceStateMetaWrapperAddition : PresenceStateMetaWrapper
u1PresenceStateMetaWrapperAddition =
    { metas =
        [ { sampleMeta | phx_ref = "1.2" }
        ]
    }


u1PresenceStateMetaWrapperWithAddition : PresenceStateMetaWrapper
u1PresenceStateMetaWrapperWithAddition =
    { metas =
        [ { sampleMeta | phx_ref = "1.2" }
        , { sampleMeta | phx_ref = "1" }
        ]
    }


u2PresenceStateMetaWrapper : PresenceStateMetaWrapper
u2PresenceStateMetaWrapper =
    { metas =
        [ { sampleMeta | phx_ref = "2" }
        ]
    }


u3PresenceStateMetaWrapper : PresenceStateMetaWrapper
u3PresenceStateMetaWrapper =
    { metas =
        [ { sampleMeta | phx_ref = "3" }
        ]
    }


fixtures : { joins : PresenceState, leaves : PresenceState, empty : PresenceState, state : PresenceState }
fixtures =
    { joins =
        Dict.empty
            |> Dict.insert "u1" u1PresenceStateMetaWrapperAddition
    , leaves =
        Dict.empty
            |> Dict.insert "u2" u2PresenceStateMetaWrapper
    , empty = Dict.empty
    , state =
        Dict.empty
            |> Dict.insert "u1" u1PresenceStateMetaWrapper
            |> Dict.insert "u2" u2PresenceStateMetaWrapper
            |> Dict.insert "u3" u3PresenceStateMetaWrapper
    }



-- END FIXTURES ---------------------------------


syncStateTests : List Test
syncStateTests =
    [ syncStateSyncsEmptyState
    ]


syncDiffTests : List Test
syncDiffTests =
    [ syncDiffSyncsEmptyState
    , syncDiffRemovesPresenceWhenMetaIsEmptyAndAddsAdditionalMeta
    , syncDiffAddsTwoNewUsersToExistingUserSuccessfully
    ]


listTests : List Test
listTests =
    [ listListsFullPresenceByDefault
    , listListsWithCustomFunction
    ]


syncStateSyncsEmptyState : Test
syncStateSyncsEmptyState =
    fixtures.joins `equals` (fixtures.empty |> syncState fixtures.joins)


syncDiffSyncsEmptyState : Test
syncDiffSyncsEmptyState =
    let
        diff =
            PresenceDiff fixtures.empty fixtures.joins
    in
        fixtures.joins `equals` (fixtures.empty |> syncDiff diff)


syncDiffRemovesPresenceWhenMetaIsEmptyAndAddsAdditionalMeta : Test
syncDiffRemovesPresenceWhenMetaIsEmptyAndAddsAdditionalMeta =
    let
        diff =
            PresenceDiff fixtures.leaves fixtures.joins

        expectedState =
            fixtures.empty
                |> Dict.insert "u1" u1PresenceStateMetaWrapperWithAddition
                |> Dict.insert "u3" u3PresenceStateMetaWrapper
    in
        expectedState `equals` (fixtures.state |> syncDiff diff)


syncDiffAddsTwoNewUsersToExistingUserSuccessfully : Test
syncDiffAddsTwoNewUsersToExistingUserSuccessfully =
    let
        joins =
            Dict.empty
                |> Dict.insert "u1" u1PresenceStateMetaWrapper

        diff =
            PresenceDiff Dict.empty joins

        expectedState =
            fixtures.empty
                |> Dict.insert "u1" u1PresenceStateMetaWrapper
                |> Dict.insert "u2" u2PresenceStateMetaWrapper

        initialState =
            Dict.empty
                |> Dict.insert "u2" u2PresenceStateMetaWrapper

        newState =
            initialState
                |> syncDiff diff
    in
        expectedState `equals` newState


listListsFullPresenceByDefault : Test
listListsFullPresenceByDefault =
    let
        initialState =
            Dict.empty
                |> Dict.insert "u1" u1PresenceStateMetaWrapperWithAddition

        result =
            initialState |> listDefault

        expected =
            [ u1PresenceStateMetaWrapperWithAddition ]
    in
        result `equals` expected


listListsWithCustomFunction : Test
listListsWithCustomFunction =
    let
        initialState : PresenceState
        initialState =
            Dict.empty
                |> Dict.insert "u1" u1PresenceStateMetaWrapperWithAddition

        listBy : String -> PresenceStateMetaWrapper -> Maybe PresenceStateMetaValue
        listBy key wrapper =
            List.head wrapper.metas

        result =
            initialState |> list listBy

        expected =
            [ Just { sampleMeta | phx_ref = "1.2" } ]
    in
        result `equals` expected


consoleTests : Test
consoleTests =
    suite "All Tests" (syncStateTests ++ syncDiffTests ++ listTests)


main =
    runSuite consoleTests
