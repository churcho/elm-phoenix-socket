module Tests exposing (..)

import ElmTest exposing (..)
import Presence exposing (syncState, syncDiff, PresenceState, PresenceDiff)
import Dict


syncStateTests : List Test
syncStateTests =
    [ syncStateSyncsEmptyState
    ]


syncDiffTests : List Test
syncDiffTests =
    [ syncDiffSyncsEmptyState
    ]


syncStateSyncsEmptyState : Test
syncStateSyncsEmptyState =
    let
        state =
            Dict.empty

        newState =
            Dict.empty
                |> Dict.insert "knewter" knewterPresenceStateMetaWrapper

        knewterPresenceStateMetaWrapper =
            { metas =
                [ { phx_ref = "1"
                  , online_at = "sometime"
                  , device = "browser"
                  }
                ]
            }
    in
        newState `equals` (state |> syncState newState)


syncDiffSyncsEmptyState : Test
syncDiffSyncsEmptyState =
    let
        state =
            Dict.empty

        joins =
            Dict.empty
                |> Dict.insert "knewter" knewterPresenceStateMetaWrapper

        diff =
            PresenceDiff Dict.empty joins

        knewterPresenceStateMetaWrapper =
            { metas =
                [ { phx_ref = "1"
                  , online_at = "sometime"
                  , device = "browser"
                  }
                ]
            }
    in
        joins `equals` (state |> syncDiff diff)


consoleTests : Test
consoleTests =
    suite "All Tests" (syncStateTests ++ syncDiffTests)


main =
    runSuite consoleTests
