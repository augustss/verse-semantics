# Copyright Epic Games, Inc. All Rights Reserved.
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/VerseInternalTesting/VerseFramework }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

physics_force_test_device := class(test_device):

    ##########################################################
    # GLOBALS
    ##########################################################

    @editable
    GravityTestTrigger:trigger_device = trigger_device{}
    @editable
    DynamicPlatformTrigger:trigger_device = trigger_device{}
    @editable
    StaticPlatformTrigger:trigger_device = trigger_device{}
    @editable
    PropMover_StaticWall:prop_mover_device = prop_mover_device{}
    @editable
    PropMover_DynamicWall:prop_mover_device = prop_mover_device{}

    GravityTriggerEventMock:device_event_optional_agent_handler_mock = device_event_optional_agent_handler_mock{}
    DynamicPlatformTriggerEventMock:device_event_optional_agent_handler_mock = device_event_optional_agent_handler_mock{}
    StaticPlatformTriggerEventMock:device_event_optional_agent_handler_mock = device_event_optional_agent_handler_mock{}

    var OptionalAgent:?agent = false

    ##########################################################
    # TEST CLASS SETUP
    ########################################################

    BeforeAll<override>()<suspends>:void=
        GravityTriggerEventMock.Subscribe(GravityTestTrigger.TriggeredEvent)
        DynamicPlatformTriggerEventMock.Subscribe(DynamicPlatformTrigger.TriggeredEvent)
        StaticPlatformTriggerEventMock.Subscribe(StaticPlatformTrigger.TriggeredEvent)

        set OptionalAgent = option:
            Players:= Self.GetPlayspace().GetPlayers()
            Players[Players.Length - 1]
        
        TeleportAllTestPawns()

    AfterAll<override>()<suspends>:void=
        PropMover_StaticWall.Reset()
        PropMover_DynamicWall.Reset()
        GravityTriggerEventMock.ResetCallCount()
        DynamicPlatformTriggerEventMock.ResetCallCount()
        StaticPlatformTriggerEventMock.ResetCallCount()
    
    TeleportAllTestPawns():void=
        Playspace : fort_playspace = GetPlayspace()
        AllParticipants := Playspace.GetParticipants()

        # First pawn is teleported 10 meters in the air 
        if:
            FortChar := AllParticipants[1].GetFortCharacter[]
        then:
            if (FortChar.TeleportTo[vector3{X := -12.510872, Y := -497.977725, Z := 1000.0}, IdentityRotation()]):
                Print("Test player pawn 1 was teleported to 10 meters up for the Gravity test.")
            else:
                Print("Cannot teleport  test player pawn 1")
        else:
            Print("Cannot find the test player pawn 1")

        # Second pawn is teleported to the moveable platform
        if:
            FortChar := AllParticipants[2].GetFortCharacter[]
        then:
            if (FortChar.TeleportTo[vector3{X := 508.733702, Y := -497.977725, Z := 410.0}, IdentityRotation()]):
                Print("Test player pawn 2 was teleported to the moveable platform")
            else:
                Print("Cannot teleport  test player pawn 2")
        else:
            Print("Cannot find the test player pawn 2")


        # Third pawn is teleported to the static platform
        # Currently bugged due to https://jira.it.epicgames.com/browse/FORT-894018, for now we are using the player agent
        if:
            FortChar := AllParticipants[0].GetFortCharacter[]
        then:
            if (FortChar.TeleportTo[vector3{X := 1544.889298, Y := -425.619157, Z := 400.0}, IdentityRotation()]):
                Print("Test player pawn 3 was teleported to the static platform")
            else:
                Print("Cannot teleport  test player pawn 3")
        else:
            Print("Cannot find the test player pawn 3")
    
    ##########################################################
    # TEST CASES
    ##########################################################
    SuiteName<override>:string = "PhysicsForceTestSuite"

    TestCases<override>():[]test_case =
        array:
            test_case:
                Name := "TestPhysicsForce_AgentFallingGravity"
                Func := TestPhysicsForce_AgentFallingGravity
                Category := array{"editor"}
                TimeoutOverride := option{10.0}
            test_case:
                Name := "TestPhysicsForce_AgentPushedStaticWall"
                Func := TestPhysicsForce_AgentPushedStaticWall
                Category := array{"editor"}
                TimeoutOverride := option{10.0}
            test_case:
                Name := "TestPhysicsForce_AgentPushedDynamicWall"
                Func := TestPhysicsForce_AgentPushedDynamicWall
                Category := array{"editor"}
                TimeoutOverride := option{10.0}


    ##########################################################
    # TEST FUNCTIONS
    ##########################################################
    TestPhysicsForce_AgentFallingGravity()<suspends>:void=
        GravityTriggerEventMock.Await()
        TestRunner.CheckEqual(1, GravityTriggerEventMock.GetCallCount(), "Checking Agent gravity when falling from a height")

    TestPhysicsForce_AgentPushedStaticWall()<suspends>:void=
        PropMover_StaticWall.Begin()

        DynamicPlatformTriggerEventMock.Await()
        TestRunner.CheckEqual(1, DynamicPlatformTriggerEventMock.GetCallCount(), "Checking Agent was knocked of a platform by a static wall")

    TestPhysicsForce_AgentPushedDynamicWall()<suspends>:void=
        PropMover_DynamicWall.Begin()

        StaticPlatformTriggerEventMock.Await()
        TestRunner.CheckEqual(1, StaticPlatformTriggerEventMock.GetCallCount(), "Checking Agent was knocked of a platform by a dynamic wall")