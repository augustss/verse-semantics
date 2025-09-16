# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Tests/SolarisTestbedScripts/TestModule }
using { /Verse.org/Restricted }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

Main<native_callable>(Payload:solaris_test_payload):void=
{
    #spawn {_TestCoro(2f)}
    Context1 := spawn {TestConcurrency()}
    #spawn {_TestNestedConcurrency()}
    Context2 := spawn {TestHit()}
    spawn {TestEvent()}
    spawn {TestLoop()}

    if (Cube := SolarisTestCubeLib.GetTestCube[])
    {
        if (Cube.GetRotationSpeed() > 0.0)
        {
            SolarisTestCubeLib.SetCubeStyle(Cube, cube_style.Cone)
            Cube.NonNativePolyFillStyleTest(0.0)
        }
        else
        {
            SolarisTestCubeLib.SetCubeRotationSpeed(Cube, 42.0)
            SolarisTestCubeLib.SetCubeStyle(Cube, cube_style.Cube)
        }
    }

    for (Actor:ImportedTestbedActorLib.FindAllTestbedActors())
    {
        if (AsScriptActor := sol_script_actor[Actor])
        {
            AsScriptActor.Print()
        }
    }

    set Payload.StringA = "Test string"

    ModuleTests()
}


Test_ArrayInterop<native_callable><public>(Strings:[]string):void=
{
    SolarisTestbedLogPrint(Concatenate(Strings))
}


TestCoro<public>(InWaitTime:float)<suspends>:float=
{
    var WaitTime:float = InWaitTime

    LocalValue := 1138.0
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Tic!")
    set WaitTime = TestCoro2(WaitTime)
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Tac!")
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Toe!")
    return (LocalValue)
}


TestCoro2<public>(WaitTime:float)<suspends>:float=
{
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Fee!")
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Fi!")
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Fo!")
    Sleep(WaitTime)
    SolarisTestbedLogPrint("Fum!")
    return (WaitTime * 2.0)
}


TestConcurrency<public>()<suspends>:void=
{
    SolarisTestbedLogPrint("A")
    sync
    {
        block
        {
            SolarisTestbedLogPrint("B")
            Sleep(1.0)
            SolarisTestbedLogPrint("C")
            42
        }

        block
        {
            SolarisTestbedLogPrint("D")
            Sleep(0.5)
            SolarisTestbedLogPrint("E")
            4711
        }

        block
        {
            SolarisTestbedLogPrint("F")
            Sleep(1.5)
            SolarisTestbedLogPrint("G")
            1138
        }
    }
    SolarisTestbedLogPrint("H")

    SolarisTestbedLogPrint("a")
    RaceResult := race
    {
        block
        {
            SolarisTestbedLogPrint("b")
            Sleep(1.0)
            SolarisTestbedLogPrint("c")
            99
        }

        block
        {
            SolarisTestbedLogPrint("d")
            Sleep(0.5)
            SolarisTestbedLogPrint("e")
            66
        }

        block
        {
            SolarisTestbedLogPrint("f")
            Sleep(1.5)
            SolarisTestbedLogPrint("g")
            33
        }
    }
    SolarisTestbedLogPrint("h")
    SolarisTestbedLogPrint("RaceResult := {RaceResult}")

    SolarisTestbedLogPrint("A")
    RushResult := rush
    {
        block
        {
            SolarisTestbedLogPrint("B")
            Sleep(1.0)
            SolarisTestbedLogPrint("C")
            9000
        }

        block
        {
            SolarisTestbedLogPrint("D")
            Sleep(0.5)
            SolarisTestbedLogPrint("E")
            8000
        }

        block
        {
            SolarisTestbedLogPrint("F")
            Sleep(1.5)
            SolarisTestbedLogPrint("G")
            7000
        }
    }
    SolarisTestbedLogPrint("H")
    SolarisTestbedLogPrint("RushResult := {RushResult}")

    SolarisTestbedLogPrint("the")
    branch
    {
        SolarisTestbedLogPrint("quick")
        Sleep(0.5)
        SolarisTestbedLogPrint("brown")
        Sleep(0.5)
        SolarisTestbedLogPrint("fox")
    }
    SolarisTestbedLogPrint("jumped over the lazy blockg")

    Sleep(1.75)
}


TestNestedConcurrency<public>()<suspends>:void=
{
    SolarisTestbedLogPrint("A")
    sync
    {
        block
        {
            SolarisTestbedLogPrint("B")
            Sleep(1.0)
            SolarisTestbedLogPrint("C")
            race
            {
                block
                {
                    SolarisTestbedLogPrint("D")
                    Sleep(0.0)
                    SolarisTestbedLogPrint("E")
                    Sleep(1.0)
                    SolarisTestbedLogPrint("F")
                    17
                }

                block
                {
                    SolarisTestbedLogPrint("G")
                    Sleep(0.5)
                    SolarisTestbedLogPrint("H")
                    15
                }
            }
        }

        block
        {
            SolarisTestbedLogPrint("I")
            Sleep(1.2)
            SolarisTestbedLogPrint("J")
            4711
        }
    }
    SolarisTestbedLogPrint("K")
}

<#>
    Results in following output:

    logMsgVerseCore: Default: _testEvent() - entered
    logMsgVerseCore: Default: Task A - begin
    logMsgVerseCore: Default: Task A - suspending (immediate)
    logMsgVerseCore: Default: Task B - begin
    logMsgVerseCore: Default: Task C - begin
    logMsgVerseCore: Default: Task event control - begin
    logMsgVerseCore: Default: Task C - suspending (after .5)
    logMsgVerseCore: Default: Task B - suspending (after 1.5f)
    logMsgVerseCore: Default: Task event control - signal (after 2)
    logMsgVerseCore: Default: Task A - resumed
    logMsgVerseCore: Default: Task A - completed (immediate post signal)
    logMsgVerseCore: Default: Task C - resumed
    logMsgVerseCore: Default: Task B - resumed
    logMsgVerseCore: Default: Task event control - continuing
    logMsgVerseCore: Default: Task C - complete (after 1.5f post signal)
    logMsgVerseCore: Default: Task B - complete (after 2 post signal)
    logMsgVerseCore: Default: Task event control - complete (after 3 post signal)
    logMsgVerseCore: Default: _testEvent() - exited


TestEvent<public>()<suspends>:void=
{
    SolarisTestbedLogPrint("_testEvent() - entered")

    # Create event
    Event := event(){}

    # Run 4 Tasks simultaneously
    sync:
        block: # Task A
            SolarisTestbedLogPrint("Task A - begin")
            SolarisTestbedLogPrint("Task A - suspending (immediate)")
            Event.Await()
            SolarisTestbedLogPrint("Task A - resumed")
            SolarisTestbedLogPrint("Task A - completed (immediate post signal)")

        block: # Task B
            SolarisTestbedLogPrint("Task B - begin")
            Sleep(1.5)
            SolarisTestbedLogPrint("Task B - suspending (after 1.5f)")
            Event.Await()
            SolarisTestbedLogPrint("Task B - resumed")
            Sleep(2.0)
            SolarisTestbedLogPrint("Task B - complete (after 2 post signal)")

        block: # Task C
            SolarisTestbedLogPrint("Task C - begin")
            Sleep(0.5)
            SolarisTestbedLogPrint("Task C - suspending (after .5)")
            Event.Await()
            SolarisTestbedLogPrint("Task C - resumed")
            Sleep(1.5)
            SolarisTestbedLogPrint("Task C - complete (after 1.5f post signal)")

        block: # Task event control
            SolarisTestbedLogPrint("Task event control - begin")
            Sleep(2.0)
            SolarisTestbedLogPrint("Task event control - signal (after 2)")
            Event.Signal()
            SolarisTestbedLogPrint("Task event control - continuing")
            # Complete Task after 3 seconds
            Sleep(3.0)
            SolarisTestbedLogPrint("Task event control - complete (after 3 post signal)")

    SolarisTestbedLogPrint("_testEvent() - exited")
}


TestHit<public>()<suspends>:void=
{
    if(Cube := SolarisTestCubeLib.GetTestCube[])
    {
        loop
        {
            Cube.WaitHit()
            SolarisTestbedLogPrint("Ouch!")
        }
    }
}


TestLoop<public>()<suspends>:void=
    SolarisTestbedLogPrint("_testLoop() - entered")

    var Idx:int = 0

    loop:
        var InnerIdx:int = 0
        Sleep(1.0)
        set Idx += 10
        SolarisTestbedLogPrint("loop outer idx:{Idx}")

        # immediate inner loop
        loop:
            set InnerIdx += 1
            SolarisTestbedLogPrint("loop inner idx:{InnerIdx}")

            if (InnerIdx = 5):
                SolarisTestbedLogPrint("loop inner break")
                break

        if (Idx = 100):
            SolarisTestbedLogPrint("loop outer break")
            break

    SolarisTestbedLogPrint("_testLoop() - exited")

ModuleTests<public>():void=
    SolarisTestbedLogPrint("moduleTests() - entered")

    Tc := test_class{}

    # default calls
    # These will all be semantic errors KillTaskuncommented - VS Code should even show error squiggle;
    #SolarisTestbedLogPrint("dataDefault: {Tc.DataDefault}")
    #SolarisTestbedLogPrint("dataClassDefault: {Tc.DataClassDefault}")
    #SolarisTestbedLogPrint("methodDefault(): {Tc.MethodDefault()}")
    #SolarisTestbedLogPrint("methodClassDefault(): {Tc.MethodClassDefault()}")

    # public calls
    SolarisTestbedLogPrint("dataPublic: {Tc.DataPublic}")
    SolarisTestbedLogPrint("dataClassPublic: {TestClass.DataClassPublic}")
    SolarisTestbedLogPrint("methodPublic(): {Tc.MethodPublic()}")
    SolarisTestbedLogPrint("methodClassPublic(): {Tc.MethodClassPublic()}")

    # internal calls
    # These will all be semantic errors if uncommented - VS Code should even show error squiggle
    #SolarisTestbedLogPrint("dataInternal: {tc.DataInternal}")
    #SolarisTestbedLogPrint("dataClassInternal: {tc.DataClassInternal}")
    #SolarisTestbedLogPrint("methodInternal(): {tc.MethodInternal()}")
    #SolarisTestbedLogPrint("methodClassInternal(): {tc.MethodClassInternal()}")

    SolarisTestbedLogPrint("moduleTests() - exited")

log_solaris_testbed<internal>:=class(log_channel){}

SolarisTestbedLogPrint(Message:string):void=log{Channel:=log_solaris_testbed}.Print(Message)