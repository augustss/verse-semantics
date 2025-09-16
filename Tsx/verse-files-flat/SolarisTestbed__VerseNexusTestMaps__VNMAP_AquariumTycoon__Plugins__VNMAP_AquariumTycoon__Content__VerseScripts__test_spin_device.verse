
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
test_spin_device := class(nexus_test_device):

    @editable
    TestProp : creative_prop = creative_prop{}

    @editable
    TestAsset : creative_prop_asset = DefaultCreativePropAsset

    @editable
    var TestPropSpawnLoc : vector3 = vector3{X:=0.0, Y:=0.0, Z:=1152.0}

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Spawn Prop Test")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=

        (super:)OnBegin() # * All devices will need this line

        # # * You can set up/Complete tests, the logic var indicates whether or not it's a crash setup
        # SetupTest("Spawn Prop Test", false)
        # CompleteTest("Spawn Prop Test", "Spawn Prop Test completed in OnBegin()", false)

        Print("Nexus OnBegin - test_spin_device")
        SpawnTestAsset(TestPropSpawnLoc)

        # Destroying objects spawning here causes log spam: FORT-751282

        # spawn:
        #     TestSpin(TestProp)
        # spawn:
        #     SpawnPropLoop()

        
    OnEnd<override>():void =
        Print("Nexus OnEnd - test_spin_device")
        (super:)OnEnd() # * All devices will need this line

    SpawnTestAsset(SpawnLocation : vector3) : void =
        SpawnedTestProp : tuple(?creative_prop, spawn_prop_result)= SpawnProp(TestAsset, SpawnLocation, IdentityRotation())
        if (var NewPropSpawned : creative_prop = SpawnedTestProp(0)?):
            AddPass("Spawn Prop Test", "Prop was spawned at Tank Location successfully in SpawnTestAsset()")
            spawn:
                TestSpin(NewPropSpawned)
        else:
            AddFail("Spawn Prop Test", "Prop was not spawned at Tank Location in SpawnTestAsset()")

    TestSpin(SpawnedProp : creative_prop)<suspends>: void =
        loop:
            #   Without the Sleep(0.0), destroying this object causes a Runaway Loop and crashes the server: FORT-744225
            Sleep(0.0)
            Transform := SpawnedProp.GetTransform()
            Position := Transform.Translation
            Rotation := Transform.Rotation
            NewRotation := Rotation .ApplyYaw(180.0)
            SpawnedProp.MoveTo(Position, NewRotation, 3.0)

    SpawnPropLoop()<suspends>: void =
        loop:
            TaggedTanks := GetCreativeObjectsWithTag(aquariumtank{})
            RandomIndex : int = GetRandomInt(0, TaggedTanks.Length - 1)
            if(NewTankLoc := TaggedTanks[RandomIndex].GetTransform()):
                SpawnTestAsset(NewTankLoc.Translation)
            Sleep(0.5)