using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }

tag_cakes<public> := class(tag){}

cake_callback := type {_(:cake):void}

cake := class<unique>:

    
    @editable
    AutoPickerDevice : autopick_manager

    cakepropasset : creative_prop_asset
    var cakeprop : creative_prop
    OldTransform : transform
    FortCharacter: fort_character
    OnCakeCollected : cake_callback

    var IsRespawning : logic = false
    pickup_distance : float = 500.0

    InitCakes():void=

                                                                                                                            spawn:
                                                                                                                                PickupLoop()

    PickupLoop()<suspends>:void=
        # Depending on the distance of the cake prop to the players location, we will pick up the cake
        loop:
                                                                                                                            if (IsRespawning?):
                                                                                                                                break
                                                                                                                            
                                                                                                                            TrackingDistance := Distance(FortCharacter.GetTransform().Translation, cakeprop.GetTransform().Translation)
                                                                                                                            if (TrackingDistance <= pickup_distance):
                                                                                                                            
                                                                                                                                PickupCake()
                                                                                                                                break

                                                                                                                            Sleep(0.3)

    PickupCake()<suspends>:void=
        if (cakeprop.IsValid[]):
            # If the cake prop is still valid we will move the cake to the player and collect the cake. Increasing our cake count. We also call the cake to respawn after a delay.
                                                                                                                            set IsRespawning = true
                                                                                                                            # Passing in the cake class
                                                                                                                            OnCakeCollected(Self)
                                                                                                                            MoveResult := cakeprop.MoveTo(FortCharacter.GetTransform(), 0.5)
                                                                                                                            
                                                                                                                            if(MoveResult = move_to_result.DestinationReached, Agent:= FortCharacter.GetAgent[]):
                                                                                                                                AutoPickerDevice.OnCakeCollected(Agent)
                                                                                                                                cakeprop.Dispose()
                                                                                                                                

                                                                                                                                spawn:
                                                                                                                                    RespawnCake()
                                                                                                                            else:
                                                                                                                                Print ("NexusError - autopickerupclasses.PickupCake() | move_to_result.DestinationReached failed to reach the players location successfully")
                                                                                                                                RespawnCake()
        else:
            Print("NexusError - The Cake Prop is NOT valid when it should be valid PickupCake")

    RespawnCake()<suspends>:void=
        # Delay then we spawn the cake prop and move it to it's old transform to create a sort of "growing" look as it scales back up
        Sleep(3.0)
        NewTransform := transform{Translation:= OldTransform.Translation, Rotation:= OldTransform.Rotation, Scale:= vector3{X:= 0.1, Y:= 0.1, Z:= 0.1}}
        # Making sure the first element is valid and then unwrapping the optional value
                                                                                                                            if (SpawnedCake := SpawnProp(cakepropasset, NewTransform)(0)?):
                                                                                                                                set cakeprop = SpawnedCake
                                                                                                                                cakeprop.MoveTo(OldTransform, 1.5)
                                                                                                                                set IsRespawning = false
                                                                                                                            else:
                                                                                                                                Print("NexusError - autopickerupclasses.RespawnCake() | Cakes could not respawn from SpawnProp")
            spawn:
                PickupLoop()



    