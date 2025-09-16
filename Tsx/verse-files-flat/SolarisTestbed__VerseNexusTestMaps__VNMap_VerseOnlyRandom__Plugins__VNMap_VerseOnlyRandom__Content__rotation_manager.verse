
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
# Creating a new Verse tag to easily attach to every object I want to rotate
activate_rotate<public> := class (tag) {}

# Rotation manager device that houses all the code OnBegin
rotation_manager := class(creative_device):
    # Variable of type creative_object_interface. An interface is a set of rules that a class needs to implement. Housing all of these available into an Array
    var RotatingObjects : []creative_object_interface = array {}

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - rotation_manager")
        # Getting all Objects with the actiavte_rotate tag OnBegin
        set RotatingObjects = GetCreativeObjectsWithTag (activate_rotate{})
        block:
            # Looping through the array of RotatingObjects
            for (Obj : RotatingObjects):
                # Cast each Object into a creative_prop from their Grandfathered creative_object_interface. Convert the RotatingObjects into a normal creative_prop
                if (Prop := creative_prop[Obj]):
                    # Spawning a new Asynchronyous function call for each prop
                    spawn:
                        SmoothRotateProp(Prop)
    OnEnd<override>():void=
        Print ("Nexus OnEnd - rotation_manager")
    SmoothRotateProp<private>(PropToRotate: creative_prop)<suspends> : void =
            Transform := PropToRotate.GetTransform()
            Position := Transform.Translation
            Rotation := Transform.Rotation
            Sleep(0.1)
            NewRotation := Rotation.ApplyYaw(180.0)
            MoveResult := PropToRotate.MoveTo(Position, NewRotation, 0.3)
            # Repeating the MoveTo once it's completed/destination is reached for smoother movement
            if (MoveResult = move_to_result.DestinationReached):
                # Print ("Destination reached, starting prop rotation again")
                Print("Destination reached, ending prop rotation")
                # SmoothRotateProp(PropToRotate)
            else:
                Print("NexusError - rotation_manager.SmoothRotateProp(PropToRotate : creative_prop) | move_to_result.DestinationReached failed")