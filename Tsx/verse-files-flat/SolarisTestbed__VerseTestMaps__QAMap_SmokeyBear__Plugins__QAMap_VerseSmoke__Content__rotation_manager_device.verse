using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }


########################################################################

# Creating a new Verse tag to easily attach to every object I want to rotate
rotation_tag:= class (tag) {}

# Rotation manager device that houses all the code OnBegin
rotation_manager_device:= class(creative_device):

    # Variable of type creative_object_interface. An interface is a set of rules that a class needs to implement. Housing all of these available into an Array
    var RotatingObjects: [] creative_object_interface = array {}

    @editable
    var RotationSpeed: float = 5.0

    @editable
    var RotateWithMoveTo: logic = true

    @editable
    ToggleRotationButton: button_device = button_device {}

    OnEnd<override>():void=
        Print("OnEnd rotation_manager_device")
    OnBegin<override>()<suspends>:void=
        ToggleRotationButton.InteractedWithEvent.Subscribe(ToggleRotation)
        Print("OnBegin rotation_manager_device")
        Print ("{LogicToString(RotateWithMoveTo)}", ?Color :=NamedColors.Yellow)
        LoopThroughRotationTagObjects()



        #####################################
        # Deprecated logic
        #####################################

        # Getting all Objects with the actiavte_rotate tag OnBegin
        # set RotatingObjects = GetCreativeObjectsWithTag (rotation_tag{})

        #     # Looping through the array of RotatingObjects
        #     for (Objects : RotatingObjects):

        #         # Cast each Object into a creative_prop from their "Grandfathered" creative_object_interface. Convert the RotatingObjects into a normal creative_prop
        #         if (Prop:= creative_prop[Objects]):

        #             if (RotateWithMoveTo = true):

        #                 # Spawning a new Asynchronyous function call for each prop using MoveTo variant
        #                 spawn:
        #                     PropRotationWithMoveTo(Prop)
        #             else:

        #                 # Spawning a new Asynchronyous function call for each prop using TeleportTo variant
        #                 spawn:
        #                     PropRotationWithTeleportTo(Prop)
        ######################################

    ToggleRotation(Agent: agent):void=
        Print("Let's try to toggle rotation")
        # if (AgentCharacter:=Agent.GetFortCharacter[]):
            if (RotateWithMoveTo?):
                Print("Successfully toggled the logic to false | ToggleRotation()")
                set RotateWithMoveTo = false
                Print ("{LogicToString(RotateWithMoveTo)}", ?Color :=NamedColors.LightBlue)
            else:
                Print("Successfully toggled the logic to true | ToggleRotation()")
                set RotateWithMoveTo = true
                Print ("{LogicToString(RotateWithMoveTo)}", ?Color :=NamedColors.Yellow)

    LoopThroughRotationTagObjects()<suspends>:void=
        set RotatingObjects = GetCreativeObjectsWithTag (rotation_tag{})
        loop:
            Sleep(3.0)
            for (Objects : RotatingObjects):
                if (Prop := creative_prop[Objects]):
                    if (RotateWithMoveTo = true):
                        spawn:
                            PropRotationWithMoveTo(Prop)
                    else:
                        spawn:
                            PropRotationWithTeleportTo(Prop)



    PropRotationWithTeleportTo<private>(PropToRotate:creative_prop)<suspends>:void=
        # loop:
            Transform:= PropToRotate.GetTransform()
            Position:= Transform.Translation
            Rotation:= Transform.Rotation
            var NewRotation : rotation = Rotation
            if (RotateWithMoveTo = false, set NewRotation = NewRotation.ApplyYaw(RotationSpeed)) {}
            if (RotateWithMoveTo = false, set NewRotation = NewRotation.ApplyRoll(RotationSpeed)) {}
            if (RotateWithMoveTo = false, set NewRotation = NewRotation.ApplyPitch(RotationSpeed)) {}
            if (PropToRotate.TeleportTo[Position, NewRotation]) { }
            Print ("[SUCCESS] TeleportTo successfully completed. | PropRotationWithTeleportTo()", ?Color:=NamedColors.LightGreen)
            Sleep(RotationSpeed)

    PropRotationWithMoveTo<private>(PropToRotate: creative_prop)<suspends>:void =
        Transform:= PropToRotate.GetTransform()
        Position:= Transform.Translation
        Rotation:= Transform.Rotation
        NewRotation:= Rotation.ApplyYaw(180.0)
        MoveResult:= PropToRotate.MoveTo(Position, NewRotation, RotationSpeed)

        if (MoveResult = move_to_result.DestinationReached):
            Print ("[SUCCESS] MoveTo successfully completed. | PropRotationWithMoveTo()", ?Color:=NamedColors.LightGreen)
            # PropRotationWithMoveTo(PropToRotate)
        else:
            Print ("[FAILURE] MoveTo NOT successfully completed. | PropRotationWithMoveTo()", ?Color:=NamedColors.Red)

    LogicToString(Logic:logic):string=

        ReturnedLogic:=if(Logic = true) 
        then "true" 
        else "false"
########################################################################
