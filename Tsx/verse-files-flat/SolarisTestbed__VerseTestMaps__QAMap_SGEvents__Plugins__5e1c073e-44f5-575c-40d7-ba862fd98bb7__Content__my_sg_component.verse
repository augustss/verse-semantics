using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SceneGraph }

######################################################################
# Verse Scene Graph Events introduces scene_event(s)                 #
# which are events that can be propagated through the scene graph.   #
# Scene Events are handled by Entities or Components.                #
# This is a Verse QA Demo Map to showcase the feature                #
# QA Owner: @branden.lee                                             #
# Dev Owner: @eric.frutcher                                          #
######################################################################

#################################################
# custom classes and interfaces which inherit   #
# from the scene_event interface                #            
#################################################
sg_entity := class<final_super>(entity, telemetry_scene_event):
    SendMyEvent(SE : scene_event):void=
        SendDirect(SE)

telemetry_scene_event := interface(scene_event):
    SomeMethod():void=block{}
    SomeAny : any = "Any String"
    SomeRational : rational = 3
    SomeVoid : void = void
    SomeComparable : comparable = 1

telemetry_scene_event_hit := class(telemetry_scene_event):
    Damage : float = 0.0

#################################################
# sg_receiver component which controls what to  #
# do with the received events                   #
#################################################
sg_receiver := class<final_super>(component):
    @editable var Consumes<public>:logic = false
    @editable var Name<public>:string = ""

    OnReceive<override>(SceneEvent:scene_event):logic =
        var Consumed : logic = (super:)OnReceive(SceneEvent)

        if (HitEvent := telemetry_scene_event_hit[SceneEvent]):
            Print("Telemetry Hit on {Name} for {HitEvent.Damage} Dmg.")
            if (Consumes?):
                set Consumed = true
        else if (Telemetry := telemetry_scene_event[SceneEvent]):
            Print("Unrecognized telemetry event on {Name}.")
            if (Consumes?):
                set Consumed = true
        return Consumed
        
#################################################
# sg_sender component which controls the        #
# sent events                                   #
#################################################
sg_sender := class<final_super>(component):
    @editable var SomeInt : int = 1
    @editable var SomeLogic : logic = false
    @editable var SomeFloat : float = 1.0
    @editable var SomeString : string = "String"
    var Any : any = "Any String"
    var Rational : rational = 3
    var Void : void = void
    var Comparable : comparable = 1

    @editable var SendUp : logic = false
    @editable var SendDown : logic = false
    @editable var SendDirect : logic = false
    @editable var DamageAmount : float = 10.0

    OnSimulate<override>()<suspends>:void =
        Print("OnSimulate")
        loop:
            Sleep(5.0)
            if (SendUp?):
                Entity.SendUp(telemetry_scene_event_hit{Damage := DamageAmount})
            if (SendDown?):
                Entity.SendDown(telemetry_scene_event_hit{Damage := DamageAmount})
            if (SendDirect?):
                Entity.SendDirect(telemetry_scene_event_hit{Damage := DamageAmount})






