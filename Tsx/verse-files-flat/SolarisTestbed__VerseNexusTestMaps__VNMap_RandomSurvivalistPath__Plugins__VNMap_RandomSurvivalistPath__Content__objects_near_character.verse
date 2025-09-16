using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }

objects_near_character := class<concrete>:

    @editable
    PropMoverDevices<private> : []prop_mover_device = array{}

    var PropMoveEventCounter<private> : int = 0
    var MainLoopCounter<private> : int = 0 
    var ExpectedAgent : ?agent = false

    var CancelFinishedEvent<private> : ?cancelable = false
    var CancelBeganEvent<private> : ?cancelable = false

    @editable
    VFXDevices<private> : []vfx_creator_device = array{}

    var CampfireDevice : campfire_device = campfire_device{}

    ZComponentAgent<private> : float = 93.0

    OnPropMoverFinishedEvent<private>():void=
        set PropMoveEventCounter += 1
        Print("NexusInfo - OnPropMoverFinishedEvent fires in objects_near_character")    

    OnPropMoverBeganEvent<private>():void=
        set PropMoveEventCounter += 1
        Print("NexusInfo - OnPropMoverBeganEvent fires in objects_near_character")    

    TeleportObjectNearToPlayer(Object : creative_object, Agent : agent)<transacts><suspends>:void=
        RandomFloatX := GetRandomFloat(-500.0, 500.0)
        RandomFloatY := GetRandomFloat(-500.0, 500.0)

        # To not spawn object so close to player
        if: 
            block:
                -250.0 > RandomFloatX or RandomFloatX > 250.0, -250.0 > RandomFloatY or RandomFloatY > 250.0
            Object.TeleportTo[(Agent.GetFortCharacter[].GetTransform().Translation + vector3{X:= RandomFloatX, Y:= RandomFloatY, Z:= -ZComponentAgent}), Agent.GetFortCharacter[].GetTransform().Rotation]
            MaybeAnotherVariable := Shuffle(0.0, 1.0)
            Print("NexusInfo - MaybeAnotherVariable: {MaybeAnotherVariable[0]}. If 1.0 then teleportation should rollback")
            Object.GetTransform().Translation.X = (Object.GetTransform().Translation.X + MaybeAnotherVariable[0])

        else:
            TeleportObjectNearToPlayer(Object, Agent)

    TeleportObjectToPlayer(Object : creative_object, Agent : agent)<suspends>:void=
        logic{Object.TeleportTo[(Agent.GetFortCharacter[].GetTransform().Translation), Agent.GetFortCharacter[].GetTransform().Rotation]}

    PlaceNecessaryObjects(Agent:agent, BaseLocationSetupEvent : event()):void=
        spawn{PlaceNecessaryObjectsSuspends(Agent, BaseLocationSetupEvent)}

    PlaceNecessaryObjectsSuspends(Agent:agent, BaseLocationSetupEvent : event())<suspends>:void=
        Print("PlaceNecessaryObjectsSuspends")            
        set ExpectedAgent = option{Agent}
        if (PropMoverDevices.Length <> 2 or VFXDevices.Length <> 2):
            Print("NexusError - assigned more or less then two existing prop movers or vfx in objects_near_character class")
            
        TaggedActors := GetCreativeObjectsWithTag(necessary_objects_nearcharacter{})
        if (PropMoverDevices.Length <> 2):
            Print("NexusError - assigned more or less then two existing prop with tags in objects_near_character class")
        
        TeleportObjectNearToPlayer(CampfireDevice, Agent)

        Print("object_near_character - main loop")               
        for (Index := 0..TaggedActors.Length):
            set MainLoopCounter += 1
            set CancelFinishedEvent = option{PropMoverDevices[Index].FinishedEvent.Subscribe(OnPropMoverFinishedEvent)}
            set CancelBeganEvent = option{PropMoverDevices[Index].BeganEvent.Subscribe(OnPropMoverBeganEvent)}
            if (Prop := creative_prop[TaggedActors[Index]], PropMover := PropMoverDevices[Index], VFX := VFXDevices[Index]):
                block:
                    Prop.Hide()
                    TeleportObjectNearToPlayer(Prop, Agent)
                    MoveProps(Prop, PropMover)
                    Sleep(0.0)
                    PropMover.Begin()
                    SetupVFX(VFX, Prop)
                    Prop.Show()

                    if(CancelBeganEvent? and CancelFinishedEvent?):
                        if (BeganSubscription := CancelBeganEvent?):
                            BeganSubscription.Cancel()
                            Print("NexusInfo - BeganSubscription prop mover event canceled in objects_near_character class")    
                        else:
                            Print("NexusError - cannot unsubcribe BeganSubscription prop mover events in objects_near_character class")    
                        if (FinishedSubscription := CancelFinishedEvent?):   
                            Print("NexusInfo - FinishedSubscription prop mover event canceled in objects_near_character class")    
                            FinishedSubscription.Cancel()
                        else:
                            Print("NexusError - cannot unsubcribe FinishedSubscription prop mover events in objects_near_character class")    
                    else:
                        Print("NexusError - cannot unsubcribe prop mover events in objects_near_character class")    
                    
        if(PropMoveEventCounter = 0):
            Print("NexusError - prop mover events didn't stop in objects_near_character's PlaceNecessaryObjectsSuspends function of main_game_device. Event counter: {PropMoveEventCounter}")            
        
        BaseLocationSetupEvent.Signal()  

    SetupVFX(VFX : vfx_creator_device, Prop : creative_prop)<suspends>:void=
        VFX.Enable()
        TeleportObjectToProp(VFX, Prop)
        VFX.Begin()

    TeleportObjectToProp<private>(Object : creative_object, ObjectTo : creative_object)<suspends>:void=      
        logic{Object.TeleportTo[ObjectTo.GetTransform().Translation, ObjectTo.GetTransform().Rotation]}    
   
    MoveProps<private>(Prop : creative_prop, PropMover : prop_mover_device)<suspends>:void=
        if (Agent := ExpectedAgent?):
            TeleportObjectNearToPlayer(Prop, Agent)
            TeleportObjectToProp(PropMover, Prop)            