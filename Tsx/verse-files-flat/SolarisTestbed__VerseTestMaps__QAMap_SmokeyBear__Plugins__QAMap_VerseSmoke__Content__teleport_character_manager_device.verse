using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Colors }


teleport_character_manager_device := class(creative_device):

    @editable
    TeleportPointOut : creative_prop = creative_prop {}
    
    @editable
    MutatorTeleport: mutator_zone_device = mutator_zone_device {}


    OnEnd<override>():void=
        Print("OnEnd teleport_character_manager_device")

    OnBegin<override>()<suspends>:void=
        Print("OnBegin teleport_character_manager_device")
        
        MutatorTeleport.AgentEntersEvent.Subscribe(TeleportOut)
        
    # Tests TeleportTo on the Character
    TeleportOut(Agent : agent):void=
        Print("Teleporting Player")
        if(TeleportPointOut.IsValid[], AgentCharacter := Agent.GetFortCharacter[]):
            PointTransform := TeleportPointOut.GetTransform()
            Position := PointTransform.Translation
            if(AgentCharacter.TeleportTo[Position , IdentityRotation()]):
                Print("[SUCCESS] Teleported to {Position.X}, {Position.Y}, {Position.Z} | TeleportOut()", ?Color:=NamedColors.LightGreen)
            else Print("[FAILURE] Can't teleport player to point, did we fail the collision test? If yes, this is not a issue. | TeleportOut()", ?Color:=NamedColors.Red)
        else Print("[FAILURE] Teleport Point in doesn't exist | TeleportOut()", ?Color:=NamedColors.Red)