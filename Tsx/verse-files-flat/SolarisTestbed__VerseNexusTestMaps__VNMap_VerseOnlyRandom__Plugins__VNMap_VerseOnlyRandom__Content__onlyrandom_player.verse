using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }
newonlyrandom_player := class<unique>():
    var CurrentElims : int = 0
    var Class : int = 1
    Player : player

    SetClass (InClass : int):void=
        set Class = InClass
    
    GetClass():int=
        return Class
        
onlyrandom_player:= class<unique>():
# For our Tycoon stuff


# For our OnlyUp Elevator stuff
    Player : player
    PetAsset : creative_prop_asset
    var PetProp<private> : creative_prop = creative_prop{}


# Init functions that spawns a petasset at a specific transform near the player. This should apply for each player that spawns in the level
    Init():void=
        if:
            Agent := agent[Player]
            FortPlayer := Agent.GetFortCharacter[]
        then:
            Scale := 1.0
            Tran := transform{Rotation:= FortPlayer.GetViewRotation(),
            Translation:=FortPlayer.GetTransform().Translation + vector3{Y:=120.0},
            Scale := vector3{X:=Scale,Y:=Scale, Z:= Scale}}
            if (Prop := SpawnProp(PetAsset, Tran)(0)?):
                set PetProp = Prop
                Print ("Successfully init player Pet")
                spawn {MovePet(FortPlayer)}
            else:
                Print("Nexus Error - onlyrandom_player.Init() | SpawnProp failed, a player pet could not Spawn")

    Dispose():void=
        PetProp.Dispose()
        
# Moves the players associated pet to their location with a small offset
    MovePet<private>(FortCharacter : fort_character)<suspends>: void=
        loop:
            # Breaking if the pet is not valid
            if (not PetProp.IsValid[]):
                break

            Scale:= 1.0
            Tran := transform{Rotation:= FortCharacter.GetViewRotation(), Translation:=FortCharacter.GetTransform().Translation + vector3{Y:=120.0}, Scale := vector3{X:=Scale, Y:=Scale, Z:= Scale}}

            PetProp.MoveTo(Tran, 0.5)
    