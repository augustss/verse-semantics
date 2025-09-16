
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
meteor_trap_device := class(obstacle_device):

    @editable
    MeteorPropAsset:creative_prop_asset=DefaultCreativePropAsset

    var SpawnedMeteors:[]?creative_prop = array{}

    SpawnRange:min_max_vector3= min_max_vector3{Minimum:=vector3{X:=-4159.0,Y:=-4998.096428, Z:=3000.0}, Maximum:=vector3{X:=4000.0, Y:=5000.0, Z:=4000.0}}

    @editable
    MeteorSpeedFloat:float= 3.0

    @editable
    MeteorExplodeZFloat :float = 69.0

    @editable
    MeteoritesPerWave:int = 25

    var IsRoundGoing : logic = false

    var ExplosiveDevices : []explosive_device = array{} 
    var ExplosionDeviceIndex : int = 0

    @editable
    GameManager:game_manager = game_manager{}

    var BigVectorArray:[]vector3 = array{}
    # Local storage of players to instigate explosions
    var Players:[]player = array{}

    OnBegin<override>()<suspends>:void=

        TaggedExplosives := GetCreativeObjectsWithTag(explosives_tag{})

        for(Explosive : TaggedExplosives, NewExplosive : explosive_device = explosive_device[Explosive]):
            NewExplosiveDevices:= set ExplosiveDevices += NewExplosiveDevice: []explosive_device = array{explosive_device(NewExplosive)} 

        Print("Explosive devices found: {ExplosiveDevices.Length}")


    SpawnMeteorProps()<suspends>:void=

        ZeroRotation : rotation = MakeRotation(vector3{X:=0.0, Y:=0.0, Z:=0.0}, 0.0)
        loop:
            Sleep(1.0)
            for(Index:=0..MeteoritesPerWave):
                NewMeteor:= SpawnProp(MeteorPropAsset, GetRandomSpawnPosition(), ZeroRotation)
                var SpawnMessageString:string = "Prop Spawned: "
                
                case(NewMeteor(1)):
                    spawn_prop_result.Ok => 
                        set SpawnMessageString += "OK"
                    spawn_prop_result.UnknownError => 
                        set SpawnMessageString += "UnknownError"
                    spawn_prop_result.InvalidSpawnPoint => 
                        set SpawnMessageString += "InvalidSpawnPoint"
                    spawn_prop_result.SpawnPointOutOfBounds => 
                        set SpawnMessageString += "SpawnPointOutOfBounds"
                    spawn_prop_result.InvalidAsset => 
                        set SpawnMessageString += "InvalidAsset"
                    spawn_prop_result.TooManyProps => 
                        set SpawnMessageString += "TooManyProps"

                
                set SpawnedMeteors += array{NewMeteor(0)}
                
                var LocalBigVectorArray : []vector3 = array{}
                if(NewMeteorProp:=NewMeteor(0)?):
                    LocationString:string = VectorToString(NewMeteorProp.GetTransform().Translation)
                    Print(SpawnMessageString + " at location " + LocationString)
                    set BigVectorArray += array{NewMeteorProp.GetTransform().Translation}
                    set LocalBigVectorArray = BigVectorArray + BigVectorArray
                    Sleep(0.05)

    GetRandomSpawnPosition():vector3=

        return vector3{
            X:=GetRandomFloat(SpawnRange.Minimum.X, SpawnRange.Maximum.X), 
            Y:= GetRandomFloat(SpawnRange.Minimum.Y, SpawnRange.Maximum.Y), 
            Z:= GetRandomFloat(SpawnRange.Minimum.Z, SpawnRange.Maximum.Z)
            }

    ActivateMeteorsFalling()<suspends>:void=

        ZeroRotation : rotation = MakeRotation(vector3{X:=0.0, Y:=0.0, Z:=0.0}, 0.0)
        Sleep(1.0)
        loop:
            Sleep(0.0)
            for(Meteor: SpawnedMeteors, MeteorProp:=Meteor?, Meteor?.IsValid[]): # Tests iterating through an array that contains missing elements

                    var MeteorPosition:vector3=MeteorProp.GetTransform().Translation

                    set MeteorPosition = AddFloatToVector(vector.Z, MeteorPosition, -MeteorSpeedFloat)
                
                    MeteorProp.MoveTo(MeteorPosition, ZeroRotation, 0.001)

                    if(MeteorPosition.Z < MeteorExplodeZFloat):
                        # Spawn the explosion here
                        ExplosivePosition:= AddFloatToVector(vector.Z, MeteorPosition, -MeteorSpeedFloat)

                        if(ExplosiveDevice:=ExplosiveDevices[ExplosionDeviceIndex],  
                        ExplosiveDevice.TeleportTo[ExplosivePosition, ZeroRotation],
                        Player1:agent=GameManager.Players[0]):

                                Sleep(0.1)
                                ExplosiveDevice.Explode(Player1)
                                set ExplosionDeviceIndex+=1
                        set MeteorPosition = AddFloatToVector(vector.Z, MeteorPosition, -MeteorSpeedFloat)
                        MeteorProp.MoveTo(MeteorPosition, ZeroRotation, 0.001)

                        if(MeteorProp.IsValid[]):

                            Print("NexusError - MeteorProp.IsValid[] returned true when expected false in ActivateMeteorsFalling()")

                            
                        # MeteorProp.Dispose()

    BeginRound<override>()<suspends>:void=
        set IsRoundGoing = true
            BeginRoundNonSync()
            loop:
                Sleep(0.5)
                SpawnMeteorProps()
            ActivateMeteorsFalling()


    BeginRoundNonSync():void=
        spawn{ActivateMeteorsFalling()}

    EndRound<override>()<suspends>:void=
        set IsRoundGoing = false


