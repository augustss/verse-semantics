using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Game }
using { /Fortnite.com/Animation/PlayAnimation }

guard_volume_tag := class(tag):

guard_manager_device := class(creative_device):
    @editable
    GuardSpawners : []guard_spawner_device = array {}

    @editable
    Zones : []volume_device = array {}

    var CustomGuardMap : [agent]custom_guard = map {}

    OnBegin<override>()<suspends>:void=
        {
        Print("Nexus OnBegin - guard_manager_device")

        for (GuardSpawner : GuardSpawners):
            GuardSpawner.SpawnedEvent.Subscribe(HandleGuardSpawn)

        for (Zone : Zones):
            Zone.AgentEntersEvent.Subscribe(HandleEntered)

        TaggedVolumes := GetCreativeObjectsWithTag (guard_volume_tag{})
        
        for (TaggedVolume : TaggedVolumes):
            if (VolumeDevice := volume_device[TaggedVolume]):
                VolumeDevice.AgentEntersEvent.Subscribe(HandleEntered)

        }
    OnEnd<override>():void=
        Print("Nexus OnEnd - guard_manager_device")

    HandleGuardSpawn(Agent : agent):void=
        if (GuardFortChar := Agent.GetFortCharacter[]):
            AGuard := custom_guard{GuardFortChar := GuardFortChar}

            if(set CustomGuardMap[Agent] = AGuard):

    HandleEntered(Agent : agent):void=
        for (CustomGuard : CustomGuardMap):
            spawn:
                CustomGuard.PlayAnim()

custom_guard := class<unique>():

    GuardFortChar : fort_character
    var IsPlaying : logic = false


    PlayAnim()<suspends>:void=
        if (GuardFortChar.IsActive[]):
            if (IsPlaying = false):
                set IsPlaying = true

                if (AnimController := GuardFortChar.GetPlayAnimationController[]):
                    AnimInstance := AnimController.Play(Anims.GuardThing)
                    Print("Playing guard Anim then death | PlayAnim() | guard_manager_device")
                    loop:
                        AnimInstance.Await()
                        GuardFortChar.Damage(100.0)
                        set IsPlaying = false
                        break