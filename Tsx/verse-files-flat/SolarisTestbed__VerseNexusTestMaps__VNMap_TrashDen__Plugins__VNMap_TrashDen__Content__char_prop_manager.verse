using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }  
   
char_prop_manager:= class(creative_device):

    @editable
    TrashChar : creative_prop = creative_prop{}
    @editable
    CharIdleAnim : cinematic_sequence_device = cinematic_sequence_device{}
    @editable
    CharRunAnim : cinematic_sequence_device = cinematic_sequence_device{}
   
    OnMoved(MoveInfo : tuple(fort_character, logic)):void=
        if (Agent := MoveInfo(0).GetAgent[]):
            if (MoveInfo(1)?):
                CharIdleAnim.Stop(Agent)
                CharRunAnim.Play(Agent)
            else:
                CharRunAnim.Stop(Agent)
                CharIdleAnim.Play(Agent)
    UpdateTrashChar(Player : fort_character)<suspends>:void=
        Sleep(1.0)
        loop:
            Sleep(0.0)
            if(TrashChar.TeleportTo[Player.GetTransform().Translation + vector3 {X:= -100.0, Y:= -100.0, Z:= -80.0}, Player.GetTransform().Rotation.ApplyYaw(-1.3)]) {}


    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin char_prop_manager======")
        # Getting the first player and hiding that player and replacing with the new prop
        AllPlayers := GetPlayspace().GetPlayers()
        if (Player := AllPlayers[0], Agent := agent[Player], FirstFortChar := Agent.GetFortCharacter[]):
            # FirstFortChar.Hide()
            CharIdleAnim.Play(Agent)
            FirstFortChar.SprintedEvent().Subscribe(OnMoved)
            CharRunAnim.SetPlayRate(0.2)
            spawn:
                UpdateTrashChar(FirstFortChar)
    OnEnd<override>():void=
        Print ("======Nexus OnEnd char_prop_manager======")