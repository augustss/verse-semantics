using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }

force_end_manager := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - force_end_manager")

        Sleep(180.0)
        if(_Player:=Self.GetPlayspace().GetPlayers()[0])
        {
            Print("NexusError - Ending game due to timeout by force_end_manager")
            EndGameDevice.Activate(_Player)
        }

    OnEnd<override>():void=
        Print("Nexus OnEnd - force_end_manager")