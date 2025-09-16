
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

force_end_device := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    @editable
    ForceEndTime:float = 55.0
    OnBegin<override>()<suspends>:void=
        Sleep(ForceEndTime)
        Print("NexusError - ending game due to timeout by force_end_device")
        if(_Player:=Self.GetPlayspace().GetPlayers()[0]){EndGameDevice.Activate(_Player)}