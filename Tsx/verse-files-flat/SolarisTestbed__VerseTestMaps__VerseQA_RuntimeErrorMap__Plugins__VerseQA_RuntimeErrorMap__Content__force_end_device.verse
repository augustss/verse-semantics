
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

force_end_device := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    @editable
    ForceEndGameTime:float = 60.0

    OnBegin<override>()<suspends>:void=
        Sleep(ForceEndGameTime)
        if(_Player:=Self.GetPlayspace().GetPlayers()[0]){EndGameDevice.Activate(_Player)}