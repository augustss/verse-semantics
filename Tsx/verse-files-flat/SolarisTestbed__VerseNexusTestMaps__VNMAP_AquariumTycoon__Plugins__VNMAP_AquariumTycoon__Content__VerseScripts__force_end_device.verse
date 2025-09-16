
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

force_end_device := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    OnBegin<override>()<suspends>:void=
        Sleep(180.0)
        if(_Player:=Self.GetPlayspace().GetPlayers()[0]){EndGameDevice.Activate(_Player)}