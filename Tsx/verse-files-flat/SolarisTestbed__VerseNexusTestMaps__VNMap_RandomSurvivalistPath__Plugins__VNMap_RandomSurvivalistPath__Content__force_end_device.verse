
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

force_end_device := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    ActivateEndGame():void=
        if(_Player:=Self.GetPlayspace().GetPlayers()[0]):
            EndGameDevice.Activate(_Player)

    OnBegin<override>()<suspends>:void=
        Sleep(60.0)
        Print("NexusError - ending game due to timeout by force_end_device")
        ActivateEndGame()