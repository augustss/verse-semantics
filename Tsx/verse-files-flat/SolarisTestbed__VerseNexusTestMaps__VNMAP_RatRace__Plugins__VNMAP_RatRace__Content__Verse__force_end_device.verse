
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
force_end_device := class(creative_device):

    @editable
    EndGameDevice : end_game_device = end_game_device{}

    OnBegin<override>()<suspends>:void=
        Sleep(180.0)
        if(_Player:=Self.GetPlayspace().GetPlayers()[0]){EndGameDevice.Activate(_Player)}