
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
game_manager := class(creative_device):
    @editable
    EndGameDevice:end_game_device = end_game_device{}
    @editable
    var GameTimer:float = 60.0

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - game_manager")
        Players := GetPlayspace().GetPlayers()

        Sleep(GameTimer)

        set GameTimer -= GameTimer

        Sleep(GameTimer)

        for(Player:Players):
            EndGame(Player)
    
    EndGame(Agent:agent):void=
        EndGameDevice.Activate(Agent)

    OnEnd<override>():void=
        Print("Nexus OnEnd - game_manager")