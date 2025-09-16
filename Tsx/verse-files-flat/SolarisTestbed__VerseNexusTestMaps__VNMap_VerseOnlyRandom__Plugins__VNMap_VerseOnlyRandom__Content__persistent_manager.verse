using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

var PlayerInfo : weak_map(player,player_data) = map {}


player_data := class<final><persistable>:
    Version : int = 0
    CurrentMoney : int = 0

player_stat := class<final><persistable>:
    CurrentXP : float = 0.0
    MaxXP : float = 0.0

GetPlayerInfo<constructor>(Src : player_data) <transacts> := player_data:
        CurrentMoney := Src.CurrentMoney

SavePlayerInfo(Src : player_data, NewMoney : int) <transacts> : player_data =
    NewData : player_data = player_data:
        GetPlayerInfo<constructor>(Src)
        CurrentMoney := NewMoney

CheckSaveDataForPlayer (Agent : agent)<decides><transacts>:void=
    if (_Player := player[Agent]):
        if (PlayerInfo[_Player]):
            # Do nothing
        else:
            set PlayerInfo[_Player] = player_data {}

persistent_manager := class(creative_device):

    OnBegin<override>()<suspends>:void =
        Print("Nexus OnBegin - persistent_manager")
        Players := GetPlayspace().GetPlayers()
        for (Player : Players):
            if:
                not PlayerInfo[Player]
                set PlayerInfo[Player] = player_data {}

    OnEnd<override>():void=
        Print("Nexus OnEnd - persistent_manager")