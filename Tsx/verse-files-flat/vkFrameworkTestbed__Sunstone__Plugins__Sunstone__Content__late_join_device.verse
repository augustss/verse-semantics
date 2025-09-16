
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

late_join_device := class(creative_device):

    @editable
    ItemGranter:item_granter_device = item_granter_device{}

    @editable
    GoldGrantPeriod:float = 0.0

    @editable
    LateJoinBufferTime:float = 0.0

    var StartTime:float = 0.0

    @editable
    GoldGranter:item_granter_device = item_granter_device{}

    OnBegin<override>()<suspends>:void=
        Playspace := GetPlayspace()
        Playspace.PlayerAddedEvent().Subscribe(OnPlayerAdded)
        set StartTime = GetSimulationElapsedTime()

    OnPlayerAdded(Player:player):void=
        ItemGranter.GrantItem(Player)

        ElapsedTime := GetSimulationElapsedTime()
        TotalTime := ElapsedTime - StartTime
        if:
            GoldGrantCount := Round[(TotalTime - LateJoinBufferTime) / GoldGrantPeriod] 
            GoldGrantCount > 0
        then:
            for (i:=1..GoldGrantCount):
                GoldGranter.GrantItem(Player)