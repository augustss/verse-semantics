
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { GameEvents }
using { Utils }


VFXsController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    StartedFireVFXs:[]vfx_creator_device = array{}

    @editable
    LocalRainsVFXs:[]vfx_spawner_device = array{}

    @editable
    LightingsVFXs:[]vfx_spawner_device = array{}

    @editable
    LeavesVFXs:[]vfx_spawner_device = array{}

    @editable
    LightingTimeot:float = 30.0

    @editable
    LightingDeltaShift:float = 1000.0

    @editable
    LeavesDeltaShift:float = 500.0


    var LightingConfigs:[]LightingConfig = array{}
    var LeavesConfigs:[]LeavesConfig = array{}



    OnBegin<override>()<suspends>:void=
        Print("VFXs Controller Starts")
        MakeSubscriptions()
        MakeArraysOfConfigs()
        Print("Signal RainsVFXs")
        GameplayEvents.LocalRainsInitEvent.Signal(LocalRainsVFXs)
        GameplayEvents.LightingsInitEvent.Signal(LightingConfigs)
        GameplayEvents.LeavesInitEvent.Signal(LeavesConfigs)


    MakeSubscriptions():void=
        Print("Make VFX subscriptions")
        spawn:
            EnableStartedFireVfxsOnRaceStart()

    
    EnableStartedFireVfxsOnRaceStart()<suspends>:void=
        Print("Await starting VFXs on race start")
        GameplayEvents.RaceStartEvent.Await()
        for(Vfx : StartedFireVFXs):
            Vfx.Enable()
            Vfx.BeginForAll()

    
    MakeArraysOfConfigs():void=
        Print("Make array of lightings")
        for(Lighting : LightingsVFXs):
            Lighting.Disable()
            set LightingConfigs += array{
                                        LightingConfig{
                                            LightingDevice:=Lighting, 
                                            TimeOut:=LightingTimeot, 
                                            DeltaShift:=LightingDeltaShift}
                                        }
        Print("Make array of Leaves")
        for(Leaves : LeavesVFXs):
            Leaves.Disable()
            set LeavesConfigs += array{
                                        LeavesConfig{
                                            LeavesDevice:=Leaves, 
                                            DeltaShift:=LeavesDeltaShift}
                                        }




LightingConfig<public> := struct:
    LightingDevice<public>:vfx_spawner_device
    TimeOut<public>:float
    DeltaShift<public>:float



LeavesConfig<public> := struct:
    LeavesDevice<public>:vfx_spawner_device
    DeltaShift<public>:float