
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


boulder_trap_device := class(obstacle_device):

    RoundName<override> : string= "Boulder Falling"
    RoundXP<override>:int = 25

    # var BoulderDevices : []physics_boulder_device = array{}
    @editable
    BoulderDestroyTime : float = 10.0

    # @editable
    var BoulderDevices : []physics_boulder_device = array{}

    OnBegin<override>()<suspends>:void=
        Sleep(0.5)
        InitializeBoulderDevices()


    InitializeBoulderDevices():void=
        TaggedBoulders := GetCreativeObjectsWithTag(boulder_tag{})
        for(Boulder : TaggedBoulders, NewBoulder : physics_boulder_device = physics_boulder_device[Boulder]):
            NewBoulderDevices:= set BoulderDevices += NewBoulderDevice: []physics_boulder_device = array{physics_boulder_device(NewBoulder)}
        Print("Boulders found = {BoulderDevices.Length}")

    ThrowBoulders()<suspends>:void=
        loop:
            Sleep(2.0)
            for(Boulder:BoulderDevices):
                Boulder.ReleaseRollingBoulder()
                Print("Pretend boulder is rolling")

            Sleep(BoulderDestroyTime)
            for(Boulder:BoulderDevices):
                Boulder.DestroyRollingBoulder()
                Print("Pretend boulder is rolling")

                # Sleep(BoulderDestroyTime)
                # Boulder.DestroyRollingBoulder()
            # sync:
            #     Sleep(0.1)
            #     for(Index:=0..BoulderDevices.Length, BoulderDevice:physics_boulder_device= BoulderDevices[Index]):
            #         BoulderDevice.ReleaseRollingBoulder()
            #         Sleep(BoulderDestroyTime)
            #         BoulderDevice.DestroyRollingBoulder()

            #         Sleep(0.1)
            # Sleep(1.0)
    EndRound<override>()<suspends>:void=
        Print("Round End")

    BeginRound<override>()<suspends>:void=
        Print("Beginning to throw the boulders")
        Sleep(1.0)
        ThrowBoulders()