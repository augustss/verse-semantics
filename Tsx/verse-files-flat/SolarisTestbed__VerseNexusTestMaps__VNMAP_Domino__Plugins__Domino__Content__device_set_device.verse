using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

domino_log<internal> := class(log_channel){}

device_set_device := class(creative_device):

    @editable
    Campfire : creative_device = creative_device{}
  
    @editable
    Speedometer : creative_device = creative_device{}
    
    @editable
    FireTrapCaptureZone : creative_device = creative_device{}
    
    @editable
    DominoMain : creative_device = creative_device{}
    
    @editable
    PinballCaptureZone : creative_device = creative_device{}
    
    @editable
    Device6 : creative_device = creative_device{}

    @editable
    Device7 : creative_device = creative_device{}
    
    @editable
    Device8 : creative_device = creative_device{}
    
    @editable
    Device9 : creative_device = creative_device{}
    
    @editable
    Device10 : creative_device = creative_device{}
    
    @editable
    Device11 : creative_device = creative_device{}
    
    @editable
    Device12 : creative_device = creative_device{}
    
    @editable
    Device13 : creative_device = creative_device{}

    @editable
    Device14 : creative_device = creative_device{}

    @editable
    Device15 : creative_device = creative_device{}

    @editable
    Device16 : creative_device = creative_device{}
    
    @editable
    Device17 : creative_device = creative_device{}

    @editable
    Device18 : creative_device = creative_device{}

    @editable
    Device19 : creative_device = creative_device{}

    @editable
    Device20 : creative_device = creative_device{}

    @editable
    Device21 : creative_device = creative_device{}
 
    @editable
    Device22 : creative_device = creative_device{}

    Logger<private> : log = log{Channel := domino_log}

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - device_set_device")  

        WinnerResultRace := race:
            block:
                Campfire.OnBegin()
                "campfire_fire_device"   
            block:
                Speedometer.OnBegin()
                "speedometer_device"
            block:
                FireTrapCaptureZone.OnBegin()
                "fire_trap_capture_zone"
            block:
                DominoMain.OnBegin()
                "domino_device"
            block:
                PinballCaptureZone.OnBegin()
                "pinball_capture_zone_device"

        WinnerResultRush := rush:
            block:
                Campfire.OnBegin()
                "campfire_fire_device"   
            block:
                Speedometer.OnBegin()
                "speedometer_device"
            block:
                FireTrapCaptureZone.OnBegin()
                "fire_trap_capture_zone"
            block:
                DominoMain.OnBegin()
                "domino_device"
            block:
                PinballCaptureZone.OnBegin()
                "pinball_capture_zone_device"

        Logger.Print("Speedometer should win")
        Logger.Print("The winning expression for race was: {WinnerResultRace}")
        Logger.Print("The winning expression for rush was: {WinnerResultRush}")
        if (WinnerResultRace <> "speedometer_device") then Print("NexusError - the winner in race should be speedometer_device in OnBegin() of device_set_device")
        if (WinnerResultRush <> "speedometer_device") then Print("NexusError - the winner in rush should be speedometer_device in OnBegin() of device_set_device")

    OnEnd<override>():void=
        Print("Nexus OnEnd - device_set_device")        