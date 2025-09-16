##################################################
# round_settings_manager.verse
#
# Enables and disables matchmaking to check for regressions in the related functions.
# See: SOL-5783, an SH bug where DisableMatchmaking caused a Verse crash.
#
# Author: Jade Hymer
##################################################
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

round_settings_manager := class(creative_device):
    @editable
    RoundSettingsDevice:round_settings_device = round_settings_device{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - round_settings_manager")
        # Sleep a bit between each function call to make it easier to see where something fails
        RoundSettingsDevice.EnableMatchmaking()
        Print("ROUND SETTINGS MANAGER: Matchmaking enabled")
        Sleep(1.0)
        RoundSettingsDevice.DisableMatchmaking()
        Print("ROUND SETTINGS MANAGER: Matchmaking disabled")
        Sleep(1.0)
        RoundSettingsDevice.ToggleMatchmaking() # should enable matchmaking
        Print("ROUND SETTINGS MANAGER: Matchmaking toggled, should be on")
        Sleep(1.0)
        RoundSettingsDevice.ToggleMatchmaking() # should disable matchmaking
        Print("ROUND SETTINGS MANAGER: Matchmaking toggled, should be off")
        Sleep(1.0)
        RoundSettingsDevice.ToggleMatchmaking() # should enable matchmaking
        Print("ROUND SETTINGS MANAGER: Matchmaking toggled, should be on")
    
    OnEnd<override>():void=
        Print("Nexus OnEnd - round_settings_manager")