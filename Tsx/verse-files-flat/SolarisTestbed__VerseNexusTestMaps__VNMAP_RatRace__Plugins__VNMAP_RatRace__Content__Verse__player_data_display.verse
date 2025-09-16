
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
player_data_display := class(creative_device):
    @editable
    var StatBillboard:billboard_device = billboard_device{}

    PlayerDataManager:player_data_manager = player_data_manager{}

    BillboardMessage<localizes>(Stat:int):message = "Your lifetime rat spawns: {Stat}"

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - player_data_display")

    #UpdateBillboard(Agent:agent):void=


    OnEnd<override>():void=
        Print("Nexus OnEnd - player_data_display")