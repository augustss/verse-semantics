using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


data_layer_deactivate_manager := class(creative_device):


    @editable
    DeactivateDLHUDMessageDevice : hud_message_device = hud_message_device {}

    ToMessage<localizes>(Message : string):message= "{Message}"

    OnBegin<override>()<suspends>:void=
        Print ("OnBegin - data_layer_deactivate_manager has began")
        DeactivateMessage()

    DeactivateMessage():void=
        DeactivateDLHUDMessageDevice.Show(ToMessage("OnBegin - Data Layer has been deactivated!"))

    OnEnd<override>():void=
        Print ("OnEnd - data_layer_deactivate_manager has ended")