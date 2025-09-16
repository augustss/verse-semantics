
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


data_layer_activate_manager := class(creative_device):
    @editable
    ActivateDLHUDMessageDevice : hud_message_device = hud_message_device {}

    ToMessage<localizes>(Message : string):message= "{Message}"

    OnBegin<override>()<suspends>:void=
        Print ("OnBegin - data_layer_activate_manager has began")
        ActivateMessage()



    ActivateMessage():void=
        ActivateDLHUDMessageDevice.Show(ToMessage("OnBegin - Data Layer has been activated!"))



    OnEnd<override>():void=
        Print ("OnEnd - data_layer_activate_manager has ended")