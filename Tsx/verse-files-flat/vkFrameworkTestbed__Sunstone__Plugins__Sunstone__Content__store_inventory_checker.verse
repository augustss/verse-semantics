
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.


store_popup_trigger_container:=class<concrete>:
    @editable
    Trigger : trigger_device = trigger_device{}
    @editable
    IsBack: logic = false
    
store_popup_container:=class<concrete>:
    @editable
    Popup : popup_dialog_device = popup_dialog_device{}

    @editable
    Triggers : []store_popup_trigger_container = array{}

# A Verse-authored creative device that can be placed in a level
store_inventory_checker := class(creative_device):
    @editable
    Message : hud_message_device = hud_message_device{}

    @editable
    PopupData : []store_popup_container = array{}

    @editable
    Sniffer : []conditional_button_device = array{}

    MaxInventory : int = 5


    OnBegin<override>()<suspends>:void=
        for (Popup : PopupData):
            spawn{HandlePopupPoppedUp(Popup)}

    HandlePopupPoppedUp(Popup :store_popup_container)<suspends>:void=
        loop:
            AgentButtonIndex := Popup.Popup.RespondingButtonEvent.Await()
            Print ("Index pressed {AgentButtonIndex(1)}")

            if:
                Popup.Triggers[AgentButtonIndex(1)].IsBack?
                Trigger := Popup.Triggers[AgentButtonIndex(1)].Trigger
            then:
                Trigger.Trigger(AgentButtonIndex(0))
                Print ("Back Pressed")
            else:
                var ItemCount : int = 0

                for (Sniff : Sniffer, Agent := AgentButtonIndex(0), Sniff.HasAllItems[Agent]):
                    set ItemCount += 1

                if (ItemCount >= MaxInventory):
                    Message.Show(AgentButtonIndex(0))
                    Print ("Inventory is full")
                else:
                    if:
                        Trigger := Popup.Triggers[AgentButtonIndex(1)].Trigger
                    then:
                        Trigger.Trigger(AgentButtonIndex(0))

            Sleep(0.0)

