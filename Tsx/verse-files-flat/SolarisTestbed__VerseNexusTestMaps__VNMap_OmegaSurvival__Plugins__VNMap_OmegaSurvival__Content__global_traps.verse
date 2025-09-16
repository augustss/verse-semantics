# Purpose of this is for traps that don't require complex class
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Simulation/Tags }




Prop_Hider := class <concrete> {
    @editable Button : button_device = button_device{}
    @editable Prop : creative_prop = creative_prop{}
    @editable var Shown : logic = true

    Setup () : void = {
        Button.InteractedWithEvent.Subscribe(OnButtonInteractedWith)
        if (Shown = true) {
            Prop.Show()
        } else {
            Prop.Hide()
        }
    }

    OnButtonInteractedWith (Agent : agent) : void = {
        if (Shown = true) {
            Prop.Hide()
            set Shown = false
        } else {
            Prop.Show()
            set Shown = true
        }
    }

}