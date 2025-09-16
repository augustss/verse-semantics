using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Colors }
prop_hider := class <concrete> 
{
    @editable 
    Button : button_device = button_device{}

    @editable 
    Prop : creative_prop = creative_prop{}

    @editable 
    var Shown : logic = true


    Setup () : void = 
        if(Prop.IsValid[]):

            Print("[SUCCESS] Hider Props are prepared", ?Color:=NamedColors.LightGreen)
            Button.InteractedWithEvent.Subscribe(OnButtonInteractedWith)

            if (Shown = true): 
                Prop.Show()
                Print("[SUCCESS] The Prop is Visible by default", ?Color:=NamedColors.LightGreen) 

            else:
                Prop.Hide()
                Print("[SUCCESS] The Prop is Hidden by default", ?Color:=NamedColors.LightGreen)
        else:
            Print("[FAILURE] The Prop is not valid for some reason so we cannot hide or show it", ?Color:=NamedColors.Red)
    

    OnButtonInteractedWith (Agent : agent) : void = 
        if(Prop.IsValid[]):

            if (Shown = true):
                Prop.Hide()
                set Shown = false
                Print("[SUCCESS] The Prop is Hidden by Agent Input", ?Color:=NamedColors.LightGreen)
            else:
                Prop.Show()
                set Shown = true
                Print("[SUCCESS] The Prop is Visible by Agent Input", ?Color:=NamedColors.LightGreen)
        else:
            Print("[FAILURE] The Prop is not valid for some reason so we cannot Hide or Show it", ?Color:=NamedColors.Red)
}
hide_prop_manager_device := class(creative_device) 
{
    @editable 
    Hiders : []prop_hider = array{}

    OnEnd<override>():void=
        Print("OnEnd hide_prop_manager_device")
        
    OnBegin<override>()<suspends>:void=

        {
        Print("OnBegin hide_prop_manager_device")
        for (Hider : Hiders) {
            Hider.Setup()            
        }
    }
}