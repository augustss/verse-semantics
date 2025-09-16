using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

store_item := class(creative_device):
    @editable
    Granter:item_granter_device = item_granter_device{}

    @editable
    Buttons:[]conditional_button_device = array{}

    OnBegin<override>()<suspends>:void=
        for (Button:Buttons):
            Button.ActivatedEvent.Subscribe(OnTryBuyItem)

    OnTryBuyItem(Agent:agent):void=
        Granter.GrantItem(Agent)
