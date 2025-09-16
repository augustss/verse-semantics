
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# A Verse-authored creative device that can be placed in a level
shop_class<public> := class<concrete>:

    ItemCostMessage<localizes>(ItemName:string,Gold:int) : message = "{ItemName}\n{Gold} Gold"

    var LocalPlayerManager:player_manager = player_manager{}

    @editable
    ShopItem:item_granter_device=item_granter_device{}

    @editable
    ItemCost:int = 50

    @editable
    WeaponName:string = "Weapon"

    @editable
    ShopButton:button_device = button_device{}
    
    OnBuyButtonPress(Agent:agent):void=

        CanPlayerAfford:logic = LocalPlayerManager.CanPlayerAffordThis(Agent, ItemCost)

        if(CanPlayerAfford = false){return} # If player can't afford, return

        LocalPlayerManager.GainGold(Agent, -ItemCost)
        ShopItem.GrantItem(Agent)

    InitializeShopClass():void=
        ShopButton.InteractedWithEvent.Subscribe(OnBuyButtonPress)
        ShopButton.SetInteractionText(ItemCostMessage(WeaponName,ItemCost))



shop_manager := class(creative_device):
    @editable
    GlobalPlayerManager:player_manager = player_manager{}

    @editable
    ShopItems:[]shop_class=array{}

    @editable
    HealerButton:button_device = button_device{}

    @editable
    HealingCost:int = 50

    OnBegin<override>()<suspends>:void=
        # Initialize shop items
        Print("Nexus OnBegin - shop_manager")

        Sleep(0.1)
        #Task1.Await()
        HealerButton.InteractedWithEvent.Subscribe(OnHealPlayer) 

        Task1:= spawn{InitializeShopItems()}
        # Task1.Await()
        
    OnEnd<override>():void=
        Print("Nexus OnEnd - shop_manager")
        
    OnHealPlayer(Agent:agent):void=
        var IsHealthFull : logic = GlobalPlayerManager.IsPlayerAtFullHealth(Agent)
        
        if(IsHealthFull=true): 
            # Failed 
            GlobalPlayerManager.FailAudioPlayer.Play(Agent) 
            return

        var CanAfford : logic = GlobalPlayerManager.CanPlayerAffordThis(Agent,HealingCost)

        CanHeal:= logic{IsHealthFull? or CanAfford?}

        if(CanHeal=true):
            GlobalPlayerManager.HealPlayer(Agent)
            GlobalPlayerManager.GainGold(Agent, -HealingCost)
        else:
             # Failed 
             GlobalPlayerManager.FailAudioPlayer.Play(Agent) 

    InitializeShopItems()<suspends>:void=
        for(ShopItem:ShopItems):
            set ShopItem.LocalPlayerManager = GlobalPlayerManager
            ShopItem.InitializeShopClass()

