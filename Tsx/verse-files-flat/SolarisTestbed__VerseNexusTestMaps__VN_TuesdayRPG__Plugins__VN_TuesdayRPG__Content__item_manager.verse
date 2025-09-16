using { /Fortnite.com/Devices }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org }
using { /Verse.org/Colors }
using { /Verse.org/Assets}
using { /Verse.org }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/UI }
using {/Verse.org/Assets}
using { /Verse.org/Random }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }

item_manager_tag:=class(tag){}

Textures<public> := module { Items<public> := module{}}


#################################
#          Item Classes         #
#################################
item_definition := class<concrete>:

    @editable
    ItemName<public>:string = "", ItemTexture<public>:texture = Textures.Items.default

item_hub:=class(creative_device):

    @editable
    Items:[]item=array{}

    GetItemIndex(ItemToIndex:item):int=

        var ItemIndex:int = 0

                for(IndexedItem:Items):

                    if(IndexedItem = ItemToIndex):

                        return ItemIndex

                    set ItemIndex +=1
                Print("NexusError - User Error, item does not exist in item hub")
                -1 # Invalid index
        
item := class<concrete><unique>:

    @editable
    ItemDefinition<public>:item_definition = item_definition{ItemTexture:=Textures.LevelUpTexture}


    Use<public>():void=

        Print("Used item")

equipment := class(item):

    @editable 
    EquipmentType<public>:equipment_type = equipment_type.Helmet
    
    # ItemDefinition<override>:item_definition = item_definition{ItemTexture:=Textures.Items.default, ItemName := "None"}


consumable := class(item):


inventory := class:

    # MaxCapacityInt : int = 11

    var Items :[]item = array{}

    AddItem(ItemToAdd:item):void=

        set Items += array{ItemToAdd}

    RemoveItem(ItemToRemove:item):void=

        var NewItemArray : []item = array{}

        var ItemHasBeenRemoved:logic = false

        for(Item:Items):
            
            if(ItemHasBeenRemoved? or not Item=ItemToRemove):

                set NewItemArray += array{Item}

            if(not ItemHasBeenRemoved? and Item=ItemToRemove):

                    set ItemHasBeenRemoved = true
                    Print("Item should be removed from inventory")

        set Items = NewItemArray

(Inventory:inventory).GetItemsLength():int=

    return Inventory.Items.Length

(Inventory:inventory).GetSaveStrings()<transacts>:[]string=

    var SaveStrings:[]string =array{}

    for(Item:Inventory.Items):

        set SaveStrings += array{(Item.ItemDefinition.ItemName)}

    return SaveStrings

#############################
#     Equipment Classes         #
#################################

equipment_inventory := class:

    var Weapon:equipment = empty_weapon{}
    var Helmet:equipment  = empty_helmet{}
    var Armor:equipment  = empty_armor{}
    var Platelegs:equipment  = empty_platelegs{}
    var Ring:equipment  = empty_ring{}
    var Amulet:equipment = empty_amulet{}

    GetEquipmentAsArray()<transacts>:[]equipment= array{Weapon,Helmet,Armor,Platelegs,Ring,Amulet}
    GetEquipmentTypeAsArray()<transacts>:[]string = array{"Weapon", "Helmet", "Armor", "Platelegs", "Ring", "Amulet"}
    EmptyEquipmentArray : []string = array{"No Weapon", "No Helmet", "No Armor", "No Platelegs", "No Ring", "No Amulet"}

    GetEquipmentFromType(EquipmentType:equipment_type)<transacts>:equipment=

        case(EquipmentType):
            equipment_type.Weapon=> return Weapon
            equipment_type.Helmet=> return Helmet
            equipment_type.Armor=> return Armor
            equipment_type.Platelegs=> return Platelegs
            equipment_type.Ring=> return Ring
            equipment_type.Amulet=> return Amulet

    EquipItem(Equipment:equipment, Inventory:inventory):void=

        Print("Equipping {Equipment.GetEquipmentName()}, Player now has {Inventory.GetItemsLength()} items")

        case(Equipment.EquipmentType):
            equipment_type.Weapon=> if(Weapon = empty_weapon{}){set Weapon = Equipment}else{UnequipItem(Weapon.EquipmentType, Inventory), set Weapon = Equipment}
            equipment_type.Helmet=> if(Helmet = empty_helmet{}){set Helmet = Equipment}else{UnequipItem(Helmet.EquipmentType, Inventory), set Helmet = Equipment}
            equipment_type.Armor=> if(Armor = empty_armor{}){set Armor = Equipment}else{UnequipItem(Armor.EquipmentType, Inventory), set Armor = Equipment}
            equipment_type.Platelegs=> if(Platelegs = empty_platelegs{}){set Platelegs = Equipment}else{UnequipItem(Ring.EquipmentType, Inventory), set Platelegs = Equipment} 
            equipment_type.Ring=> if(Ring = empty_ring{}){set Ring = Equipment}else{UnequipItem(Ring.EquipmentType, Inventory),set Ring = Equipment}
            equipment_type.Amulet=> if(Amulet = empty_amulet{}){set Amulet = Equipment}else{UnequipItem(Amulet.EquipmentType, Inventory), set Amulet = Equipment}

        Print("Equipped {Equipment.GetEquipmentName()} in the {Equipment.EquipmentType.ToString()} slot")
            
    EquipItem(Equipment:equipment):void=

        Print("Equipping {Equipment.GetEquipmentName()}")

        case(Equipment.EquipmentType):
            equipment_type.Weapon=> set Weapon = Equipment
            equipment_type.Helmet=> set Helmet = Equipment
            equipment_type.Armor=> set Armor = Equipment
            equipment_type.Platelegs=> set Platelegs = Equipment
            equipment_type.Ring=> set Ring = Equipment
            equipment_type.Amulet=> set Amulet = Equipment

    UnequipItem(Equipment:equipment_type, Inventory:inventory):void=
        
        # if(not EmptyWeaponMap[Equipment] = false){return}
        if(CheckIfIsEmpty(Equipment)?){return}
        
        Inventory.AddItem(GetEquipmentFromType(Equipment))

        case(Equipment):
            equipment_type.Weapon=> set Weapon = GetEmptyEquipmentFromType(equipment_type.Weapon)
            equipment_type.Helmet=> set Helmet = GetEmptyEquipmentFromType(equipment_type.Helmet)
            equipment_type.Armor=> set Armor = GetEmptyEquipmentFromType(equipment_type.Armor)
            equipment_type.Platelegs=> set Platelegs = GetEmptyEquipmentFromType(equipment_type.Platelegs)
            equipment_type.Ring=> set Ring = GetEmptyEquipmentFromType(equipment_type.Ring)
            equipment_type.Amulet=> set Amulet = GetEmptyEquipmentFromType(equipment_type.Amulet)

    GetEmptyEquipmentFromType(EquipmentType:equipment_type)<transacts>:equipment=

        case(EquipmentType):
            equipment_type.Weapon=> return empty_weapon{}
            equipment_type.Helmet=> return empty_helmet{}
            equipment_type.Armor=> return empty_armor{}
            equipment_type.Platelegs=> return empty_platelegs{}
            equipment_type.Ring=> return empty_ring{}
            equipment_type.Amulet=> return empty_amulet{}

    CheckIfIsEmpty(EquipmentType:equipment_type)<transacts>:logic=

        # for(Key -> Value : EmptyWeaponMap):
            
        #     if(EquipmentType.GetEquipmentName() = EmptyWeaponMap[Key].GetEquipmentName()){return true}

        for(EquipmentName:EmptyEquipmentArray):

            if(GetEquipmentFromType(EquipmentType).GetEquipmentName() = EquipmentName){return true}

        return false

    # Custom API
(EquipmentType:equipment_type).GetEquipment()<transacts>:equipment=

    case(EquipmentType):
        equipment_type.Weapon=> return empty_weapon{}
        equipment_type.Helmet=> return empty_helmet{}
        equipment_type.Armor=> return empty_armor{}
        equipment_type.Platelegs=> return empty_platelegs{}
        equipment_type.Ring=> return empty_ring{}
        equipment_type.Amulet=> return empty_amulet{}

(EquipmentType:equipment_type).GetEmptyEquipmentName()<transacts>:string=

    case(EquipmentType):
        equipment_type.Weapon=> return empty_weapon{}.ItemDefinition.ItemName
        equipment_type.Helmet=> return empty_helmet{}.ItemDefinition.ItemName
        equipment_type.Armor=> return empty_armor{}.ItemDefinition.ItemName
        equipment_type.Platelegs=> return empty_platelegs{}.ItemDefinition.ItemName
        equipment_type.Ring=> return empty_ring{}.ItemDefinition.ItemName
        equipment_type.Amulet=> return empty_amulet{}.ItemDefinition.ItemName

(EquipmentType:equipment_type).ToString()<transacts>:string=

    case(EquipmentType):
        equipment_type.Weapon=> return "Weapon"
        equipment_type.Helmet=> return "Helmet"
        equipment_type.Armor=> return "Armor"
        equipment_type.Platelegs=> return "Platelegs"
        equipment_type.Ring=> return "Ring"
        equipment_type.Amulet=> return "Amulet"

(Equipment:equipment).GetEquipmentName()<transacts>:string=

    return Equipment.ItemDefinition.ItemName

(EquipmentInventory:equipment_inventory).GetSaveStrings()<transacts>:[]string=

    var SaveStrings:[]string =array{}

    set SaveStrings += array{(EquipmentInventory.Weapon.ItemDefinition.ItemName)}
    set SaveStrings += array{(EquipmentInventory.Helmet.ItemDefinition.ItemName)}
    set SaveStrings += array{(EquipmentInventory.Armor.ItemDefinition.ItemName)}
    set SaveStrings += array{(EquipmentInventory.Platelegs.ItemDefinition.ItemName)}
    set SaveStrings += array{(EquipmentInventory.Ring.ItemDefinition.ItemName)}
    set SaveStrings += array{(EquipmentInventory.Amulet.ItemDefinition.ItemName)}

    return SaveStrings

equipment_type := enum:

    Weapon, Helmet, Armor, Platelegs, Ring, Amulet

#################################
#          Item Classes         #
################################# 

### Consumables ###

health_consumable := class(consumable):

    HealthToHealInt<public>:int = 5;

    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.LevelUpTexture}

    Use<override>():void=

        Print("Health potion used and has {HealthToHealInt} healing power")

small_health_potion := class(health_consumable):

    HealthToHealInt<override>:int = 20;

    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.smallpotion, ItemName:= "Small Health Potion"}

medium_health_potion := class(health_consumable):

    HealthToHealInt<override>:int = 50;

    ItemTexture<public>:texture = Textures.Items.mediumpotion

    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.mediumpotion, ItemName:= "Medium Health Potion"}

large_health_potion := class(health_consumable):

    HealthToHealInt<override>:int = 50;

    ItemTexture<public>:texture = Textures.Items.largepotion

    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.largepotion, ItemName:= "Large Health Potion"}

    ### Equipment ###

# Empty
empty_weapon := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyweapon, ItemName:="No Weapon"}, EquipmentType<override>:equipment_type=equipment_type.Weapon
empty_helmet := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyhelmet, ItemName:="No Helmet"}, EquipmentType<override>:equipment_type=equipment_type.Helmet
empty_armor := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyarmor, ItemName:="No Armor"}, EquipmentType<override>:equipment_type=equipment_type.Armor
empty_platelegs := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyplatelegs, ItemName:="No Platelegs"}, EquipmentType<override>:equipment_type=equipment_type.Platelegs
empty_ring := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyring, ItemName:="No Ring"}, EquipmentType<override>:equipment_type=equipment_type.Ring
empty_amulet := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.emptyamulet, ItemName:="No Amulet"}, EquipmentType<override>:equipment_type=equipment_type.Amulet    

# Tier 1
weapon_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.weapon0, ItemName:="Noob Sword (T1)"}, EquipmentType<override>:equipment_type=equipment_type.Weapon
helmet_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.helmet0, ItemName:="Leather Helmet (T1)"}, EquipmentType<override>:equipment_type=equipment_type.Helmet
armor_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.armor0, ItemName:="Leather Armor (T1)"}
    EquipmentType<override>:equipment_type=equipment_type.Armor
platelegs_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.platelegs0, ItemName:="Leather Platelegs (T1)"}, EquipmentType<override>:equipment_type=equipment_type.Platelegs
ring_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.ring0, ItemName:="Ruby Ring (T1)"}, EquipmentType<override>:equipment_type=equipment_type.Ring
amulet_t1 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.amulet0, ItemName:="Ruby Amulet (T1)"}, EquipmentType<override>:equipment_type=equipment_type.Amulet  

# Tier 2
weapon_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.weapon1, ItemName:="Super Sword (T2)"}
    EquipmentType<override>:equipment_type=equipment_type.Weapon
helmet_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.helmet1, ItemName:="Steel Helmet (T2)"}, EquipmentType<override>:equipment_type=equipment_type.Helmet
armor_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.armor1, ItemName:="Steel Armor (T2)"}, EquipmentType<override>:equipment_type=equipment_type.Armor
platelegs_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.platelegs1, ItemName:="Steel Platelegs (T2)"}, EquipmentType<override>:equipment_type=equipment_type.Platelegs
ring_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.ring1, ItemName:="Amethyst Ring (T2)"}, EquipmentType<override>:equipment_type=equipment_type.Ring
amulet_t2 := class(equipment):
    ItemDefinition<override>:item_definition=item_definition{ItemTexture:=Textures.Items.amulet1, ItemName:="Amethyst Amulet (T2)"}
    EquipmentType<override>:equipment_type=equipment_type.Amulet  
    
#################################
#            MAIN               #
#################################

    
item_manager := class(creative_device):

    # Leaving these comments here to test grandchild inheritence initialization issues
    # Items : []item = array{small_health_potion{}, medium_health_potion{}, large_health_potion{}, weapon_t1{},helmet_t1{}, armor_t1{}, platelegs_t1{}, ring_t1{}, amulet_t1{},weapon_t2{},helmet_t2{}, armor_t2{}, platelegs_t2{}, ring_t2{}, amulet_t2{}}
    # Items : []item = array{small_health_potion{}, medium_health_potion{}, large_health_potion{}, 
    # equipment{ItemDefinition:=item_definition{ItemTexture:=Textures.Items.weapon1, ItemName:="Super Sword (T2)"}, EquipmentType:=equipment_type.Weapon}}
    Items : []string = array{"small health potion", "medium health potion", "large health potion", "weapon t1", "armor t1","helmet t1", "platelegs t1","ring t1", "amulet t1",
    "weapon t2", "armor t2", "helmet t2", "platelegs t2","ring t2", "amulet t2"}
    
    # Using an item map to edit enemy drops in the editor via the enemy_manager class
    ItemMap : [string]item = map{"small health potion" => small_health_potion{}, "medium health potion" => small_health_potion{}, "large health potion" => small_health_potion{},
    "weapon t1" => weapon_t1{}, "armor t1" => armor_t1{}, "helmet t1" => helmet_t1{}, "platelegs t1" => platelegs_t1{},"ring t1" => ring_t1{}, "amulet t1" => amulet_t1{},
    "weapon t2" => weapon_t2{}, "armor t2" => armor_t2{}, "helmet t2" => helmet_t2{}, "platelegs t2" => platelegs_t2{},"ring t2" => ring_t2{}, "amulet t2" => amulet_t2{}}



    @editable
    MutatorZone:mutator_zone_device=mutator_zone_device{}

    @editable
    DebugForceInventoryButton:button_device=button_device{}

    var Players:[]player=array{}

    var PlayerHudMap:[player]?canvas = map{}
    var PlayerInventoryMap:[player]?inventory = map{}
    var PlayerEquipmentMap:[player]?equipment_inventory = map{}

    @editable
    DebugInventoryButton:button_device=button_device{}

    OnBegin<override>()<suspends>:void=
        
        Print("Nexus OnBegin - item_manager")

        set Players = Self.GetPlayspace().GetPlayers()
        
        InitializePlayerInventoyAndEquipment()

        MutatorZone.AgentBeginsEmotingEvent.Subscribe(PlayerEmotedToOpenInventory)

        DebugForceInventoryButton.InteractedWithEvent.Subscribe(PlayerEmotedToOpenInventory)
        DebugInventoryButton.InteractedWithEvent.Subscribe(OnDebugInventoryButton)
        Print("Items in Items := []item: {Items.Length}")

    OnDebugInventoryButton(Agent:agent):void=
        Print("========== Debugging inventory ============")
        if:
            Player:=player[Agent]

        then:
            _Items:=GetPlayerInventory(Player).Items
            for(Item:_Items):
                Print(Item.ItemDefinition.ItemName)


    PlayerEmotedToOpenInventory(Agent:agent):void=

        TogglePlayerUI(Agent)
        Print("Player Emoted")

    InitializePlayerInventoyAndEquipment():void=

        Print("ITEM Players for inv found: {Players.Length}")

        for(Player:Players, not PlayerInventoryMap[Player], set PlayerInventoryMap[Player] = option{inventory{}}, not PlayerEquipmentMap[Player], set PlayerEquipmentMap[Player] = option{equipment_inventory{}}):
            Print("ITEM Player successfully added")

    InitializeSavedPlayerInventoryAndEquipment(Player:player, InventorySaveStrings:[]string,EquipmentSaveStrings:[]string)<suspends>:void=


        # If there isn't any entries in PlayerInventoryMap, or PlayerEquipmentMap for the player, create an empty one
        if(not PlayerInventoryMap[Player], set PlayerInventoryMap[Player] = option{inventory{}},not PlayerEquipmentMap[Player],set PlayerEquipmentMap[Player] = option{equipment_inventory{}}){}
        Print("LogInv: InitializeSavedPlayerInventoryAndEquipment called")

        # Debug code to check if the strings saved and are being sent to this function
        for(InvString:InventorySaveStrings):
            Print("InvLoad: {InvString}")

        # Sleep(1.0)

        #Create a new inventory
        NewInventory : ?inventory= option{inventory{}}

        # for(InvString:InventorySaveStrings, InvItem:=ItemMap[InvString], RealNewInventory:=NewInventory):
        #     # Add items to that inventory
        #     RealNewInventory?.AddItem(InvItem)
        #     # Print("LogItem: {InvString}")
        #     Print("LogInv0: NewInventory items = {RealNewInventory.Items.Length}")

        # Set the player inventory to the new inventory
            if(set PlayerInventoryMap[Player] = NewInventory):
                Print("InvLoad: Inventory Success")


            else:
                Print("InvLoad: Equipment Fail")

        # @@@@@@@@@ EQUIPMENT @@@@@@@@@@ #
        for(EquipString:EquipmentSaveStrings):
            Print("InvLoad: {EquipString}")

        NewEquipmentInventory:equipment_inventory=equipment_inventory{}
        
        for(EqString:EquipmentSaveStrings, EqItem:=ItemMap[EqString], EquipmentItem:=equipment[EqItem]):
            NewEquipmentInventory.EquipItem(EquipmentItem)

        if(PlayerEquipment:=PlayerEquipmentMap[Player]):
            if(set PlayerEquipmentMap[Player] = option{NewEquipmentInventory}):
                Print("InvLoad: Equipment Success")
            else:
                Print("InvLoad: Equipment Fail")
        else:
            Print("InvLoad: Equipment not found - try delaying InitializeSavedPlayerInventoyAndEquipment")

        # Get the amount of maps
        Print("===== Active Inventory Maps =====")
        for(Key->Value: PlayerInventoryMap):
            Print("There is a map")
        Print("===== Active Inventory Maps =====")
        
    InitializeSavedPlayerInventoryAndEquipment(PlayerID:int, InventorySaveStrings:[]string,EquipmentSaveStrings:[]string)<suspends>:void=

        if(_Player:=Players[PlayerID]):
            InitializeSavedPlayerInventoryAndEquipment(_Player,InventorySaveStrings,EquipmentSaveStrings)
            Print("InvLoad: Initialized using PlayerID")

    (EquipmentSaveString:[]string).GetEquipmentFromSaveStrings()<transacts><decides>:equipment_inventory=

        var LoadedEquipment:equipment_inventory=equipment_inventory{}
    
        if(SaveStringElement:=EquipmentSaveString[0],LoadedItem:=ItemMap[SaveStringElement],LoadedWeapon:=equipment[LoadedItem]):
            set LoadedEquipment.Weapon = LoadedWeapon
    
        if(SaveStringElement:=EquipmentSaveString[1],LoadedItem:=ItemMap[SaveStringElement],LoadedHelmet:=equipment[LoadedItem]):
            set LoadedEquipment.Helmet = LoadedHelmet
    
        if(SaveStringElement:=EquipmentSaveString[2],LoadedItem:=ItemMap[SaveStringElement],LoadedArmor:=equipment[LoadedItem]):
            set LoadedEquipment.Armor = LoadedArmor
    
        if(SaveStringElement:=EquipmentSaveString[3],LoadedItem:=ItemMap[SaveStringElement],LoadedPlatelegs:=equipment[LoadedItem]):
            set LoadedEquipment.Platelegs = LoadedPlatelegs
    
        if(SaveStringElement:=EquipmentSaveString[4],LoadedItem:=ItemMap[SaveStringElement],LoadedRing:=equipment[LoadedItem]):
            set LoadedEquipment.Ring = LoadedRing
    
        if(SaveStringElement:=EquipmentSaveString[5],LoadedItem:=ItemMap[SaveStringElement],LoadedAmulet:=equipment[LoadedItem]):
            set LoadedEquipment.Amulet = LoadedAmulet
    
        return LoadedEquipment

    
    (InventorySaveString:[]string).GetInventoryFromSaveStrings()<transacts><decides>:inventory=

        var LoadedInventory:inventory = inventory{}

        var LoadedItems:[]item = array{}

        for(Item:InventorySaveString, LoadedItem:=ItemMap[Item]):

            set LoadedItems += array{LoadedItem}

        set LoadedInventory.Items = LoadedItems
        return LoadedInventory

    GetPlayerInventorySaveStrings(Player:player):[]string=

        return GetPlayerInventory(Player).GetSaveStrings()

    GetPlayerInventory(Player:player):inventory=
        
        var Inventory :inventory = inventory{}

        if(Inven:=PlayerInventoryMap[Player]?):
            Print("Inventory found")
            set Inventory = Inven
        else:
            Print("Inventory not found")

        return Inventory
    
    GetPlayerEquipmentSaveStrings(Player:player)<transacts>:[]string=

        return GetPlayerEquipment(Player).GetSaveStrings()

    GetPlayerEquipment(Player:player)<transacts>:equipment_inventory=
        
        var EquipmentInventory :equipment_inventory = equipment_inventory{}

        if(EquipmentInven:=PlayerEquipmentMap[Player]?):
            set EquipmentInventory = EquipmentInven

        return EquipmentInventory

    TogglePlayerUI<public>(Agent : agent) : void =

        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := PlayerHudMap[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set PlayerHudMap[InPlayer] = false) {}
            else:
                NewUI := ItemAndEquipmentHUD(Agent)
                PlayerUI.AddWidget(NewUI, player_ui_slot{InputMode:=ui_input_mode.All})
                if (set PlayerHudMap[InPlayer] = option{NewUI}) {}
                
    ##################################
    #              HUD               #
    ##################################

    CloseInventoryTitleMessage<localizes>:message = "Close Inventory"

    InventoryMessage<localizes>:message = "========== Inventory ==========               "
    EquipmentMessage<localizes>:message = "               ========== Equipment =========="

    
    # Debug Message
    AddRandomItemMessage<localizes>:message = "Add random item"

    ItemAndEquipmentHUD(Agent : agent):canvas = 

        InventoryColor:color = NamedColors.Black

        CloseInventoryButton : button_regular = button_regular{DefaultText := CloseInventoryTitleMessage}
        CloseInventoryButton.OnClick().Subscribe(OnCloseInventoryButton)

        #Debug button
        AddRandomItemButton :button_regular = button_regular{DefaultText := AddRandomItemMessage}
        AddRandomItemButton.OnClick().Subscribe(OnAddRandomItemButton)

        var Inventory:inventory = inventory{}
        var EquipmentInventory:equipment_inventory = equipment_inventory{}
        if (Player := player[Agent], set Inventory = PlayerInventoryMap[Player]?, set EquipmentInventory = PlayerEquipmentMap[Player]?):

        NewCanvas: canvas = canvas:
            Slots := array:
                canvas_slot: # Close Menu Button 
                        Widget := CloseInventoryButton
                        Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.0}, Maximum := vector2{X := 0.5, Y := 0.0} }
                        Offsets := margin{Top := 50.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                        Alignment := vector2{X := 0.5, Y := 0.0}
                        SizeToContent := true
                canvas_slot: # Inventory 
                    
                    Widget := overlay:
                        Slots := array:
                            overlay_slot: 
                                Widget := texture_block:
                                    DefaultImage := Textures.inventoryUI
                                    # DefaultOpacity := 0.9
                                    DefaultDesiredSize := vector2{X := 666.0, Y := 1006.66}
                    Anchors := anchors{Minimum := vector2{X := 1.0, Y := 0.5}, Maximum := vector2{X := 1.0, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 1.0, Y := 0.5}
                canvas_slot: #Inventory Items
                    Widget := 
                        InventorySlots(Inventory)
                    Anchors := anchors{Minimum := vector2{X := 1.0, Y := 0.0}, Maximum := vector2{X := 1.0, Y := 0.0}}
                    Offsets := margin{Top := 66.6, Left := 0.0, Right :=0.0, Bottom := 0.0}
                    Alignment := vector2{X := 1.0, Y := 0.0}

                canvas_slot: # Equipment 
                    Widget := overlay:
                        Slots := array:
                            overlay_slot: 
                                Widget := texture_block:
                                    DefaultImage := Textures.inventoryUI

                                    DefaultDesiredSize := vector2{X := 666.0, Y := 1006.66} 
                    Anchors := anchors{ Minimum := vector2{X := 0.0, Y := 0.5}, Maximum := vector2{X := 0.0, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.0, Y := 0.5}
        
                canvas_slot: #Equipment Items
                    Widget := stack_box:
                        Orientation := orientation.Vertical
                        Slots := array:
                            stack_box_slot:
                                Widget := EquipmentSlots(EquipmentInventory)
                        
                    Anchors := anchors{ Minimum := vector2{X := 0.0, Y := 0.0}, Maximum := vector2{X := 0.0, Y := 0.0}}
                    Offsets := margin{Top := 66.6, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.0, Y := 0.0}
    
        
        NewCanvas

    ##################################
    #      UI Button Functions       #
    ##################################

    OnCloseInventoryButton(Message : widget_message):void=
        if:
            PlayerUI := GetPlayerUI[Message.Player] 
            MyUI := PlayerHudMap[Message.Player]?
            SelectedButton := text_button_base[Message.Source]
            Player:player = Message.Player
        then:
            TogglePlayerUI(Player)
            PlayerUI.RemoveWidget(MyUI)

    # OnUseInventoryItemButton(Message : widget_message):void=
    #     if:
    #         PlayerUI := GetPlayerUI[Message.Player] 
    #         MyUI := PlayerHudMap[Message.Player]?
    #         SelectedButton := text_button_base[Message.Source]
    #         Player:player = Message.Player
    #     then:
    #         TogglePlayerUI(Player)
    #         PlayerUI.RemoveWidget(MyUI)
    
    OnUseInventoryItemButton(Message : widget_message, Item:item):void=
        if:
            PlayerUI := GetPlayerUI[Message.Player] 
            MyUI := PlayerHudMap[Message.Player]?
            SelectedButton := text_button_base[Message.Source]
            Player:player = Message.Player
            PlayerInventory:inventory = PlayerInventoryMap[Player]?
            EquipmentInventory:equipment_inventory = PlayerEquipmentMap[Player]?
        then:
            Item.Use()

            # If it's an equipment piece
            if(Equipment:=equipment[Item]):
                EquipmentInventory.EquipItem(Equipment, PlayerInventory)
            PlayerInventory.RemoveItem(Item)

            TogglePlayerUI(Player)
            TogglePlayerUI(Player)

    OnUnequipmentButton(Message : widget_message, Equipment:equipment):void=

        if:
            PlayerUI := GetPlayerUI[Message.Player] 
            MyUI := PlayerHudMap[Message.Player]?
            SelectedButton := text_button_base[Message.Source]
            Player:player = Message.Player
            PlayerInventory:inventory = PlayerInventoryMap[Player]?
            EquipmentInventory:equipment_inventory = PlayerEquipmentMap[Player]?
            not Equipment = EquipmentInventory.GetEmptyEquipmentFromType(Equipment.EquipmentType)
        then:
            # if(PlayerInventory.Items.Length>=PlayerInventory.MaxCapacityInt){return}

            EquipmentInventory.UnequipItem(Equipment.EquipmentType, PlayerInventory)
            # PlayerInventory.AddItem(Equipment)
            TogglePlayerUI(Player)
            TogglePlayerUI(Player)
    
    ##################################
    #         Debug stuff            #
    ##################################

    OnAddRandomItemButton(Message : widget_message):void=
        if:
            PlayerUI := GetPlayerUI[Message.Player] 
            MyUI := PlayerHudMap[Message.Player]?
            SelectedButton := text_button_base[Message.Source]
            Player:player = Message.Player
            PlayerInventory:inventory = PlayerInventoryMap[Player]?
            
            RandomItemString:string = Items[GetRandomInt(0,Items.Length)]
            RandomItem:item = ItemMap[RandomItemString]
            # RandomItem:item = ItemMap[Items[GetRandomInt(0,Items.Length)]]
        then:
            Print("Adding item: {RandomItem.ItemDefinition.ItemName}")
            OldInvLength:= PlayerInventory.Items.Length

            PlayerInventory.AddItem(RandomItem)

            if(ItemAsEquipment:equipment=equipment[RandomItem]):
                PlayerInventory.AddItem(ItemAsEquipment)

            Print("Player had {OldInvLength} items but now has {PlayerInventory.Items.Length} items")
            TogglePlayerUI(Player)
            TogglePlayerUI(Player)
        else:
            Print("We hit the else because of item")

    ##################################
    #    Inventory Slot Definition   #
    ##################################

    ItemSlot(Item:item):widget=

        UseItemButton : button_regular = button_regular{DefaultText := StringToMessage(Item.ItemDefinition.ItemName)}
        UseItemButton.OnClick().SubscribeWidgetMessage(OnUseInventoryItemButton, Item)
        
        Widget := stack_box:
            Orientation := orientation.Horizontal
            Slots := array:
                stack_box_slot: 
                    Widget := 
                        texture_block:
                            DefaultImage := Item.ItemDefinition.ItemTexture
                            DefaultDesiredSize := vector2{X := 64.0, Y := 64.0} 
                stack_box_slot:
                    Widget := UseItemButton

        return Widget

    InventorySlots(Inventory : inventory):stack_box=

        InvPadding:margin = margin{Right:=5.0, Bottom:=5.0}

        var SlotsArray:[]stack_box_slot = array{}

        set SlotsArray += array{stack_box_slot{
                                Widget := text_block{
                                    DefaultText := InventoryMessage,
                                    DefaultTextColor := MakeColorFromSRGB(
                                        Red:float:=0.0, 
                                        Green:float:=0.0, 
                                        Blue:float:=0.0)
                                        },
                                Padding:=InvPadding,
                                HorizontalAlignment:=horizontal_alignment.Left
                                }}

        for(Item:Inventory.Items):

            set SlotsArray += array{stack_box_slot{Widget := ItemSlot(Item), Padding := InvPadding}}

        Widget := stack_box:
            Orientation := orientation.Vertical
            Slots := SlotsArray

    ##################################
    #    Equipment Slot Definition   #
    ##################################

    EquipmentSlots(EquipmentInventory : equipment_inventory):stack_box=

        EquipPadding:margin = margin{Right:=5.0, Bottom:=5.0}

        var SlotsArray:[]stack_box_slot = array{}

        set SlotsArray += array{stack_box_slot{ # Title
                                Widget := text_block{
                                    DefaultText := EquipmentMessage,
                                    DefaultTextColor := MakeColorFromSRGB(
                                        Red:float:=0.0, 
                                        Green:float:=0.0, 
                                        Blue:float:=0.0)
                                        },
                                Padding:= EquipPadding,
                                HorizontalAlignment:=horizontal_alignment.Left
                            
                                }}

        EquipmentArrayLength :=EquipmentInventory.GetEquipmentAsArray().Length

        # for(Index:=0..EquipmentArrayLength, POTENTIALLY BUG TO LOOK INTO
        # IndexedEquipment:=EquipmentInventory.GetEquipmentAsArray()[Index]):
        # for(Equipment:EquipmentInventory.GetEquipmentAsArray()):
        #         # set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(IndexedEquipment), Padding := EquipPadding}}
        #         set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(Equipment), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Weapon), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Helmet), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Armor), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Platelegs), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Ring), Padding := EquipPadding}}
        set SlotsArray += array{stack_box_slot{Widget := EquipmentSlot(EquipmentInventory.Amulet), Padding := EquipPadding}}

        Widget := stack_box:
            Orientation := orientation.Vertical
            Slots := SlotsArray


    EquipmentSlot(Equipment:equipment):widget=

        UnequipButton : button_regular = button_regular{DefaultText := StringToMessage(Equipment.ItemDefinition.ItemName)}
        UnequipButton.OnClick().SubscribeWidgetMessage(OnUnequipmentButton, Equipment)

        Widget := stack_box:
            Orientation := orientation.Horizontal
            Slots := array:
                stack_box_slot:
                    HorizontalAlignment:=horizontal_alignment.Left ,
                    Widget := 
                        texture_block:
                            DefaultImage := Equipment.ItemDefinition.ItemTexture
                            DefaultDesiredSize := vector2{X := 64.0, Y := 64.0} 
                stack_box_slot:
                    HorizontalAlignment:=horizontal_alignment.Left,
                    Widget := UnequipButton

    OnEnd<override>():void=
        Print("Nexus OnEnd - item_manager")

    ############################
    #  Inventory Automation    #
    ############################
            

    # This will support both equipment and inventory automation once inheritance issues are fixed
    AutoInventory()<suspends>:void=

        for(Player:Players):

            TogglePlayerUI(Player)
            
            Sleep(1.0)

            if(PlayerInventory:=PlayerInventoryMap[Player],  RandomItem:=PlayerInventory?.Items[GetRandomInt(0,PlayerInventory?.Items.Length-1)]):
            
                RandomItem.Use()

            else:

                Print("NexusError - AutoInventory issue")

            Sleep(1.0)

            TogglePlayerUI(Player)

    GrantPlayerItem(Item:item, Player:player):void=

        if(PlayerInventory:=PlayerInventoryMap[Player]?):
            
            PlayerInventory.AddItem(Item)

            # If the inventory is open, refresh the inventory UI
            if (not PlayerHudMap[Player] = false) 
            {
                TogglePlayerUI(Player)
                TogglePlayerUI(Player)
            }

    GrantPlayerItem(ItemName:string, Player:player):void=
        Print("    GrantPlayerItem(ItemName:string, Player:player):void=")
        if(PlayerInventory:=PlayerInventoryMap[Player]?, ItemToGrant:=ItemMap[ItemName]):

            PlayerInventory.AddItem(ItemToGrant)

            # If the inventory is open, refresh the inventory UI
            if (not PlayerHudMap[Player] = false) 
            {
                TogglePlayerUI(Player)
                TogglePlayerUI(Player)
            }
        else:
            # This condition is expected when Passive Play eliminates enemies
            Print("GrantPlayerItemElseStatement")
            RandomPlayerInt:= GetRandomInt(0,Players.Length-1)

            # if(RandomPlayer:=Players[RandomPlayerInt]):
            if(RandomPlayer:=Self.GetPlayspace().GetPlayers()[RandomPlayerInt]):

                if(PlayerInventory:=PlayerInventoryMap[RandomPlayer]?):
                    

                    if(ItemToGrant:=ItemMap[ItemName]):

                        PlayerInventory.AddItem(ItemToGrant)
                    
                    else:

                        Err("ABC123 ItemMap Issue")
                else:

                    Err("ABC123 Player Inventory Issue")
                # If the inventory is open, refresh the inventory UI
                if (not PlayerHudMap[RandomPlayer] = false) 
                {
                    TogglePlayerUI(RandomPlayer)
                    TogglePlayerUI(RandomPlayer)
                }
            else:
                Err("ABC123 Random Player not found")
                Print("ERROR: Player not found in PlayerInventoryMap[Players[RandomPlayerInt]]?")

    GrantPlayerItem(ItemName:string):void=


        RandomPlayerInt:= GetRandomInt(0,Players.Length-1)

        if(RandomPlayer:=Players[RandomPlayerInt], PlayerInventory:=PlayerInventoryMap[RandomPlayer]?, ItemToGrant:=ItemMap[ItemName]):

            PlayerInventory.AddItem(ItemToGrant)

            # If the inventory is open, refresh the inventory UI
            if (not PlayerHudMap[RandomPlayer] = false) 
            {
                TogglePlayerUI(RandomPlayer)
                TogglePlayerUI(RandomPlayer)
            }
        else:
            Print("ERROR: Player not found in PlayerInventoryMap[Players[RandomPlayerInt]]?")

#################
#    Wrapper    #
################# Wrapper found: https://dev.epicgames.com/community/snippets/d8k/fortnite-wrapping-subscribe-to-pass-additional-data-to-listeners
(Listenable : listenable(widget_message)).SubscribeWidgetMessage(OutputFunc : tuple(widget_message, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_widget_message(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
    
wrapper_widget_message(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(widget_message, t) -> void
    InputFunc(WidgetMessage : widget_message):void = OutputFunc(WidgetMessage, ExtraData)
    
(Listenable : listenable(tuple())).SubscribeEmpty(OutputFunc : t -> void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_empty(t) {ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
    
wrapper_empty(t : type) := class():
    ExtraData : t;
    OutputFunc : t -> void
    InputFunc():void = OutputFunc(ExtraData)