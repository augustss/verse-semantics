# Copyright Epic Games, Inc. All Rights Reserved.

item_type := enum{
    Item,
    Weapon,
    Helmet,
    Armor,
    Boots
}
item_data := struct:
    Type:item_type = item_type.Item
    Name:string = ""
    RequiredCapacity:int = 0
    
item_data_manager<public> := class:

    GetNextItemOfType(ItemType:item_type, CurrentItem:string)<transacts>:string=
        var ItemFound:logic = false

        for (Key->Value:Items):
            if:
                ItemFound?
                Value.Type = ItemType
            then:
                return Key
            else if:
                Key = CurrentItem
                set ItemFound = true

        CurrentItem

    DefaultItems:[]string = array:
        "Item_Food"
        "Item_Bedroll"
    DefaultWeapon:string = "Weapon_Staff"
    DefaultHelmet:string = "Helmet_ClothHat"
    DefaultArmor:string = "Armor_ClothTunic"
    DefaultBoots:string = "Boots_Leather"

    var Items:[string]item_data = map:
        # Items
        "Item_TreasureMap" => item_data{
            Type:=item_type.Item
            Name:="Treasure Map"
            RequiredCapacity:=1
        }
        "Item_Food" => item_data{
            Type:=item_type.Item
            Name:="Food"
            RequiredCapacity:=2
        }
        "Item_MagicElixir" => item_data{
            Type:=item_type.Item
            Name:="Magic Elixir"
            RequiredCapacity:=2
        }
        "Item_SpecialTotem" => item_data{
            Type:=item_type.Item
            Name:="Special Totem"
            RequiredCapacity:=3
        }
        "Item_Talisman" => item_data{
            Type:=item_type.Item
            Name:="Talisman"
            RequiredCapacity:=2
        }
        "Item_Bedroll" => item_data{
            Type:=item_type.Item
            Name:="Bedroll"
            RequiredCapacity:=5
        }
        "Item_Key" => item_data{
            Type:=item_type.Item
            Name:="Key"
            RequiredCapacity:=1
        }
        "Item_DragonEgg" => item_data{
            Type:=item_type.Item
            Name:="Dragon Egg"
            RequiredCapacity:=5
        }
        "Item_Dragonscales" => item_data{
            Type:=item_type.Item
            Name:="Dragonscales"
            RequiredCapacity:=3
        }

        # Weapons
        "Weapon_Staff" => item_data{
            Type:=item_type.Weapon
            Name:="Staff"
            RequiredCapacity:=1
        }
        "Weapon_Bow" => item_data{
            Type:=item_type.Weapon
            Name:="Bow & Arrow"
            RequiredCapacity:=1
        }
        "Weapon_MetalSword" => item_data{
            Type:=item_type.Weapon
            Name:="Metal Sword"
            RequiredCapacity:=1
        }
        "Weapon_BattleAxe" => item_data{
            Type:=item_type.Weapon
            Name:="Battle Axe"
            RequiredCapacity:=1
        }
        "Weapon_FireSword" => item_data{
            Type:=item_type.Weapon
            Name:="Fire Sword"
            RequiredCapacity:=1
        }

        # Helmets
        "Helmet_ClothHat" => item_data{
            Type:=item_type.Helmet
            Name:="Cloth Hat"
            RequiredCapacity:=0
        }
        "Helmet_MetalHelmet" => item_data{
            Type:=item_type.Helmet
            Name:="Metal Helmet"
            RequiredCapacity:=0
        }
        "Helmet_CrownOfPower" => item_data{
            Type:=item_type.Helmet
            Name:="Crown of Power"
            RequiredCapacity:=0
        }

        # Armors
        "Armor_ClothTunic" => item_data{
            Type:=item_type.Armor
            Name:="Cloth Tunic"
            RequiredCapacity:=0
        }
        "Armor_LeatherJacket" => item_data{
            Type:=item_type.Armor
            Name:="Leather Jacket"
            RequiredCapacity:=0
        }
        "Armor_ChainmailVest" => item_data{
            Type:=item_type.Armor
            Name:="Chainmail Vest"
            RequiredCapacity:=0
        }
        "Armor_PlateArmor" => item_data{
            Type:=item_type.Armor
            Name:="Plate Armor"
            RequiredCapacity:=0
        }
        "Armor_CloakOfInvisibility" => item_data{
            Type:=item_type.Armor
            Name:="Cloak of Invisibility"
            RequiredCapacity:=0
        }
        "Armor_DragonscaleArmor" => item_data{
            Type:=item_type.Armor
            Name:="Dragonscale Armor"
            RequiredCapacity:=0
        }

        # Boots
        "Boots_Leather" => item_data{
            Type:=item_type.Boots
            Name:="Leather"
            RequiredCapacity:=0
        }
        "Boots_Chainmail" => item_data{
            Type:=item_type.Boots
            Name:="Chainmail"
            RequiredCapacity:=0
        }
        "Boots_PlateSteel" => item_data{
            Type:=item_type.Boots
            Name:="Plate Steel"
            RequiredCapacity:=0
        }
        "Boots_Diamond" => item_data{
            Type:=item_type.Boots
            Name:="Diamond"
            RequiredCapacity:=0
        }