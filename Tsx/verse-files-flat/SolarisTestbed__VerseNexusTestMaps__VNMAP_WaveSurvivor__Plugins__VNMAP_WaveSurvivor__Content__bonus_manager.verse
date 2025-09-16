
using { /Fortnite.com/Devices }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Vehicles }
using { /Verse.org/Simulation }
using { /Verse.org/Colors }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }

CreateBonus<constructor><public>(ID:int, BonusString:string, BonusColor:color, IsRepeated:logic) := bonus:
    id:= ID
    bonus_string := BonusString
    bonus_color := BonusColor
    is_repeated  := IsRepeated

bonus:=class<unique>(IGiveable):
    id<internal>:int
    bonus_string<internal>:string
    bonus_color<internal>:color
    is_repeated<internal>:logic 

    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Its a parent function!")

#Character bonuses
healt_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.Heal(1000.0)
        
shield_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.SetShield(Player.GetShield() + 1000.0)

max_health_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.SetHealth(Player.GetMaxHealth())

max_shield_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.SetShield(Player.GetMaxShield())

invulnerability_bonus:=class(bonus):
    DoInSec(Player:fort_character)<suspends>:void=
        var i:int =0
        loop:
            Sleep(1.0)
            set i = i + 1
            if(i >= 10):
                Player.SetVulnerability(true)
                break
        

    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.SetVulnerability(false)
        spawn:
            DoInSec(Player)
     

invisibility_bonus:=class(bonus):
    DoInSec(Player:fort_character)<suspends>:void=
        var i:int =0
        loop:
            Sleep(1.0)
            set i = i + 1
            if(i = 10):
                Player.Show()
                break

    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void = 
        Print("Bonus given")
        Player.Hide()
        spawn:
            DoInSec(Player)

#Item bonuses
rocket_launcher_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void= 
        ItemGranter.SetNextItem(1)
        if(Agent := Player.GetAgent[]):
            ItemGranter.GrantItem(Agent)

minigun_bonus:=class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void= 
        ItemGranter.SetNextItem(2)
        if(Agent := Player.GetAgent[]):
            ItemGranter.GrantItem(Agent)

shotgun_bonus:= class(bonus):
    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void=
        ItemGranter.SetNextItem(3)
        if(Agent := Player.GetAgent[]):
            ItemGranter.GrantItem(Agent)

bus_bonus:= class(bonus):
    DoInSec(Bus:vehicle_spawner_armored_battle_bus_device, Agent:agent)<suspends>:void=
        Sleep(0.5)
        Bus.AssignDriver(Agent)
        var x:int := 0
        loop:
            Sleep(1.0)
            set x = x + 1
            if(x = 10):
                Bus.Disable()
                break 

    GiveBonus<override>(Player:fort_character, ItemGranter:item_granter_device, Bus:vehicle_spawner_armored_battle_bus_device):void=
        PlayerTransform := Player.GetTransform()
        if:
            Agent:= Player.GetAgent[]
            Bus.TeleportTo[PlayerTransform]
        then:
            Bus.Enable()
            spawn:
                DoInSec(Bus, Agent)

bonus_manager := class(creative_device):
    var Bonuses:[]bonus := array{}

    HealtBonus:bonus := healt_bonus{
        id := 0
        bonus_string := "Increase your health"
        bonus_color := NamedColors.Green
        is_repeated := true

    }

    ShieldBonus:bonus := shield_bonus{
        id := 1
        bonus_string := "Increase your shield"
        bonus_color := NamedColors.Green
        is_repeated := true
    }

    MaxHealthBonus:bonus := max_health_bonus{
        id := 2
        bonus_string := "Survive them all"
        bonus_color := NamedColors.Violet
        is_repeated := false
    }

    MaxShieldBonus:bonus := max_shield_bonus{
        id := 3
        bonus_string := "Raise the shields"
        bonus_color := NamedColors.Violet
        is_repeated := false
    }

    InvulnerabilityBonus:bonus := invulnerability_bonus{
        id := 4
        bonus_string := "God mode"
        bonus_color := NamedColors.Orange
        is_repeated := false
    }

    InvisibilityBonus:bonus := invisibility_bonus{
        id := 5
        bonus_string := "Fool them all"
        bonus_color := NamedColors.Violet
        is_repeated := true
    }

    RocketLauncherBonus:bonus := rocket_launcher_bonus{
        id := 6
        bonus_string := "Blow them all up"
        bonus_color := NamedColors.Violet
        is_repeated := false
    }

    MinigunBonus:bonus := minigun_bonus{
        id := 7
        bonus_string := "Shoot them all"
        bonus_color := NamedColors.Orange
        is_repeated := false
    }

    ShotgunBonus:bonus := shotgun_bonus{
        id := 8
        bonus_string := "Feed them buckshot"
        bonus_color := NamedColors.Green
        is_repeated := false
    }

    BusBonus:bonus := bus_bonus{
        id := 9
        bonus_string := "Crush them all"
        bonus_color := NamedColors.Orange
        is_repeated := false
    }

    SetupBonusArray():void=
        set Bonuses += array{HealtBonus}
        set Bonuses += array{ShieldBonus}
        set Bonuses += array{MaxHealthBonus}
        set Bonuses += array{MaxShieldBonus}
        set Bonuses += array{InvulnerabilityBonus}
        set Bonuses += array{InvisibilityBonus}
        set Bonuses += array{RocketLauncherBonus}
        set Bonuses += array{MinigunBonus}
        set Bonuses += array{ShotgunBonus}
        set Bonuses += array{BusBonus}
        for(Bonus:Bonuses):
            Print("Bonuses: " + ToString(Bonus.id))
