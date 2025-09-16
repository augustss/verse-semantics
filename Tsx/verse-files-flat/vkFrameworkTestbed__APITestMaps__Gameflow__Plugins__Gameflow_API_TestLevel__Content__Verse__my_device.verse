
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Fortnite.com/Game }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Vehicles }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation }

device_script_log := class(log_channel){}

# Your new device script
my_device := class<concrete>(creative_device):

    Logger:log = log{Channel := device_script_log}
    VehicleError:string := "Error getting player Vehicle (Are you in one?)"

    # Basic GoldenPath Buttons for Team/Player Info
    @editable
    GetPlayersButtonDevice:button_device := button_device{}
    @editable
    GetTeamsButtonDevice:button_device := button_device{}
    @editable
    CheckTeamButtonDevice:button_device := button_device{}

    # Character Golden Path
    @editable
    var GetCharacterTransformButtonDevice:button_device := button_device{}
    @editable
    var DamageCharacterButtonDevice:button_device := button_device{}
    @editable
    HealCharacterButtonDevice:button_device := button_device{}
    @editable
    GetCharacterHealthButtonDevice:button_device := button_device{}
    @editable
    SetCharacterHealthButtonDevice:button_device := button_device{}
    @editable
    GetCharacterMaxHealthButtonDevice:button_device := button_device{}
    @editable
    SetCharacterMaxHealthButtonDevice:button_device := button_device{}
    @editable
    GetCharacterShieldButtonDevice:button_device := button_device{}
    @editable
    SetCharacterShieldButtonDevice:button_device := button_device{}
    @editable
    GetCharacterMaxShieldButtonDevice:button_device := button_device{}
    @editable
    SetCharacterMaxShieldButtonDevice:button_device := button_device{}
    
    # Vehicle Golden Path
    @editable
    GetVehicleTransformTrigger:trigger_device := trigger_device{}
    @editable
    GetVehicleTransformTrigger2:trigger_device := trigger_device{}
    @editable
    DamageVehicleTrigger:trigger_device := trigger_device{}
    @editable
    GetVehicleHealthTrigger:trigger_device := trigger_device{}
    @editable
    SetVehicleHealthTrigger:trigger_device := trigger_device{}
    @editable
    GetVehicleMaxHealthTrigger:trigger_device := trigger_device{}
    @editable
    SetVehicleMaxHealthTrigger:trigger_device := trigger_device{}

    # Negative Team/Player Testing Buttons
    @editable
    SetHealthNegativeDevice:button_device := button_device{}
    @editable
    SetShieldNegativeDevice:button_device := button_device{}
    @editable
    SetMaxHealthNegativeDevice:button_device := button_device{}
    @editable
    SetMaxShieldNegativeDevice:button_device := button_device{}
    @editable
    SetVehicleHealthNegativeDevice:trigger_device := trigger_device{}
    @editable
    SetVehicleMaxHealthNegativeDevice:trigger_device := trigger_device{}
        

    OnBegin<override>()<suspends>:void=
        Logger.Print("Hello Devices!")

        # Set up Golden Path Buttons
        GetPlayersButtonDevice.InteractedWithEvent.Subscribe(HandlePlayerButtonDevicePushed)
        GetTeamsButtonDevice.InteractedWithEvent.Subscribe(HandleTeamsButtonDevicePushed)
        CheckTeamButtonDevice.InteractedWithEvent.Subscribe(HandleCheckTeamButtonDevicePushed)

        # Set up Golden Path Character Buttons
        GetCharacterTransformButtonDevice.InteractedWithEvent.Subscribe(HandleGetCharacterTransformButtonPushed)
        DamageCharacterButtonDevice.InteractedWithEvent.Subscribe(HandleDamageCharacterButtonDevice)
        HealCharacterButtonDevice.InteractedWithEvent.Subscribe(HandleHealCharacterButtonDevice)
        GetCharacterHealthButtonDevice.InteractedWithEvent.Subscribe(HandleGetCharacterHealthButtonDevice)
        SetCharacterHealthButtonDevice.InteractedWithEvent.Subscribe(HandleSetCharacterHealthButtonDevice)
        GetCharacterMaxHealthButtonDevice.InteractedWithEvent.Subscribe(HandleGetCharacterMaxHealthButtonDevice)
        SetCharacterMaxHealthButtonDevice.InteractedWithEvent.Subscribe(HandleSetCharacterMaxHealthButtonDevice)
        GetCharacterShieldButtonDevice.InteractedWithEvent.Subscribe(HandleGetCharacterShieldButtonDevice)
        SetCharacterShieldButtonDevice.InteractedWithEvent.Subscribe(HandleSetCharacterShieldButtonDevice)
        GetCharacterMaxShieldButtonDevice.InteractedWithEvent.Subscribe(HandleGetCharacterMaxShieldButtonDevice)
        SetCharacterMaxShieldButtonDevice.InteractedWithEvent.Subscribe(HandleSetCharacterMaxShieldButtonDevice)

        # Set up Vehicle Trigger Events
        GetVehicleTransformTrigger.TriggeredEvent.Subscribe(HandleGetVehicleTransformTrigger)
        GetVehicleTransformTrigger2.TriggeredEvent.Subscribe(HandleGetVehicleTransformTrigger)
        DamageVehicleTrigger.TriggeredEvent.Subscribe(HandleVehicleDamageTrigger)
        GetVehicleHealthTrigger.TriggeredEvent.Subscribe(HandleVehicleGetHealthTrigger)
        SetVehicleHealthTrigger.TriggeredEvent.Subscribe(HandleVehicleSetHealthTrigger)
        GetVehicleMaxHealthTrigger.TriggeredEvent.Subscribe(HandleVehicleGetMaxHealthTrigger)
        SetVehicleMaxHealthTrigger.TriggeredEvent.Subscribe(HandleVehicleSetMaxHealthTrigger)

        # Set up Negative Test Buttons
        SetHealthNegativeDevice.InteractedWithEvent.Subscribe(HandleSetHealthNegativePushed)
        SetShieldNegativeDevice.InteractedWithEvent.Subscribe(HandleSetShieldNegativePushed)
        SetMaxHealthNegativeDevice.InteractedWithEvent.Subscribe(HandleSetCharacterMaxHealthNegativeDevice)
        SetMaxShieldNegativeDevice.InteractedWithEvent.Subscribe(HandleSetCharacterMaxShieldNegativeDevice)
        SetVehicleHealthNegativeDevice.TriggeredEvent.Subscribe(HandleVehicleSetHealthNegativeTrigger)
        SetVehicleMaxHealthNegativeDevice.TriggeredEvent.Subscribe(HandleVehicleSetMaxHealthNegativeTrigger)


        # Configure Event subscriptions

        for (MyAgent : Self.GetPlayspace().GetPlayers()):
            if:
                Char := MyAgent.GetFortCharacter[]
            then:
                HandlePlayerEventSubscriptions(Char)
            else:
                Print("Error! Cannot get the Player/Fort_character retrieval. Cannot bind event subscriptions")
            

    # Check to get the correct number of players in the game
    HandlePlayerButtonDevicePushed(MyAgent:agent):void=
        Print("Testing Getting All Players")
        Print("There are {Self.GetPlayspace().GetPlayers().Length} player(s) in the game")

    # Check that there are the correct number of possible Teams in the game
    HandleTeamsButtonDevicePushed(MyAgent:agent):void=
        Print("Testing Getting All Teams")
        Print("There are {Self.GetPlayspace().GetTeamCollection().GetTeams().Length} team(s) in the game")

    # TODO Expand Team Checks

    # Report on which team the player is on
    HandleCheckTeamButtonDevicePushed(MyAgent:agent):void=
        var IsOnTeam:logic := false

        Print("Checking what Team the Player is on")
        if:
            PlayerTeam:team := Self.GetPlayspace().GetTeamCollection().GetTeam[MyAgent]
            AllTeams:[]team = Self.GetPlayspace().GetTeamCollection().GetTeams()
        then:
            for( Index := 0..AllTeams.Length - 1)
            {
                if (PlayerTeam = AllTeams[Index]):
                    Print("Player is on Team {Index + 1}")
                    set IsOnTeam = true
            }
            if (IsOnTeam = false):
                Print("Couldn't detect the player on any team!")
        else:
            Print("Error getting Team from player. Fundamental system break")

    ##################### GOLDEN PATH CHARACTER TESTS #####################

    # Check Player's Transform
    HandleGetCharacterTransformButtonPushed(MyAgent:agent):void=
        Print("Testing Getting Player Transform")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
            Transform := FortCharacter.GetTransform()
            Rotation := FortCharacter.GetViewRotation()
        then:
            Print("Current player translation is ({Transform.Translation})")
            Print("Current player rotation is {Transform.Rotation}")
            Print("Current player scale is {Transform.Scale}")
            Print("Current Player View Rotation is {Rotation}")
        else:
            Print("Error getting Translation!")

    # Check Player can be damaged
    HandleDamageCharacterButtonDevice(MyAgent:agent):void=
        dmg := 15.0
        Print("Testing damaging the player for {dmg} damage")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            startHealth := FortCharacter.GetHealth()
            FortCharacter.Damage(dmg)
            Print("Player has taken {startHealth - FortCharacter.GetHealth()} damage - Should be {dmg} (confirming via button worked)")
        else:
            Print("Error getting the FortCharacter")

    # Check Player Can Be healed
    HandleHealCharacterButtonDevice(MyAgent:agent):void=
        heal := 10.0
        Print("Testing healing the player for {heal} health")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            startHealth := FortCharacter.GetHealth()
            FortCharacter.Heal(heal)
            Print("Healing the player for {FortCharacter.GetHealth() - startHealth} health - Should be {heal} if player was hurt (Confirming via Button worked)")
        else:
            Print("Error getting the FortCharacter")

    # Check Getting Player's Health
    HandleGetCharacterHealthButtonDevice(MyAgent:agent):void=
        Print("Testing check the Player's current health")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            Print("Player has {FortCharacter.GetHealth()} Health")
        else:
            Print("Error getting the FortCharacter")

    # Check Setting Player's Health
    HandleSetCharacterHealthButtonDevice(MyAgent:agent):void=
        health := 14.0
        Print("Testing setting the Player's health to {health} HPs")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetHealth(health)
            Print("Verifying Player Health is now {FortCharacter.GetHealth()}")
        else:
            Print("Error getting the FortCharacter")
                
    # Check Getting Player's Max Health
    HandleGetCharacterMaxHealthButtonDevice(MyAgent:agent):void=
        Print("Testing Check the player's Maximum Health")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            Print("Player has {FortCharacter.GetMaxHealth()} Max Health")
        else:
            Print("Error getting the FortCharacter")

    # Check Setting Player's Max Health
    HandleSetCharacterMaxHealthButtonDevice(MyAgent:agent):void=
        health := 200.0
        Print("Testing set the Player's Max Health to {health}")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetMaxHealth(health)
            Print("Verifying Player Max Health is {FortCharacter.GetMaxHealth()}")
        else:
            Print("Error getting the FortCharacter")
        
    # Check Getting Player's Shield
    HandleGetCharacterShieldButtonDevice(MyAgent:agent):void=
        Print("Testing Get Player's current Shields")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            Print("Player has {FortCharacter.GetShield()} Shields")
        else:
            Print("Error getting the FortCharacter")

    # Check setting player's Shield
    HandleSetCharacterShieldButtonDevice(MyAgent:agent):void=
        val := 34.0
        Print("Testing set Player Shields to {val}")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetShield(val)
            Print("Verifying Player Shield is {FortCharacter.GetShield()}")
        else:
            Print("Error getting the FortCharacter")

    # Check Getting Player's Max Shield
    HandleGetCharacterMaxShieldButtonDevice(MyAgent:agent):void=
        Print("Testing get the Player's Max Shields")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            Print("Player has {FortCharacter.GetMaxShield()} Max Shields")
        else:
            Print("Error getting the FortCharacter")

    # Check Setting Player's Max Shield
    HandleSetCharacterMaxShieldButtonDevice(MyAgent:agent):void=
        val := 300.0
        Print("Testing set the Player's Max shields to {val}")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetMaxShield(val)
            Print("Verifying Player Max Shield is {FortCharacter.GetMaxShield()}")
        else:
            Print("Error getting the FortCharacter")

    ##################### GOLDEN PATH VEHICLE TESTS #####################

    # Check Vehicle's Transform
    HandleGetVehicleTransformTrigger(MyAgent:?agent):void=
        Print("Testing Player Vehicle's Transform")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Print("Vehicle Transform is {Vehicle.GetTransform().Translation}")
        else:
            Print(VehicleError)

    # Check Vehicle getting Damaged
    HandleVehicleDamageTrigger(MyAgent:?agent):void=
        dmg := 25.0
        Print("Testing Damaging the Player's Vehicle for {dmg} damage")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Vehicle.Damage(dmg)
            Print("Vehicle damaged for {dmg}")
        else:
            Print(VehicleError)

    # Check Get Vehicle Health
    HandleVehicleGetHealthTrigger(MyAgent:?agent):void=
        Print("Testing getting the Player's Vehicle's Current Health")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Print("Vehicle currently has {Vehicle.GetHealth()} health")
        else:
            Print(VehicleError)

    # Check Setting Vehicle's Health
    HandleVehicleSetHealthTrigger(MyAgent:?agent):void=
        val := 127.0
        Print("Testing setting Player's Vehicle's Health to {val}")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Vehicle.SetHealth(val)
            Print("Verifying Vehicle is set to {Vehicle.GetHealth()} Health")
        else:
            Print(VehicleError)
    
    # Check Getting Vehicle's Max Health
    HandleVehicleGetMaxHealthTrigger(MyAgent:?agent):void=
        Print("Testing getting Player's Vehicle's Max Health")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Print("Vehicle has {Vehicle.GetMaxHealth()} Max Health")
        else:
            Print(VehicleError)

    # Check Setting Vehicle's Max Health
    HandleVehicleSetMaxHealthTrigger(MyAgent:?agent):void=
        val := 400.0
        Print("Testing setting Player's Vehicle's Max Health to {val}")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Print("Attempting to set vehicle Max Health to {val}")
            Vehicle.SetMaxHealth(val)
            Print("Verifying vehicle set to {Vehicle.GetMaxHealth()} Max Health")
        else:
            Print(VehicleError)

    # ##################### NEGATIVE TEST FUNCTIONS #####################

    # Negative Health Test
    HandleSetHealthNegativePushed(MyAgent:agent):void=
        health := -15.0
        Print("Testing setting the Player's health to {health} HPs")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetHealth(health)
            Print("Verifying Player Health is now {FortCharacter.GetHealth()}")
            #Sleep(5.0)
        else:
            Print("Error getting the FortCharacter")

    # Negative Shield Test
    HandleSetShieldNegativePushed(MyAgent:agent):void=
        shield := -15.0
        Print("Testing setting the Player's shield to {shield} HPs")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetShield(shield)
            Print("Verifying Player Shield is now {FortCharacter.GetShield()}")
        else:
            Print("Error getting the FortCharacter")    

    # Negative Max health Test
    HandleSetCharacterMaxHealthNegativeDevice(MyAgent:agent):void=
        health := -200.0
        Print("Testing set the Player's Max Health to {health}")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetMaxHealth(health)
            Print("Verifying Player Max Health is {FortCharacter.GetMaxHealth()}")
        else:
            Print("Error getting the FortCharacter")

    # Negative Max Shield Test
    HandleSetCharacterMaxShieldNegativeDevice(MyAgent:agent):void=
        val := -300.0
        Print("Testing set the Player's Max shields to {val}")
        if:
            FortCharacter := MyAgent.GetFortCharacter[]
        then:
            FortCharacter.SetMaxShield(val)
            Print("Verifying Player Max Shield is {FortCharacter.GetMaxShield()}")
        else:
            Print("Error getting the FortCharacter")

    # Negative Vehicle Health Test
    HandleVehicleSetHealthNegativeTrigger(MyAgent:?agent):void=
        val := -127.0
        Print("Testing setting Player's Vehicle's Health to {val}")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Vehicle.SetHealth(val)
            Print("Verifying Vehicle is set to {Vehicle.GetHealth()} Health")
        else:
            Print(VehicleError)

    # Negative Vehicle Health Test
    HandleVehicleSetMaxHealthNegativeTrigger(MyAgent:?agent):void=
        val := -400.0
        Print("Testing setting Player's Vehicle's Max Health to {val}")
        if:
            Vehicle := MyAgent?.GetFortCharacter[].GetVehicle[]
        then:
            Vehicle.SetHealth(val)
            Print("Verifying Vehicle is set to {Vehicle.GetHealth()} Max Health")
        else:
            Print(VehicleError)
    


    ##################### Initialize Player for Event Subscriptions #####################
    # Done in a more general scope to protect from being Garbage collected
    HandlePlayerEventSubscriptions(Char:fort_character):void=
        Char.CrouchedEvent().Subscribe(HandleCrouch)
        Char.JumpedEvent().Subscribe(HandleJump)
        Char.SprintedEvent().Subscribe(HandleSprint)
        Char.DamagedEvent().Subscribe(HandleDamaged)
        Char.HealedEvent().Subscribe(HandleHealed)

    #TODO: Will need to expand to properly test params, but event itself can still be tested
    HandleCrouch(Char:fort_character, State:logic):void=
        if:
            State = true
        then:
            Print("Player is crouching.")
        else:
            Print("Player has stopped crouching.")

    #TODO: Will need to expand to properly new params, but event itself can still be tested
    HandleJump(Char:fort_character):void=
        Print("Player is jumping!")

    #TODO: Will need to expand to properly new params, but event itself can still be tested
    HandleSprint(Char:fort_character, State:logic):void=
        if:
            State = true
        then:
            Print("Player has started sprinting.")
        else:
            Print("Player has stopped sprinting.")

    #TODO: Will need to expand to properly new params, but event itself can still be tested
    HandleDamaged(Result:damage_result):void=
        Print("Player has taken {Result.Amount} Damage (Event Subscription)")

    #TODO: Will need to expand to properly new params, but event itself can still be tested
    HandleHealed(Result:healing_result):void=
        Print("Player is healed for {Result.Amount} (Event Subscription)")