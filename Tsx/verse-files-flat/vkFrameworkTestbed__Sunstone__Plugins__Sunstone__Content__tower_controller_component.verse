# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /Fortnite.com/Devices }
using { /Verse.org/SpatialMath }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Game }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Teams }

tower_location := enum:
    None,
    Bottom,
    Mid,
    Top

tower_element_type := enum:
    None,
    Arcane,
    Fire,
    Ice,
    Electric

tower_controller_log := class(log_channel){}

BlankMessage<localizes>:message = ""
HealthMessage<localizes>(DisplayHealth:int, DisplayMaxHealth:int):message = "{DisplayHealth}/{DisplayMaxHealth}"
TowerCategory<localizes>:message = "Tower"
ShieldCategory<localizes>:message = "Shield"
TowerAttackCategory<localizes>:message = "Tower Attack"
DebugCategory<localizes>:message = "Debug"
DebugNameTip<localizes>:message = "The name of the tower in the log."
TimeBetweenAttacksTip<localizes>:message = "The amount of time between each attack."
TargetingTimeTip<localizes>:message = "The amount of time it takes for the tower to play the targeting VFX."
DamageToDealTip<localizes>:message = "The amount of damage the tower does each attack."
SimultaneousTargetsTip<localizes>:message = "The numer of simultaneous tower targets."
TargetingRangeTip<localizes>:message = "The distance from the tower center to acquire targets."

tower_interactable_component<public> := class<final_super>(interactable_component):
    CustomText<localizes> : message = "Start Upgrade"

    InteractMessage<override>(Agent:agent)<computes><decides><reads>:message =
        return CustomText


tower_data_component := class<final_super>(component):
    @editable_slider(float){Categories := array{TowerAttackCategory}, ToolTip := TimeBetweenAttacksTip, MinValue:=option{0.5}, MaxValue:=option{10.0}}
    TimeBetweenAttacks:float = 3.0

    @editable_slider(float){Categories := array{TowerAttackCategory}, ToolTip := TargetingTimeTip, MinValue:=option{0.5}, MaxValue:=option{10.0}}
    TargetingTime:float = 1.0

    @editable_slider(float){Categories := array{TowerAttackCategory}, ToolTip := DamageToDealTip, MinValue:=option{1.0}, MaxValue:=option{2000.0}}
    DamageToDeal:float = 5.0

    @editable_number(int){Categories := array{TowerAttackCategory}, ToolTip := SimultaneousTargetsTip, MinValue:=option{1}, MaxValue:=option{4}}
    SimultaneousTargets:int = 1

    @editable_slider(float){Categories := array{TowerAttackCategory}, ToolTip := TargetingRangeTip, MinValue:=option{10.0}, MaxValue:=option{20000.0}}
    TargetingRange:float = 10.0

shield_data_component := class<final_super>(component):
    @editable{Categories := array{ShieldCategory}}
    MaxShield:float = 500.0

tower_controller_component := class<final_super>(component):
    DebugLog:log = log{Channel := tower_controller_log}

    # Runtime state
    var CurrentLevel:int = 0
    var CurrentShieldLevel:int = 0
    var CurrentElement:tower_element_type = tower_element_type.None
    var PreviousElement:tower_element_type = tower_element_type.None
    var ShieldEntity:?entity = false
    var ElementEntity:?entity = false
    var BeamVFX:[]particle_system_component = array{}
    var TargetImpactVFX:[]particle_system_component = array{}
    var TargetingVFX:[]particle_system_component = array{}
    var MuzzleVFX:[]particle_system_component = array{}
    var DamageVFX:?particle_system_component = false
    var DestroyVFX:?particle_system_component = false
    var Destroy2VFX:?particle_system_component = false
    var Destroy3VFX:?particle_system_component = false
    var LevelUpVFX:?particle_system_component = false
    var LevelUp2VFX:?particle_system_component = false
    var FortDamage:?fort_damageable_component = false
    var TowerData:?tower_data_component = false
    var WorldText:?text_display_component = false
    var ShieldData:?shield_data_component = false
    var Interaction:?interactable_component = false
    var Overlap:?mesh_component = false
    var ShieldDestroyed:logic = false
    var NumberOfTimesDestroyed:int = 0
    var TimeBetweenAttacks:float = 3.0 # TODO: Drive this from a stat
    var TargetingTime:float = 1.0 # TODO: Drive this from a stat
    var DamageToDeal:float = 25.0 # TODO: Drive this from a stat
    var ActiveTowerEntities:[]entity = array{}
    var ActiveShieldEntities:[]entity = array{}
    var LastHealth:float = 1.0
    var DamageEventHandle:?cancelable = false
    var UpgradePanel:?tower_upgrade_panel = false
    var ActiveSubscriptionHandles:[] cancelable = array{}
    DeactivateEvent:event() = event(){}
    ActivatedEvent:event() = event(){}

    @editable{Categories := array{DebugCategory}, ToolTip:=DebugNameTip}
    DebugName:string  = ""

    @editable_number(int){Categories := array{TowerCategory}, MinValue:=option{0}, MaxValue:=option{3}}
    InitialShieldLevel:int = 0

    @editable_number(int){Categories := array{TowerCategory}, MinValue:=option{0}, MaxValue:=option{3}}
    InitialTowerLevel:int = 1

    @editable{Categories := array{TowerCategory}}
    InitialTowerElement:tower_element_type = tower_element_type.None

    @editable{Categories := array{DebugCategory}}
    RunTests:logic = false

    @editable{Categories := array{DebugCategory}}
    RunUpgradeCycling:logic = false

    @editable{Categories := array{TowerCategory}}
    SentryDeviceToDestroyOnDisable:sentry_device = sentry_device{}

    @editable{Categories := array{TowerCategory}}
    TowerLocation : tower_location = tower_location.None

    @editable{Categories := array{TowerCategory}}
    Beacon : beacon_device = beacon_device{}
    @editable{Categories := array{TowerCategory}}
    TimeToShowBeacon : float = 0.0
    var BeaconShowing : logic = false


    LogEntry(LogText:string):void=
        DebugLog.Print("Tower[{DebugName}] {LogText}")

    LevelTags:[]tag = array{tower_level_none{}, tower_level_one{}, tower_level_two{}, tower_level_three{}}
    ElementTags:[]tag = array{no_tower_element{}, arcane_tower_element{}, fire_tower_element{}, ice_tower_element{}, electric_tower_element{}}

    GetElementTag(TowerElement:tower_element_type)<transacts>:tag=
        case(TowerElement):
            tower_element_type.None => return no_tower_element{}
            tower_element_type.Arcane => return arcane_tower_element{}
            tower_element_type.Fire => return fire_tower_element{}
            tower_element_type.Ice => return ice_tower_element{}
            tower_element_type.Electric => return electric_tower_element{}

    OnSimulate<override>()<suspends>:void=
        Initialize()

        if (RunTests?):
            Test()

        if (RunUpgradeCycling?):
            TestUpgradeCycle()

    Initialize():void=
        set CurrentLevel = InitialTowerLevel
        set CurrentElement = InitialTowerElement
        set CurrentShieldLevel = InitialShieldLevel

        SubEntities := Entity.GetEntities()
        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{shield_tag{}}]
            then:
                set ShieldEntity = option{SubEntity}
<#        if (not ShieldEntity?):
            LogEntry("ERROR: No tower shield entity found")
#>
        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{tower_element_tag{}}]
            then:
                set ElementEntity = option{SubEntity}
        if (not ElementEntity?):
            LogEntry("ERROR: No tower element entity found")

        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{explosion_vfx_tag{}}]
                ExplosionVFXComponent := SubEntity.GetComponent[particle_system_component]
            then:
                set DestroyVFX = option{ExplosionVFXComponent}
        if (not DestroyVFX?):
            LogEntry("ERROR: No tower explosion VFX entity found")

        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{explosion_2_vfx_tag{}}]
                ExplosionVFXComponent := SubEntity.GetComponent[particle_system_component]
            then:
                set Destroy2VFX = option{ExplosionVFXComponent}
        if (not Destroy2VFX?):
            LogEntry("ERROR: No tower explosion 2 VFX entity found")

        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{explosion_3_vfx_tag{}}]
                ExplosionVFXComponent := SubEntity.GetComponent[particle_system_component]
            then:
                set Destroy3VFX = option{ExplosionVFXComponent}
        if (not Destroy3VFX?):
            LogEntry("ERROR: No tower explosion 3 VFX entity found")

        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{level_up_vfx_tag{}}]
                LevelUpVFXComponent := SubEntity.GetComponent[particle_system_component]
            then:
                set LevelUpVFX = option{LevelUpVFXComponent}
        if (not LevelUpVFX?):
            LogEntry("ERROR: No tower level up VFX entity found")

        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[array{level_up_2_vfx_tag{}}]
                LevelUpVFXComponent := SubEntity.GetComponent[particle_system_component]
            then:
                set LevelUp2VFX = option{LevelUpVFXComponent}
        if (not LevelUp2VFX?):
            LogEntry("ERROR: No tower level up 2 VFX entity found")

            #UpdateShieldEntities()
        UpdateTowerEntities()

        spawn { AttackLoop() }

    ClearHealth():void=
        if:
            TextDisplay := WorldText?
        then:
            set TextDisplay.Message = BlankMessage

    UpdateHealth():void=
        if:
            Damage := FortDamage?
            TextDisplay := WorldText?
            #Shield := ShieldData?
        then:
            set LastHealth = Damage.Health# + Shield.MaxShield
            MaxHealth := Damage.MaxHealth# + Shield.MaxShield
            if:
                LastHealth > 0.0
                DisplayHealth := Floor[LastHealth] # Remove the floating point for display
                DisplayMaxHealth := Floor[MaxHealth]
            then:
                set TextDisplay.Message = HealthMessage(DisplayHealth,DisplayMaxHealth)
            else:
                set TextDisplay.Message = BlankMessage
        LogEntry("Health: {LastHealth}")

    UpdateShieldEntities():void=
        if:
            Shield:= ShieldEntity?
            LevelTag := LevelTags[CurrentShieldLevel]
        then:
            if (ShieldDestroyed?):
                ExclusiveTagActivation(Shield, array{tower_level_none{}})
            else:
                set ActiveShieldEntities = ExclusiveTagActivation(Shield, array{LevelTag})
        UpdateActiveShield()

    UpdateTowerEntities():void=
        ClearHealth()
        if:
            ElementRoot:= ElementEntity?
            LevelTag := LevelTags[CurrentLevel]
            ElementTag := GetElementTag(CurrentElement)
        then:
            set ActiveTowerEntities = ExclusiveTagActivation(ElementRoot, array{ElementTag, LevelTag})
        UpdateActiveTower()

    UpdateTeam():void=
        if (TeamComponent := Entity.GetComponent[team_component]):
            LogEntry("Updating Team")
            set TeamComponent.TeamTag = TeamComponent.TeamTag
            for (TowerEntity:ActiveTowerEntities):
                UpdateTeam(TowerEntity, TeamComponent.TeamTag)

    UpdateTeam(TowerEntity:entity, TeamTag:team_tag):void=
        if (TowerEntityTeamComponent := TowerEntity.GetComponent[team_component]):
            set TowerEntityTeamComponent.TeamTag = TeamTag
        for (ChildEntity:TowerEntity.GetEntities()):
            UpdateTeam(ChildEntity, TeamTag)

    UpdateActiveShield():void=
        set ShieldData = false
        for (TowerEntity:ActiveShieldEntities):
            ActivateShieldDataEntity(TowerEntity)
        UpdateHealth()
        if (not ShieldData?):
            LogEntry("ERROR: No tower shield entity found")

    UpdateActiveTower():void=
        set DamageVFX = false
        set BeamVFX = array{}
        set MuzzleVFX = array{}
        set TargetImpactVFX = array{}
        set TargetingVFX = array{}
        set WorldText = false
        set Interaction = false
        set Overlap = false
        set TowerData = false
        for (Handle:ActiveSubscriptionHandles):
            Handle.Cancel()
        set ActiveSubscriptionHandles = array{}
        for (TowerEntity:ActiveTowerEntities):
            ActivateDamageEntity(TowerEntity)
            ActivateBeamEntity(TowerEntity)
            ActivateMuzzleEntity(TowerEntity)
            ActivateTargetImpactEntity(TowerEntity)
            ActivateTargetingEntity(TowerEntity)
            ActivateWorldTextEntity(TowerEntity)
            ActivateInteractionEntity(TowerEntity)
            ActivateOverlapEntity(TowerEntity)
            ActivateTowerDataEntity(TowerEntity)
        UpdateHealth()
        UpdateTeam()
        if (not DamageVFX?):
            LogEntry("ERROR: No tower damage entity found")
        if (BeamVFX.Length = 0):
            LogEntry("ERROR: No tower beam VFX entity found")
        if (MuzzleVFX.Length = 0):
            LogEntry("ERROR: No tower muzzle VFX entity found")
        if (TargetImpactVFX.Length = 0):
            LogEntry("ERROR: No tower target impact VFX entity found")
        if (TargetingVFX.Length = 0):
            LogEntry("ERROR: No tower targeting VFX entity found")
        if (not WorldText?):
            LogEntry("ERROR: No tower world text entity found")
        if (not Interaction?):
            LogEntry("ERROR: No tower interact entity found")
        if (not Overlap?):
            LogEntry("ERROR: No tower overlap entity found")
        if (not TowerData?):
            LogEntry("ERROR: No tower data entity found")

        if (LastHealth = 0.0):
            spawn { WaitForPositiveHealth()}

    WaitForPositiveHealth()<suspends>:void=
        loop:
            if (LastHealth = 0.0):
                Sleep(1.0)

                UpdateHealth()
            else:
                UpdateTeam()
                break

    ActivateDamageEntity(TowerEntity:entity):void=
        if:
            TagComp := TowerEntity.GetComponent[tag_component]
            TagComp.HasAll[array{damage_tag{}}]
            DamageVFXComponent := TowerEntity.GetComponent[particle_system_component]
            FortDamageComponent := TowerEntity.GetComponent[fort_damageable_component]
        then:
            set DamageVFX = option{DamageVFXComponent}
            if (Handle := DamageEventHandle?):
                Handle.Cancel()
            set FortDamage = option{FortDamageComponent}
            set DamageEventHandle = option{FortDamageComponent.DamagedEventWithAgent.Subscribe(OnTakeDamage)}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateDamageEntity(SubEntity)

    ActivateBeamEntity(TowerEntity:entity):void=
        if:
            TagComp := TowerEntity.GetComponent[tag_component]
            TagComp.HasAll[array{beam_vfx_tag{}}]
            BeamVFXComponent := TowerEntity.GetComponent[particle_system_component]
        then:
            set BeamVFX += array{BeamVFXComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateBeamEntity(SubEntity)

    ActivateMuzzleEntity(TowerEntity:entity):void=
        if:
            TagComp := TowerEntity.GetComponent[tag_component]
            TagComp.HasAll[array{muzzle_flash_vfx_tag{}}]
            MuzzleVFXComponent := TowerEntity.GetComponent[particle_system_component]
        then:
            set MuzzleVFX += array{MuzzleVFXComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateMuzzleEntity(SubEntity)

    ActivateTargetImpactEntity(TowerEntity:entity):void=
        if:
            TagComp := TowerEntity.GetComponent[tag_component]
            TagComp.HasAll[array{target_impact_vfx_tag{}}]
            TargetImpactVFXComponent := TowerEntity.GetComponent[particle_system_component]
        then:
            set TargetImpactVFX += array{TargetImpactVFXComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateTargetImpactEntity(SubEntity)

    ActivateTargetingEntity(TowerEntity:entity):void=
        if:
            TagComp := TowerEntity.GetComponent[tag_component]
            TagComp.HasAll[array{targeting_vfx_tag{}}]
            TargetingVFXComponent := TowerEntity.GetComponent[particle_system_component]
        then:
            set TargetingVFX += array{TargetingVFXComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateTargetingEntity(SubEntity)

    ActivateWorldTextEntity(TowerEntity:entity):void=
        if:
            TextDisplayComponent := TowerEntity.GetComponent[text_display_component]
        then:
            set WorldText = option{TextDisplayComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateWorldTextEntity(SubEntity)

    ActivateInteractionEntity(TowerEntity:entity):void=
        if:
            InteractComponent := TowerEntity.GetComponent[interactable_component]
        then:
            set Interaction = option{InteractComponent}
            set ActiveSubscriptionHandles += array{InteractComponent.InteractFinishedEvent.Subscribe(OnTowerInteract)}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateInteractionEntity(SubEntity)

    ActivateOverlapEntity(TowerEntity:entity):void=
        if:
            CollisionComponent := TowerEntity.GetComponent[mesh_component]
        then:
            set Overlap = option{CollisionComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateOverlapEntity(SubEntity)

    ActivateTowerDataEntity(TowerEntity:entity):void=
        if:
            TowerDataComponent := TowerEntity.GetComponent[tower_data_component]
        then:
            set TowerData = option{TowerDataComponent}
        else:
            for (SubEntity : TowerEntity.GetEntities()):
                ActivateTowerDataEntity(SubEntity)

    ActivateShieldDataEntity(ShieldDataEntity:entity):void=
        if:
            ShieldDataComponent := ShieldDataEntity.GetComponent[shield_data_component]
        then:
            set ShieldData = option{ShieldDataComponent}
        else:
            for (SubEntity : ShieldDataEntity.GetEntities()):
                ActivateShieldDataEntity(SubEntity)

    DestroyTower():void=

        set NumberOfTimesDestroyed = NumberOfTimesDestroyed + 1
        DeactivateEvent.Signal()

        SentryDeviceToDestroyOnDisable.DestroySentry()

        case(CurrentLevel):
            1 => if (VFX := DestroyVFX?) then VFX.Enable()
            2 => if (VFX := Destroy2VFX?) then VFX.Enable()
            3 => if (VFX := Destroy3VFX?) then VFX.Enable()
            _ => {}
        SwitchElement(tower_element_type.None)

        # Find a player on the towers team and use that player to emit the destroyed message to friendlies via a message device
        if:
            TowerLocation <> tower_location.None
            Agent := FindTowerTeamMate[]
            GEM := global_events_accessor{}.Instance[Entity]
        then:
            case(TowerLocation):
                tower_location.Bottom => GEM.TriggerEvent(event_type.Tower, sub_events.TowerDownBottom, ?Agent:= option{Agent})
                tower_location.Mid => GEM.TriggerEvent(event_type.Tower, sub_events.TowerDownMid, ?Agent:= option{Agent})
                tower_location.Top => GEM.TriggerEvent(event_type.Tower, sub_events.TowerDownTop, ?Agent:= option{Agent})
                _ =>

     <#   OnDestroyShields() # Destroy shields too. Ideally we would do this when health was damaged more than shields, but we don't have a MaxHealth value exposed right now.
        UpdateTowerEntities() #>

    OnDestroyShields():void=
        set ShieldDestroyed = true
        # TODO: Play shield destruction VFX
        UpdateShieldEntities()

    FindTowerTeamMate()<transacts><decides>:agent=
        TeamComponent := Entity.GetComponent[team_component]
        Team := TeamComponent.GetTeam[]

        Entity.GetPlayspaceForEntity[].GetTeamCollection().GetAgents[Team][0]

    OnTakeDamage(DamageData: tuple(float, ?agent)):void=

        if:
            Agent := DamageData(1)?
            TeamComponent := Entity.GetComponent[team_component]
            Team := TeamComponent.GetTeam[]
            TeamCollection := Entity.GetPlayspaceForEntity[].GetTeamCollection()
            TeamMate:= TeamCollection.GetAgents[Team].Find[Agent]
            Damage := FortDamage?
        then:
            # this is a teammate - ignore it and undo anything fort_damageable_component did
            set Damage.Health = Damage.Health + DamageData(0)
            return

        if (not BeaconShowing?):
            set BeaconShowing = true
            spawn:
                ShowBeacon()

        if:
            TowerLocation <> tower_location.None
            Agent := FindTowerTeamMate[]
            GEM := global_events_accessor{}.Instance[Entity]
        then:
            case(TowerLocation):
                tower_location.Bottom => GEM.TriggerEvent(event_type.Tower, sub_events.TowerUnderAttackBottom, ?Agent:= option{Agent})
                tower_location.Mid => GEM.TriggerEvent(event_type.Tower, sub_events.TowerUnderAttackMid, ?Agent:= option{Agent})
                tower_location.Top => GEM.TriggerEvent(event_type.Tower, sub_events.TowerUnderAttackTop, ?Agent:= option{Agent})
                _ =>

        if (VFX := DamageVFX?):
            VFX.Enable()
        UpdateHealth()
        LogEntry("Damaged Tower: {DamageData(0)}")
        if:
            Damage := FortDamage?
            Damage.Health <= 0.0
            #Shield := ShieldData?
            #(Damage.Health + Shield.MaxShield) <= 0.0
        then:
            DestroyTower()

    ShowBeacon()<suspends>:void=
        Beacon.Enable()
        Sleep(TimeToShowBeacon)
        Beacon.Disable()
        set BeaconShowing = false

    OnTowerInteract(Agent:agent):void=
        if (UpgradeMenu := UpgradePanel?):
            UpgradeMenu.OpenUpgradeMenu(Agent)

    Test()<suspends>:void=
        <#
        if (Shield:= ShieldEntity?):
            loop:
                for (TheLevel:LevelTags):
                    Sleep(5.0)
                    ExclusiveTagActivation(Shield, array{TheLevel})#>
        if (Element:= ElementEntity?):
            loop:
                for (TheElement:ElementTags):
                    for (TheLevel:LevelTags):
                        Sleep(3.0)
                        ExclusiveTagActivation(Element, array{TheElement, TheLevel})

    TestUpgradeCycle()<suspends>:void=
        loop:
            LogEntry("Starting Tower Upgrade Cycle")
            for (ElementIndex := 0..ElementList.Length-1):
                if:
                    NewElement := ElementList[ElementIndex]
                then:
                    SwitchElement(NewElement)
                else:
                    SwitchElement(tower_element_type.None)
                Sleep(1.0)
                UpgradeTower()
                UpgradeShields()
                LogEntry("Ugraded to 1")
                Sleep(5.0)
                UpgradeTower()
                UpgradeShields()
                LogEntry("Upgraded to 2")
                Sleep(5.0)
                UpgradeTower()
                UpgradeShields()
                LogEntry("Upgraded to 3")
                Sleep(5.0)
                set CurrentShieldLevel = 0
                set CurrentLevel = 0
                UpdateShieldEntities()
                UpdateTowerEntities()
                LogEntry("Reset Tower Level")
                Sleep(1.0)

    ElementList:[]tower_element_type = array:
        tower_element_type.None,
        tower_element_type.Arcane,
        tower_element_type.Fire,
        tower_element_type.Ice,
        tower_element_type.Electric

    UpgradeShields():void=
        if (CurrentShieldLevel >= LevelTags.Length):
            return

        set CurrentShieldLevel += 1
        set ShieldDestroyed = false
        UpdateShieldEntities()

    RestoreTower():void=
        DeactivateEvent.Signal()

        case(CurrentLevel):
            1 => if (VFX := DestroyVFX?) then VFX.Disable()
            2 => if (VFX := Destroy2VFX?) then VFX.Disable()
            3 => if (VFX := Destroy3VFX?) then VFX.Disable()
            _ => {}

        SwitchElement(PreviousElement)

        if(Damage := FortDamage?):
            set Damage.Health = Damage.MaxHealth

        ActivatedEvent.Signal()

    UpgradeTower():void=
        if (CurrentLevel >= LevelTags.Length):
            return

        # Restore if destroyed and don't level
        if (IsDestroyed[]):
            RestoreTower()
            return

        if (CurrentElement = tower_element_type.None):
            return

        if (CurrentLevel = 1):
            if (VFX := LevelUpVFX?):
                VFX.Enable()
        if (CurrentLevel = 2):
            if (VFX := LevelUp2VFX?):
                VFX.Enable()

        set CurrentLevel += 1
        spawn{FinishTowerUpgrade()}

    FinishTowerUpgrade()<suspends>:void=
        Sleep(2.0)
        DeactivateEvent.Signal()
        UpdateTowerEntities()
        ActivatedEvent.Signal()

    SwitchElement(NewElement:tower_element_type):void=
        set PreviousElement = CurrentElement
        set CurrentElement = NewElement
        LogEntry("Changing Tower Element to: {GetElementTag(CurrentElement)}")
        UpdateTowerEntities()

    # Attack characters within range
    AttackLoop()<suspends>:void=
        loop:
            if (CurrentLevel > 0):
                race:
                    DeactivateEvent.Await()
                    loop:
                        TargetedCharacters := LookForTargetsInRange()
                        if (TowerAttackData := TowerData?):
                            # Play the targeting VFX
                            for (TargetIndex := 0..Min(TargetedCharacters.Length, TowerAttackData.SimultaneousTargets)):
                                if:
                                    VFXTargetingComponent := TargetingVFX[TargetIndex]
                                    TargetLocation := ConvertVector(TargetedCharacters[TargetIndex].GetTransform().Translation)
                                then:
                                    SetBeamEnd(VFXTargetingComponent, TargetLocation)
                                    VFXTargetingComponent.Enable()

                            Sleep(TargetingTime)

                            # Play the attack VFX and apply damage to the target
                            for (TargetIndex := 0..Min(TargetedCharacters.Length, TowerAttackData.SimultaneousTargets)):
                                if (Target := TargetedCharacters[TargetIndex]):
                                    if (VFXBeamComponent := BeamVFX[TargetIndex]):
                                        TargetLocation := ConvertVector(Target.GetTransform().Translation)
                                        SetBeamEnd(VFXBeamComponent, TargetLocation)
                                        VFXBeamComponent.Enable()

                                    if (MuzzleVFXComponent := MuzzleVFX[TargetIndex]):
                                        TargetLocation := ConvertVector(Target.GetTransform().Translation)
                                        SetBeamEnd(MuzzleVFXComponent, TargetLocation)
                                        MuzzleVFXComponent.Enable()

                                    if (VFXTargetComponent := TargetImpactVFX[TargetIndex]):
                                        TargetLocation := ConvertVector(Target.GetTransform().Translation)
                                        SetBeamEnd(VFXTargetComponent, TargetLocation)
                                        VFXTargetComponent.Enable()

                                    Target.Damage(DamageToDeal)

                        # Wait until we can target again
                        Sleep(TimeBetweenAttacks - TargetingTime)
            ActivatedEvent.Await()

    LookForTargetsInRange()<suspends>:[]fort_character_component=
        if:
            TowerAttackData := TowerData?
            TargetingComponent := Entity.GetComponent[tower_targeting_component]
        then:
            return TargetingComponent.LookForTargetsInRange(TowerAttackData.TargetingRange)
        else:
            LogEntry("ERROR: No tower targeting component found")
        Sleep(Inf)
        return array{}

    ExclusiveTagActivation(TargetEntity:entity, EntityTags:[]tag):[]entity=
        DisableEntitiesWithoutTags(TargetEntity, EntityTags)
        return EnableEntitiesWithTags(TargetEntity, EntityTags)

    EnableEntitiesWithTags(TargetEntity:entity, EntityTags:[]tag):[]entity=
        var EntitiesWithTags:[]entity = array{}
        SubEntities := TargetEntity.GetEntities()
        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                TagComp.HasAll[EntityTags]
                Components := SubEntity.GetComponents()
            then:
                for (TheComponent:Components):
                    if (EnableableComponent := enableable[TheComponent]):
                        EnableableComponent.Enable()
                set EntitiesWithTags += array{SubEntity}
            set EntitiesWithTags += EnableEntitiesWithTags(SubEntity, EntityTags)
        return EntitiesWithTags

    DisableEntitiesWithoutTags(TargetEntity:entity, EntityTags:[]tag):void=
        SubEntities := TargetEntity.GetEntities()
        for (SubEntity:SubEntities):
            if:
                TagComp := SubEntity.GetComponent[tag_component]
                not TagComp.HasAll[EntityTags]
                Components := SubEntity.GetComponents()
            then:
                for (TheComponent:Components):
                    if (EnableableComponent := enableable[TheComponent]):
                        EnableableComponent.Disable()
            DisableEntitiesWithoutTags(SubEntity, EntityTags)

    IsDestroyed()<decides><transacts>:void=
        CurrentElement = tower_element_type.None


    #HACK SOLUTION SINCE WE DON'T HAVE PROPERTY INTERFACES
    SetBeamEnd(ParticleSystem:particle_system_component, BeamEnd:vector3):void=
        if (Vfx := VFXAssets.Niagara.FireTower.NS_FireTower_Beam[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.FireTower.NS_FireTower_AttackImpacts[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.IceTower.NS_IceTower_AttackImpact[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.ArcaneTower.NS_ArcaneTower_Beam[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.ArcaneTower.NS_ArcaneTower_AttackImpact[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.LightningTower.NS_LightningTower_Beam[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }
        else if (Vfx := VFXAssets.Niagara.LightningTower.NS_LightningTower_AttackImpact[ParticleSystem]) { set Vfx.BeamEnd = BeamEnd }