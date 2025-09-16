
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SpatialMath }
using { /Verse.org/Assets }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Game }
using { /Verse.org/SceneGraph }

UpgradeTowerText<localizes>(Cost:int):message = "Upgrade {Cost} Gold"
RebuildTowerText<localizes>(Cost:int):message = "Rebuild {Cost} Gold"
UpgradeTowerMissingText<localizes>(Cost:int):message = "Upgrade [Missing] {Cost} Gold"
RebuildTowerMissingText<localizes>(Cost:int):message = "Rebuild [Missing] {Cost} Gold"
UpgradeTowerUnavailableText<localizes>:message = "Upgrade Unavailable"
UpgradeShieldsText<localizes>(Cost:int):message = "Upgrade Shields {Cost} Gold"
UpgradeShieldsMissingText<localizes>(Cost:int):message = "Shields [Missing] {Cost} Gold"
UpgradeShieldsUnavailableText<localizes>:message = "Upgrade Shields Unavailable"
FireElementText<localizes>(Cost:int):message = "Change To Fire {Cost} Gold"
FireElementMissingText<localizes>(Cost:int):message = "Fire [Missing] {Cost} Gold"
IceElementText<localizes>(Cost:int):message = "Change To Ice {Cost} Gold"
IceElementMissingText<localizes>(Cost:int):message = "Ice [Missing] {Cost} Gold"
ElectricElementText<localizes>(Cost:int):message = "Change To Lightning {Cost} Gold"
ElectricElementMissingText<localizes>(Cost:int):message = "Lightning [Missing] {Cost} Gold"
ArcaneElementText<localizes>(Cost:int):message = "Change To Arcane {Cost} Gold"
ArcaneElementMissingText<localizes>(Cost:int):message = "Arcane [Missing] {Cost} Gold"
CloseText<localizes>:message = "Close"

tower_upgrade_panel_log := class(log_channel){}

rebuild_costs_post_destruction := class<concrete>:
    @editable
    RebuildInteractionTime:float = 30.0

    @editable
    RebuildInteractionCost:int = 200    


tower_upgrade_panel := class(creative_device):
    @editable
    UpgradeTowerDialog:popup_dialog_device = popup_dialog_device{}

    @editable
    Button:button_device = button_device{}

    @editable
    PaymentButton:conditional_button_device = conditional_button_device{}

    @editable
    TowerLevelCost:[]int = array{50, 100, 300}

    @editable
    ShieldLevelCost:[]int = array{50, 100, 200}
    
    @editable
    BaseUpgradeTimes:[]float = array{5.0, 10.0, 15.0}

    @editable
    TowerRebuildCostsAndTimes:[]rebuild_costs_post_destruction = array{
                                                                        rebuild_costs_post_destruction{RebuildInteractionCost:= 200, RebuildInteractionTime:= 30.0},
                                                                        rebuild_costs_post_destruction{RebuildInteractionCost:= 300, RebuildInteractionTime:= 35.0},
                                                                        rebuild_costs_post_destruction{RebuildInteractionCost:= 400, RebuildInteractionTime:= 40.0}
                                                                }

    @editable
    SwitchElementCost:int = 200

    @editable
    Tower:?entity = false

    var SubscriptionHandles:[]cancelable = array{}

    DebugLog:log = log{Channel := tower_upgrade_panel_log}

    LogEntry(LogText:string):void=
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            DebugLog.Print("Tower[{TowerController.DebugName}] {LogText}")
        else:
            DebugLog.Print("TowerPanel {LogText}")

    OnBegin<override>()<suspends>:void=
        Button.InteractedWithEvent.Subscribe(OpenUpgradeMenu)
        UpgradeTowerDialog.SetButtonCount(7)

        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            set TowerController.UpgradePanel = option{Self}
            if:
                InteractComponent := TowerController.Interaction?
            then:
                InteractComponent.InteractFinishedEvent.Subscribe(OpenUpgradeMenu)
                DebugLog.Print("Waiting for Tower Interact")

            # Hack to ensure team is setup properly
            Sleep(2.0)
            TowerController.UpdateTeam()

    GetRebuildCostData(TowerController:tower_controller_component)<transacts>: rebuild_costs_post_destruction =
        if(NumberEntries:= TowerRebuildCostsAndTimes.Length > 0):
            IndexToUse:= TowerController.NumberOfTimesDestroyed-1
            if(NumberEntries > IndexToUse, RebuildData:= TowerRebuildCostsAndTimes[IndexToUse]):
                return RebuildData
            else if (HighestData:= TowerRebuildCostsAndTimes[NumberEntries-1]):
                return HighestData

        return rebuild_costs_post_destruction{RebuildInteractionCost:= 100, RebuildInteractionTime:= 5.0}
        
    GetUpgradeCostData(TowerController:tower_controller_component)<transacts>: rebuild_costs_post_destruction =
        if:
            TowerController.IsDestroyed[]
        then:
            GetRebuildCostData(TowerController)
        else if:            
                BuildCost:= TowerLevelCost[TowerController.CurrentLevel]
                UpgradeTime:= BaseUpgradeTimes[TowerController.CurrentLevel]
        then:
            return rebuild_costs_post_destruction{RebuildInteractionCost:= BuildCost, RebuildInteractionTime:= UpgradeTime}
        
        return rebuild_costs_post_destruction{RebuildInteractionCost:= 100, RebuildInteractionTime:= 5.0}

    UpgradeTowerSelected(Agent:agent):void=
        LogEntry("Upgrade Tower Selected")
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
            TowerLevelCost.Length > TowerController.CurrentLevel or TowerController.IsDestroyed[]
            Cost := GetUpgradeCostData(TowerController)
        then:
            LogEntry("Upgrade Tower Payment")
            PaymentButton.SetItemCountRequired(0, Cost.RebuildInteractionCost)
            SucceedHandle := PaymentButton.ActivatedEvent.Subscribe(OnUpgradeTowerSucceeded)
            FailHandle := PaymentButton.NotEnoughItemsEvent.Subscribe(OnUpgradeTowerFailed)
            set SubscriptionHandles += array{SucceedHandle, FailHandle}
            PaymentButton.Activate(Agent)
 
    UpgradeShieldsSelected(Agent:agent):void=
        LogEntry("Upgrade Shield Selected")
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
            ShieldLevelCost.Length > TowerController.CurrentShieldLevel
            Cost := ShieldLevelCost[TowerController.CurrentShieldLevel]
        then:
            LogEntry("Upgrade Shield Payment")
            PaymentButton.SetItemCountRequired(0, Cost)
            SucceedHandle := PaymentButton.ActivatedEvent.Subscribe(OnUpgradeShieldsSucceeded)
            FailHandle := PaymentButton.NotEnoughItemsEvent.Subscribe(OnUpgradeShieldsFailed)
            set SubscriptionHandles += array{SucceedHandle, FailHandle}
            PaymentButton.Activate(Agent)
                
    OpenUpgradeMenu(Agent:agent):void=
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            if:
                TowerLevelCost.Length > TowerController.CurrentLevel or TowerController.IsDestroyed[]
            then:
                UpgradeCostData:= GetUpgradeCostData(TowerController)
                PaymentButton.SetItemCountRequired(0, UpgradeCostData.RebuildInteractionCost)
                if (PaymentButton.HasAllItems[Agent]):
                    if (TowerController.IsDestroyed[]):
                        UpgradeTowerDialog.SetButtonText(RebuildTowerText(UpgradeCostData.RebuildInteractionCost), 0)
                    else:
                        UpgradeTowerDialog.SetButtonText(UpgradeTowerText(UpgradeCostData.RebuildInteractionCost), 0)
                else:
                    if (TowerController.IsDestroyed[]):
                        UpgradeTowerDialog.SetButtonText(RebuildTowerMissingText(UpgradeCostData.RebuildInteractionCost), 0)
                    else:
                        UpgradeTowerDialog.SetButtonText(UpgradeTowerMissingText(UpgradeCostData.RebuildInteractionCost), 0)
            else:
                UpgradeTowerDialog.SetButtonText(UpgradeTowerUnavailableText , 0)
<#
            if:
                ShieldLevelCost.Length > TowerController.CurrentShieldLevel
                ShieldUpgradeCost := ShieldLevelCost[TowerController.CurrentShieldLevel]
            then:
                PaymentButton.SetItemCountRequired(0, ShieldUpgradeCost)
                if (PaymentButton.HasAllItems[Agent]):
                    UpgradeTowerDialog.SetButtonText(UpgradeShieldsText(ShieldUpgradeCost) , 1)
                else:
                    UpgradeTowerDialog.SetButtonText(UpgradeShieldsMissingText(ShieldUpgradeCost) , 1)
            else:
                UpgradeTowerDialog.SetButtonText(UpgradeShieldsUnavailableText , 1)
#>
        PaymentButton.SetItemCountRequired(0, SwitchElementCost)
        if (PaymentButton.HasAllItems[Agent]):
            UpgradeTowerDialog.SetButtonText(FireElementText(SwitchElementCost) , 1)
            UpgradeTowerDialog.SetButtonText(ArcaneElementText(SwitchElementCost) , 2)
            UpgradeTowerDialog.SetButtonText(IceElementText(SwitchElementCost) , 3)
            UpgradeTowerDialog.SetButtonText(ElectricElementText(SwitchElementCost) , 4)
        else:
            UpgradeTowerDialog.SetButtonText(FireElementMissingText(SwitchElementCost) , 1)
            UpgradeTowerDialog.SetButtonText(ArcaneElementMissingText(SwitchElementCost) , 2)
            UpgradeTowerDialog.SetButtonText(IceElementMissingText(SwitchElementCost) , 3)
            UpgradeTowerDialog.SetButtonText(ElectricElementMissingText(SwitchElementCost) , 4)
        UpgradeTowerDialog.SetButtonText(CloseText, 5)
        UpgradeHandle := UpgradeTowerDialog.RespondingButtonEvent.Subscribe(OnUpgradeSelected)
        set SubscriptionHandles += array{UpgradeHandle}
        UpgradeTowerDialog.Show(Agent)

    OnUpgradeSelected(Agent:agent, ButtonIndex:int):void=
        CancelSubscribers()
        case(ButtonIndex):
            0 => UpgradeTowerSelected(Agent)
#            1 => UpgradeShieldsSelected(Agent)
            1 => SelectElement(Agent, tower_element_type.Fire)
            2 => SelectElement(Agent, tower_element_type.Arcane)
            3 => SelectElement(Agent, tower_element_type.Ice)
            4 => SelectElement(Agent, tower_element_type.Electric)
            _ => {}

    CancelSubscribers():void=
        for (Handle:SubscriptionHandles):
            Handle.Cancel()
        set SubscriptionHandles = array{}

    OnUpgradeTowerFailed(Agent:agent):void=
        LogEntry("Failed to upgrade tower")
        CancelSubscribers()

    OnUpgradeShieldsFailed(Agent:agent):void=
        LogEntry("Failed to upgrade shields")
        CancelSubscribers()
            
    OnUpgradeTowerSucceeded(Agent:agent):void=
        LogEntry("Upgrade tower")
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            LogEntry("Upgrading tower")
            TowerController.UpgradeTower()

            if (GEM := global_events_accessor{}.Instance[Self]):
                GEM.TriggerEvent(event_type.Tower, sub_events.TowerUpgraded, ?Agent:= option{Agent})

        CancelSubscribers()

    OnUpgradeShieldsSucceeded(Agent:agent):void=
        LogEntry("Upgrade shield")
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            LogEntry("Upgrading shield")
            TowerController.UpgradeShields()  
        CancelSubscribers()      
        
    SelectElement(Agent:agent, Element:tower_element_type):void=
        PaymentButton.SetItemCountRequired(0, SwitchElementCost)
        set DesiredElement = Element
        SucceedHandle := PaymentButton.ActivatedEvent.Subscribe(OnSwitchElementSucceeded)
        FailHandle := PaymentButton.NotEnoughItemsEvent.Subscribe(OnSwitchElementFailed)
        set SubscriptionHandles += array{SucceedHandle, FailHandle}
        PaymentButton.Activate(Agent)

    OnSwitchElementFailed(Agent:agent):void=
        LogEntry("Failed to switch element")
        CancelSubscribers()
            
    OnSwitchElementSucceeded(Agent:agent):void=
        LogEntry("Switch element")
        if:
            TowerEntity := Tower?
            TowerController := TowerEntity.GetComponent[tower_controller_component]
        then:
            TowerController.SwitchElement(DesiredElement)
            LogEntry("Element Switched")
        CancelSubscribers()

    var DesiredElement:tower_element_type = tower_element_type.None

