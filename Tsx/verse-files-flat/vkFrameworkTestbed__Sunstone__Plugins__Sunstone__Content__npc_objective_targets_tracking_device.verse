
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level.
# Tracking explicitly the targets destruction as can't get generically or check health
npc_objective_targets_tracking_device := class(creative_device):

    @editable
    ShieldGenerator1:objective_device = objective_device{}
    
    @editable
    ShieldGenerator2:objective_device = objective_device{}
    
    @editable
    BaseObjective:objective_device = objective_device{}

    var ShieldGenerator1Destroyed:logic = false
    var ShieldGenerator2Destroyed:logic = false
    var BaseObjectiveDestroyed:logic = false


    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        
        # Commenting out as can't get through the barriers to get to the shields
        # ShieldGenerator1.DestroyedEvent.Subscribe(OnShieldGen1Destroyed)
        # ShieldGenerator2.DestroyedEvent.Subscribe(OnShieldGen2Destroyed)
        BaseObjective.DestroyedEvent.Subscribe(OnBaseObjectiveGen1Destroyed)
    
    OnShieldGen1Destroyed(Agent:agent):void=
        set ShieldGenerator1Destroyed = true
    
    OnShieldGen2Destroyed(Agent:agent):void=
        set ShieldGenerator2Destroyed = true
    
    OnBaseObjectiveGen1Destroyed(Agent:agent):void=
        set BaseObjectiveDestroyed = true

    GetActiveHighPriorityTargets():[]objective_device=
        var ActiveTargets:[]objective_device = array{}

        # Commenting out as can't get through the barriers to get to the shields
        # if(not ShieldGenerator1Destroyed?):
        #     set ActiveTargets = ActiveTargets + array{ShieldGenerator1}
        
        # if(not ShieldGenerator2Destroyed?):
        #     set ActiveTargets = ActiveTargets + array{ShieldGenerator2}
                
        if(not BaseObjectiveDestroyed?):
            set ActiveTargets = ActiveTargets + array{BaseObjective}
        
        return ActiveTargets