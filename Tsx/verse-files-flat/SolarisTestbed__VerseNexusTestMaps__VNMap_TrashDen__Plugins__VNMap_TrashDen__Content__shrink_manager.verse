using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }


shrinkables := class<concrete>():
    @editable
    TrashManipulator: prop_manipulator_device = prop_manipulator_device {}
    @editable
    Prop: creative_prop = creative_prop {}
    var ShrinkMode: logic = false
    @editable
    var ShrinkGrowthAmount: float = 0.01

    ToggleShrinkMode():void =
        if (ShrinkMode?):
            set ShrinkMode = false
        else:
            set ShrinkMode = true
    InitTrashManipulators():void=
        TrashManipulator.DamagedEvent.Subscribe(OnPropDamaged)
        Print ("Manipulator initialized============")

    OnPropDamaged(Agent: agent):void=
        ScaleProp()
        Print ("Successfully damaged")

    ScaleProp():void =
        PropScale:= Prop.GetTransform().Scale.X
        if (ShrinkMode?):
            Shrink:= vector3{X:= PropScale - ShrinkGrowthAmount, Y:= PropScale - ShrinkGrowthAmount, Z:= PropScale - ShrinkGrowthAmount}
            Transform:= transform
                {
                Translation:= Prop.GetTransform().Translation,
                Rotation:= Prop.GetTransform().Rotation,
                Scale:= Shrink
                }
            if (Prop.TeleportTo[Transform]) {}
        else:
            Grow:= vector3{X:= PropScale + ShrinkGrowthAmount, Y:= PropScale + ShrinkGrowthAmount, Z:= PropScale + ShrinkGrowthAmount}
            Transform:= transform
                {
                Translation:= Prop.GetTransform().Translation,
                Rotation:= Prop.GetTransform().Rotation,
                Scale:= Grow
                }
            if (Prop.TeleportTo[Transform]) {}
            
shrink_manager := class(creative_device):

    @editable
    ShrinkableTrash:[]shrinkables = array {}
    @editable
    ShrinkOrGrowTrigger: trigger_device = trigger_device{}



    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin shrink_manager======")
        ShrinkOrGrowTrigger.TriggeredEvent.Subscribe(OnShrinkToggle)
        for (Shrinkable: ShrinkableTrash):
            Shrinkable.InitTrashManipulators()
            
    OnEnd<override>():void=
        Print ("======Nexus OnEnd shrink_manager======")
    
    OnShrinkToggle(Agent: ?agent):void=
        for (Shrinkable: ShrinkableTrash):
            Shrinkable.ToggleShrinkMode()