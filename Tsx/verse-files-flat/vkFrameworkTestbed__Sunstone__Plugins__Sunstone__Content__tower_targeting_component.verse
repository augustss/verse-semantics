# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Fortnite.com/Game }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Teams }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org }
using { /Verse.org/Assets }
using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }

tower_targeting_log := class(log_channel){}

player_targeting_range_ui_handler := class<concrete>():
    DebugLog:log = log{Channel := tower_targeting_log}
    
    var MaybeSavedEnteredTargetingRangeWidget:?widget = false
    var MaybeSavedPlayer:?player = false
    
    # Set to true to 
    UseImageBasedWidget:logic = false

    ##################################################################    
    # Text widget
    InTargetingRangeMessage<localizes>:message = "In enemy tower range!"
    MakeTargetingTextWidget():widget=
        Targetingwidget := text_block{DefaultText := InTargetingRangeMessage, DefaultTextColor := NamedColors.OrangeRed}
        Targetingwidget.SetJustification(text_justification.Center)
        return Targetingwidget
    ##################################################################    

    ##################################################################    
    # Image widget
    InTargetingRangeUIMessageImage:texture = Textures.T_ClassRing_Heavy
    MakeTargetingImageWidget():widget=
        Targetingwidget := texture_block{DefaultImage := InTargetingRangeUIMessageImage}
        return Targetingwidget
    ##################################################################    

    # Edit fuction to choose between image and text widget
    MakeTargetingWidget():widget=
        if (UseImageBasedWidget = true):
            return MakeTargetingImageWidget()
        else:
            return MakeTargetingTextWidget()

    MakeTargetingWidgetOverlay():widget=
        Targetingwidget := MakeTargetingWidget()

        MrginValue := 10.0 
        Padding := margin{Left := MrginValue, Right := MrginValue, Top := MrginValue, Bottom := MrginValue}
        OverlaySlot := overlay_slot{Widget := Targetingwidget, HorizontalAlignment := horizontal_alignment.Center, VerticalAlignment := vertical_alignment.Bottom, Padding := Padding}
        TargetingUIOverlay:overlay := overlay{}
        TargetingUIOverlay.AddWidget(OverlaySlot)
        return TargetingUIOverlay

    AddWidget():void=
        DebugLog.Print("AddWidget")
        if(S := MaybeSavedPlayer?):
            DebugLog.Print("Has MaybeSavedPlayer")

        if(S := MaybeSavedEnteredTargetingRangeWidget?):
            DebugLog.Print("Has MaybeSavedEnteredTargetingRangeWidget")

        if:
            SavedPlayer := MaybeSavedPlayer?
            SavedPlayerUI := GetPlayerUI[SavedPlayer]
        then:
            DebugLog.Print("Adding targeting widget")
            TargetingWidgetOverlay := MakeTargetingWidgetOverlay()
            set MaybeSavedEnteredTargetingRangeWidget = option{TargetingWidgetOverlay}
            SavedPlayerUI.AddWidget(TargetingWidgetOverlay)

    RemoveWidget():void=
        DebugLog.Print("RemoveWidget")
        if:
            SavedPlayer := MaybeSavedPlayer?
            SavedPlayerUI := GetPlayerUI[SavedPlayer]
            SavedTargetingRangeWidget := MaybeSavedEnteredTargetingRangeWidget?
        then:
            DebugLog.Print("Removing targeting widget")
            SavedPlayerUI.RemoveWidget(SavedTargetingRangeWidget)
            set MaybeSavedEnteredTargetingRangeWidget = false
            set MaybeSavedPlayer = false

    HideWidget():void=
        DebugLog.Print("HideWidget")
        if:
            SavedTargetingRangeWidget := MaybeSavedEnteredTargetingRangeWidget?
        then:
            DebugLog.Print("Hiding targeting widget")
            SavedTargetingRangeWidget.SetVisibility(widget_visibility.Collapsed)

    ShowWidget():void=
        DebugLog.Print("ShowWidget")
        if:
            SavedTargetingRangeWidget := MaybeSavedEnteredTargetingRangeWidget?
        then:
            DebugLog.Print("Showing targeting widget")
            SavedTargetingRangeWidget.SetVisibility(widget_visibility.Visible)
        else:
            AddWidget()

target_data := class:
    TargetDistance:float
    Target:fort_character_component

tower_targeting_component := class<final_super>(component):
    DebugLog:log = log{Channel := tower_targeting_log}


    @editable
    TargetedEffect : visual_effect_powerup_device = visual_effect_powerup_device{}
    @editable
    TargetedEffectClearTrigger : trigger_device = trigger_device{}
    
    @editable
    DisplayInTargetingRangeUIMessage:logic = true
    var PlayerUIWidgetMap:[player]player_targeting_range_ui_handler = map{}

    var TargetingLocation:vector3 = vector3{}
    var TargetingVolume:collision_volume = collision_sphere{Radius:=1.0}
    var TransformComponent:?transform_component = false
    var TeamComponent:?team_component = false
    var CollisionComponent:?mesh_component = false
    var MyPlayspace:?fort_playspace = false
    var TeamCollection:?fort_team_collection = false
    StopUICheck:event() = event(){}

    GetLocation()<decides><transacts>:(/Verse.org/SpatialMath:)vector3=
        return TransformComponent?.GetGlobalTransform().Translation


    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()
        set TransformComponent = option{Entity.GetComponent[transform_component]}   
        set TeamComponent = option{Entity.GetComponent[team_component]}  
        set MyPlayspace = option{Entity.GetPlayspaceForEntity[]}
        set TeamCollection = option{MyPlayspace?.GetTeamCollection()}

        set CollisionComponent = option{Entity.GetComponent[mesh_component]}
        if:
            DisplayInTargetingRangeUIMessage = true
            Collision := CollisionComponent?
        then:
            DebugLog.Print("Registering for Overlaps")

            Collision.EntityEnteredEvent.Subscribe(OnBeginOverlap)
            Collision.EntityExitedEvent.Subscribe(OnEndOverlap)
        UpdateTargetingRange(1000.0)

    OnEndSimulation<override>():void=
        (super:)OnEndSimulation()
        ResetPlayerUIWidgetMap()

    ResetPlayerUIWidgetMap():void=
        DebugLog.Print("ResetPlayerUIWidgetMap")
        for:
            UIHandler : PlayerUIWidgetMap
        do:
            UIHandler.RemoveWidget()
        set PlayerUIWidgetMap = map{} 
    
    TryAddUIHandler(InPlayer:player)<transacts>:void=
        DebugLog.Print("TryAddUIHandler")
        if:
            not PlayerUIWidgetMap[InPlayer]
            NewUIHandler := player_targeting_range_ui_handler{MaybeSavedPlayer := option{InPlayer}}
            set PlayerUIWidgetMap[InPlayer] = NewUIHandler

    GetUIHandler(InPlayer:player, TryCreate:logic)<decides><transacts>:player_targeting_range_ui_handler=
        DebugLog.Print("GetUIHandler")
        if (TryCreate = true):
            TryAddUIHandler(InPlayer)
        return PlayerUIWidgetMap[InPlayer]

    DeactivateUIOnDisable(OverlappingEntity:entity)<suspends>:void=
        race:
            StopUICheck.Await()
            loop:    
                if:
                    Tower := Entity.GetComponent[tower_controller_component]
                    Tower.IsDestroyed[]
                then:
                    for:
                        CharacterComponent : OverlappingEntity.FindDescendantComponents(fort_character_component)
                        IsEnemy(CharacterComponent) = true
                        ResultPlayer := player[CharacterComponent.GetAgent[]]
                        UIHandler := GetUIHandler[ResultPlayer, false]
                    do:
                        UIHandler.HideWidget()
                        TargetedEffectClearTrigger.Trigger(ResultPlayer)

                Sleep(0.0)

    OnBeginOverlap(OverlappingEntity:entity):void=
        DebugLog.Print("OnBeginOverlap")

        for:
            CharacterComponent : OverlappingEntity.FindDescendantComponents(fort_character_component)
            IsEnemy(CharacterComponent) = true
            ResultPlayer := player[CharacterComponent.GetAgent[]]
            UIHandler := GetUIHandler[ResultPlayer, true]
        do:
            StopUICheck.Signal()
            
            spawn:
                DeactivateUIOnDisable(OverlappingEntity)

            UIHandler.ShowWidget()
            TargetedEffect.Pickup(ResultPlayer)

    OnEndOverlap(OverlappingEntity:entity):void=
        DebugLog.Print("OnEndOverlap")
        
        for:
            CharacterComponent : OverlappingEntity.FindDescendantComponents(fort_character_component)
            ResultPlayer := player[CharacterComponent.GetAgent[]]
            UIHandler := GetUIHandler[ResultPlayer, false]
        do:
            UIHandler.HideWidget()
            TargetedEffectClearTrigger.Trigger(ResultPlayer)
            StopUICheck.Signal()

    UpdateTargetingRange(TargetingRange:float):void=
        if:
            NewTargetingLocation := GetLocation[]
        then:
            set TargetingLocation = NewTargetingLocation
            set TargetingVolume = collision_sphere{Radius:=TargetingRange}

    ToString<public>(Attitude: team_attitude):string =
        case(Attitude):
            team_attitude.Friendly => "Friendly"
            team_attitude.Neutral => "Neutral"
            team_attitude.Hostile => "Hostile"

    IsEnemy(FortCharacterComponent:fort_character_component)<transacts>:logic=
        if:
            Attitude := TeamCollection?.GetTeamAttitude[FortCharacterComponent.GetAgent[], Entity]
            (Attitude = team_attitude.Friendly)
        then:
            return false
        return true

    SortByClosest(Targets:[]target_data):[]fort_character_component=
        OrderedTargets := Sort(Targets, ClosestTarget)
        return for (NextTarget:OrderedTargets):
            NextTarget.Target

    Sort(Xs:[]t, Less(:t, :t)<decides><transacts>:void where t:type):[]t =
        ArrayLength := Xs.Length
        var Result:[]t = Xs
        for(i:=1..ArrayLength; negative_j:=-i..-1):
            j:=-negative_j
            if:
                Less[Result[j], Result[j-1]]
                Temp := Result[j]
                set Result[j] = Result[j-1] or Err()
                set Result[j-1] = Temp
        return Result

    ClosestTarget(X:target_data, Y:target_data)<decides><transacts>:void = X.TargetDistance < Y.TargetDistance

    LookForTargetsInRange(TargetingRange:float)<suspends>:[]fort_character_component=
        UpdateTargetingRange(TargetingRange)
        var Targets:[]target_data = array{}
        loop:
            for: 
                Overlap : Entity.FindOverlapHits(transform{Translation:=TargetingLocation, Scale:=Ones()}, TargetingVolume)
                SourceComponent := Overlap.SourceComponent?
                FortCharacterComponent : SourceComponent.Entity.FindDescendantComponents(fort_character_component)
                IsEnemy(FortCharacterComponent) = true
                TargetLocation := ConvertVector(FortCharacterComponent.GetTransform().Translation)
                TargetDistance := (/Verse.org/SpatialMath:)Distance(TargetLocation, GetLocation[])
            do:
                DebugLog.Print("Found enemy character component via overlap.")
                set Targets += array{target_data{TargetDistance := TargetDistance, Target:=FortCharacterComponent}}

            if (Targets.Length > 0):
                return SortByClosest(Targets)
            Sleep(1.0)
