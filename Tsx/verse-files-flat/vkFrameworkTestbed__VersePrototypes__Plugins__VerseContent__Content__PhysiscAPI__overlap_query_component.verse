using { /Verse.org }
using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }
using { /Verse.org/SpatialMath }

# A Verse-authored component that can be added to entities
overlap_query_component<public> := class<final_super>(component):

    # A custom variable you can expose to the editor
    @editable
    var QueryOffset:vector3 = vector3{ Left:=0.0, Forward:=0.0, Up:=0.0 }
  
    @editable
    LightEntity:?entity = false

    var NumOverlappingComponents:int = 0
    var IndicatorLightComponent:?capsule_light_component = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()

        if (ChildEntity := LightEntity?):
            if (ChildLightComponent := ChildEntity.GetComponent[capsule_light_component]):
                set IndicatorLightComponent = option{ ChildLightComponent }
        else:
            ChildEntities := Entity.GetEntities()
            for (ChildEntity : ChildEntities):
                if (ChildLightComponent := ChildEntity.GetComponent[capsule_light_component]):
                    set IndicatorLightComponent = option{ ChildLightComponent }


    OnSimulate<override>()<suspends>:void =
        # TODO: Place simple suspends logic to run for this component here
        loop:
            UpdateOverlaps()
            Sleep(0.0)


    OnEndSimulation<override>():void =
        (super:)OnEndSimulation()


    UpdateOverlaps():void =
        EntityTransform := Entity.GetGlobalTransform()
        OverlapTransform := transform:
            Translation := EntityTransform.Translation + QueryOffset
            Rotation := EntityTransform.Rotation
            Scale := EntityTransform.Scale

        set NumOverlappingComponents = 0

        for:
            Overlap : Entity.FindOverlapHits(OverlapTransform)
            OverlapEntity := Overlap.TargetComponent.Entity
            OverlapEntityName := OverlapEntity.GetComponent[name_component].Name
        do:
            set NumOverlappingComponents = NumOverlappingComponents + 1
            Print("{OverlapEntityName}")
        
        UpdateIndicatorLight()

    UpdateIndicatorLight():void = 
        Print("NumOverlaps={NumOverlappingComponents}")
        if (LightComponent := IndicatorLightComponent?):
            if (NumOverlappingComponents > 1):
                set LightComponent.ColorFilter = NamedColors.Red
            else if (NumOverlappingComponents > 0):
                set LightComponent.ColorFilter = NamedColors.Orange
            else:
                set LightComponent.ColorFilter = NamedColors.Green
