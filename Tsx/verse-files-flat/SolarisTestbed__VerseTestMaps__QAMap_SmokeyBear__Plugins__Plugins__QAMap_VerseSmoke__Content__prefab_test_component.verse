using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SceneGraph }
using { /Verse.org/Colors }
using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

# A Verse-authored component that can be added to entities
prefab_test_component<public> := class<direct>(component):

    # A custom variable you can expose to the editor
    @editable
    var Duration<public>:float = 5.0
  
    OnSimulate<override>()<suspends>:void =      
        SBearPrefab := Prefabs.sbear_prefab{}
        # Attach prefab to entity
        Entity.AddEntities of array:
            SBearPrefab
        SBearBodies := SBearPrefab.FindEntitiesWithTag(sbear_body_tag{})
        if: 
            EntityComponent : tag_component = SBearBodies[0].GetComponent[tag_component]
            if (EntityComponent.IsSimulating[]):
                Print("[SUCCESS] Prefab has been properly created. | EntityComponent.IsSimulating[]", ?Color := NamedColors.LightGreen)
            else:
                Print("[FAILURE] Prefab has NOT been properly created. | EntityComponent.IsSimulating[]", ?Color := NamedColors.Red)
        else:
            Print("[FAILURE No tag component found! | SBearBodies[0].GetComponent[tag_component]", ?Color := NamedColors.Red)
        
        # Attach component to prefab
        for:
            SBearBody : SBearBodies
        do:
            if (EntityComponent : mesh_component = SBearBody.GetComponent[mesh_component]):  
                SBearBody.AddComponents of array:
                    spin_around_component:
                        Entity := Entity
                        YawRotationRate := -75.0
                        PitchRotationRate := 0.0
                        RollRotationRate := 0.0
            else:
                Print("[FAILURE] Prefab mesh component NOT FOUND! | SBearBody.GetComponent[mesh_component]", ?Color := NamedColors.Red)

        if (EntityComponent : spin_around_component = SBearBodies[0].GetComponent[spin_around_component]):
            if (EntityComponent.IsSimulating[]):
                Print("[SUCCESS] Component successfully added. | EntityComponent.IsSimulating[]", ?Color := NamedColors.LightGreen)
            else:
                Print("[FAILURE] Component NOT successfully added! | EntityComponent.IsSimulating[]", ?Color := NamedColors.Red)
        else:
            Print("[FAILURE No rotation_component found! | SBearBodies[0].GetComponent[rotation_component]", ?Color := NamedColors.Red)

        Sleep(Duration)




