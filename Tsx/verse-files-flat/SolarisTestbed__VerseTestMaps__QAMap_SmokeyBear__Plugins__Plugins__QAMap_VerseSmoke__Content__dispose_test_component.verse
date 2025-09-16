using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SceneGraph }
using { /Verse.org/Colors }

# A Verse-authored component that can be added to entities
dispose_test_component<public> := class<direct>(component):

    @editable
    var Duration<public>:float = 10.0
  

    OnSimulate<override>()<suspends>:void =
        Sleep(Duration)
        # Remove component
        if (EntityComponent : mesh_component = Entity.GetComponent[mesh_component]):
            EntityComponent.RemoveFromEntity()
            Print("Remove component")
            # Check if component is removed
            if(not EntityComponent.IsInScene[]):
                Print("[SUCCESS] Component has been removed from the scene. | EntityComponent.IsInScene[]", ?Color := NamedColors.LightGreen)
            else:
                Print("[FAILURE] Component has not been removed from the scene | EntityComponent.IsInScene[]", ?Color := NamedColors.Red)
            Sleep(Duration)
        # Remove Entity from the scene
            Print("Remove entity from the scene")
            Entity.RemoveFromParent() 
        # Check if entity is in the scene
        if (not Entity.IsInScene[]):
            Print("[SUCCESS] Entity is not in the scene | Entity.IsInScene[]", ?Color := NamedColors.LightGreen)
        else:
            Print("[FAILURE] Entity is in the scene | Entity.IsInScene[]", ?Color := NamedColors.LightGreen)

        
        
        
