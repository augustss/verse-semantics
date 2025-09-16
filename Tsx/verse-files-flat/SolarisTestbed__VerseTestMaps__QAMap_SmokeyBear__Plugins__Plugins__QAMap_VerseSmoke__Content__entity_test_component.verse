using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SceneGraph }
using { /Verse.org/Colors }

# A Verse-authored component that can be added to entities
entity_test_component<public> := class<direct>(component):

    @editable
    var Duration<public>:float = 5.0
   
    OnSimulate<override>()<suspends>:void =
        # Query for entities tagged with sbear_head_tag
        for:
            TaggedEntity : Entity.FindEntitiesWithTag(sbear_body_tag{})
            Parent := TaggedEntity.GetParent[]
            
        do:
            Print("Removing from parent")
            TaggedEntity.RemoveFromParent()
            # Check if entity is detached
            if (Parent = TaggedEntity.GetParent[]):
                Print("[FAILURE] Entity NOT successfully removed from parent entity. | TaggedEntity.RemoveFromParent[]", ?Color := NamedColors.Red)
            else:
                Print("[SUCCESS] Entity successfully removed from parent entity.| TaggedEntity.RemoveFromParent[]", ?Color := NamedColors.LightGreen)
            Sleep(Duration)
            Print("Adding back to parent")
            Parent.AddEntities(array{TaggedEntity})
            # Check if entity is attached
            if (Parent = TaggedEntity.GetParent[]):
                Print("[SUCCESS] Entity successfully added to entity.| Parent.AddEntities()", ?Color := NamedColors.LightGreen)
            else:
                Print("[FAILURE] Entity NOT successfully added to entity.| Parent.AddEntities()", ?Color := NamedColors.Red)




                







