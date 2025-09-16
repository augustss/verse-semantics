# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Your new core entity component
@experimental
core_entity_component_example := class<final_super>(component):
  
 <# Initialize<override>():void=
        #TODO: The component is being initialized, called when it is attached to an entity
        (super:)Initialize() #>

 <# AddedToWorld<override>():void=
        #TODO: Notification called then the component owner entity was added to a world
        (super:)AddedToWorld() #>

 <# BeginPlay<override>():void=
        #TODO: Place logic to run when the component starts simulating here
        (super:)BeginPlay() #>

 <# EndPlay<override>():void=
        #TODO: Add cleanup logic here. Will be run when the component stop simulating
        (super:)EndPlay() #>

 <# RemovingFromWorld<override>():void=
        #TODO: Notification called when the component owner entity is about to be removed from world
        (super:)RemovingFromWorld() #>

 <# Uninitialize<override>():void=
        #TODO: The component is being uninitialized, called when it is about to be detached from an entity
        (super:)Uninitialize() #>

 <# @editable
    @display_name("Custom Variable")
    @category("Custom Category")
    @display_priority("1")
    @short_tool_tip("A custom var")
    @tool_tip("A custom variable you can expose to the editor.")
    @doc("A custom variable you can expose to the editor")
    var MyCustomInt<public>:int=10 #>

