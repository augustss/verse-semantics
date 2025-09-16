# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Temporary/EpicGamesRestricted/Network }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Allows the target entity to be replicated to the client
@experimental
verse_prototypes_camera_target_component<epic_internal><native> := class<final_super>(component):
    
    @replicated("")
    @experimental
    @editable
    var TargetEntity<public><native> : ?entity = false