# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
@available{MinUploadedAtFNVersion := 3200} # castable_subtype version dependency
name_component<public> := class<final_super>(component):

    @editable
    Name<public>:string = ""
