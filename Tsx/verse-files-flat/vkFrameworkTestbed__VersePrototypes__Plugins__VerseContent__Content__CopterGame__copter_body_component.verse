# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Concurrency }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Derived camera component for helicopter
@experimental
copter_body_component := class<final_super>(component) {}