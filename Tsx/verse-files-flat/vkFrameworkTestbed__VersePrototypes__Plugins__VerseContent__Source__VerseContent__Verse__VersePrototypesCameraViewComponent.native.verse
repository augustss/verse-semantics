# Copyright Epic Games, Inc. All Rights Reserved.

#using { /Verse.org/Temporary/EpicGamesRestricted/Network }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation }

@doc("Component used to associate a camera behavior component with a player")
@experimental
verse_prototypes_player_camera_view<epic_internal><native> := class<epic_internal>(player_camera_view):

<#
# Get player camera view so we can set the active camera
@experimental
GetPlayerCameraView<epic_internal><native>(Player:player)<decides><transacts>:verse_prototypes_player_camera_view

@doc("Activate a camera component for a player, setting it as the active view target")
@experimental
(InCameraComponent:camera_component).ActivateForPlayer<native><epic_internal>(InPlayer:player)<decides><transacts>:void

@doc("Deactivate a camera component for a player, restoring the player's view target to the default")
@experimental
(InCameraComponent:camera_component).DeactivateForPlayer<native><epic_internal>(InPlayer:player)<decides><transacts>:void
#>

# Temp version for testing - This will go away and will definitely not be shipped
@experimental
GetPlayerCameraViewByIndex<epic_internal><native>(PlayerIndex:int)<decides><transacts>:verse_prototypes_player_camera_view

@doc("Activate a camera component for a player, setting it as the active view target")
@experimental
(InCameraComponent:camera_component).ActivateForPlayerAtIndex<native><epic_internal>(PlayerIndex:int)<decides><transacts>:void

@doc("Deactivate a camera component for a player, restoring the player's view target to the default")
@experimental
(InCameraComponent:camera_component).DeactivateForPlayerAtIndex<native><epic_internal>(PlayerIndex:int)<decides><transacts>:void
