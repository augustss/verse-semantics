using { /Fortnite.com/Game }
using { /Verse.org }
using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }
using { /Verse.org/SpatialMath }

##############################################################################################
# This VerseQA map is for demoing the Verse SceneGraph RoundManager and GameEvents feature.
# Verse SceneGraph RoundManager and GameEvents introduces a way to manage 
# the lifetime of experiences in conjunction with SceneGraph.
# It consists of the fort_round_manager interface and fort_game_events_component.
# QA Owner: Holly.Lucas
# Dev Owner: Fredrik.Lindh
##############################################################################################

########################################################
# fort_round_manager goes Live 33.30
########################################################

QA_roundevents_component<public> := class<final_super>(component):

    var Cancelable:[]cancelable = array{}

    ########################################################
    # Gets the fort_round_manager from the sim entity.
    # Subscribes to RoundStarted and RoundEnded.
    ########################################################
    OnAddedToScene<override>():void=
        (super:)OnAddedToScene()
        Print("RoundEventsTest: OnAddedToScene")

        if (RM := Entity.GetFortRoundManager[]):
            set Cancelable += array:
                RM.SubscribeRoundStarted(OnRoundBegin)
                RM.SubscribeRoundEnded(OnRoundEnd)

            Print("RoundEventsRegistered {Cancelable.Length}")
     
    ########################################################
    # During OnBeginSimulation add an entity 
    # with components to scene.
    ########################################################
    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()
        Print("OnBeginSimulation")

        ShroomEntity_Orange := entity{}
        ShroomEntity_Orange.AddComponents of array:
            transform_component:
                Entity := ShroomEntity_Orange
            mesh_component:
                Entity := ShroomEntity_Orange
                Mesh := Meshes.Mushroom01_asset
            sphere_light_component:
                Entity := ShroomEntity_Orange
                ColorFilter := NamedColors.Orange
                CastShadows := true
            
        Entity.AddEntities of array:
            ShroomEntity_Orange

    ########################################################
    # During Round Start add a new entity 
    # with components to scene.
    # This should coincide with a device's OnBegin
    ########################################################
    OnRoundBegin():void=
        Print("RoundEventsTest: OnRoundBegin")

        ShroomEntity_Red := entity{}
        ShroomEntity_Red.AddComponents of array:
            transform_component:
                Entity := ShroomEntity_Red
                LocalTransform := transform:
                    Translation := vector3:
                        Forward := -170.0
                        Right := 170.0
                        Up := 1.0
            mesh_component:
                Entity := ShroomEntity_Red
                Mesh := Meshes.Mushroom04_asset
            sphere_light_component:
                Entity := ShroomEntity_Red
                ColorFilter := NamedColors.Red
                CastShadows := true

        Entity.AddEntities of array:
            ShroomEntity_Red

    OnRoundEnd():void=
        Print("RoundEventsTest: OnRoundEnd")

    OnEndSimulation<override>():void=
        (super:)OnEndSimulation()
        Print("OnEndSimulation")

    OnRemovingFromScene<override>():void=
        (super:)OnRemovingFromScene()
        Print("OnRemovingFromScene")
        CancelEventSubs()

    CancelEventSubs<protected>():void =
        for (C : Cancelable) { C.Cancel()}
            Print("Cleanup: Canceled {Cancelable.Length} Round Events Subscriptions")

            

