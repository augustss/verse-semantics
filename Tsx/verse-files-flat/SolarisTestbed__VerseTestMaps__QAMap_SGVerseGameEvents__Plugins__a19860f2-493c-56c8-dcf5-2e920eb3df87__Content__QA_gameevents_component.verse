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
# fort_game_events_component goes Live 34.20
########################################################

QA_gameevents_component<public> := class<final_super>(component):

    var Cancelables:[]cancelable = array{}

    ########################################################
    # Gets the fort_game_events_component.
    # Subscribes to lifetime events under that component.
    ########################################################
    OnAddedToScene<override>():void=
        (super:)OnAddedToScene()
        Print("GameEventsTest: OnAddedToScene")

        if (GEC := Entity.GetFortGameEvents[]):
            set Cancelables += array:
                GEC.SetupGameEvent.Subscribe(OnSetupGame)
                GEC.RoundWarmupEvent.Subscribe(OnRoundWarmup)
                GEC.RoundStartEvent.Subscribe(OnBeginRound)
                GEC.RoundEndEvent.Subscribe(OnEndRound)
                GEC.EndGameEvent.Subscribe(OnEndGame)
            
            Print("GameEventsTest: GameEventsRegistered {Cancelables.Length}")

    ########################################################
    # During Game Setup add an entity 
    # with components to scene.
    ########################################################
    OnSetupGame():void=
        Print("GameEventsTest: OnSetupGame")

        ShroomEntity_Blue := entity{}
        ShroomEntity_Blue.AddComponents of array:
            transform_component:
                Entity := ShroomEntity_Blue
                LocalTransform := transform:
                    Translation := vector3:
                        Forward := 700.0
                        Right := 1.0
                        Up := 1.0
            mesh_component:
                Entity := ShroomEntity_Blue
                Mesh := Meshes.Mushroom03_asset
            sphere_light_component:
                Entity := ShroomEntity_Blue
                ColorFilter := NamedColors.Blue
                CastShadows := true

        Entity.AddEntities of array:
            ShroomEntity_Blue

    ########################################################
    # During Round Warmup add a new entity 
    # with components to scene.
    ########################################################
    OnRoundWarmup():void=
        Print("GameEventsTest: OnRoundWarmup")

        ShroomEntity_Pink := entity{}
        ShroomEntity_Pink.AddComponents of array:
            transform_component:
                Entity := ShroomEntity_Pink
                LocalTransform := transform:
                    Translation := vector3:
                        Forward := 900.0
                        Right := 230.0
                        Up := 1.0
            mesh_component:
                Entity := ShroomEntity_Pink
                Mesh := Meshes.Mushroom02_asset
            sphere_light_component:
                Entity := ShroomEntity_Pink
                ColorFilter := NamedColors.Pink
                CastShadows := true

        Entity.AddEntities of array:
            ShroomEntity_Pink

    ########################################################
    # During Round Start add a new entity 
    # with components to scene.
    # This should coincide with a device's OnBegin
    ########################################################
    OnBeginRound():void=
        Print("GameEventsTest: OnBeginRound")

        ShroomEntity_Red := entity{}
        ShroomEntity_Red.AddComponents of array:
            transform_component:
                Entity := ShroomEntity_Red
                LocalTransform := transform:
                    Translation := vector3:
                        Forward := 1000.0
                        Right := 550.0
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

    OnEndRound():void=
        Print("GameEventsTest: OnEndRound")

    OnEndGame():void=
        Print("GameEventsTest: OnEndGame")

    CancelEventSubs<protected>():void =
        for (C : Cancelables) { C.Cancel()}
            Print("Cleanup: Canceled {Cancelables.Length} GameEvents Subscriptions")
