using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }

cinematic_manager:= class(creative_device):
    @editable
    LifeTimeSong: radio_device = radio_device {}
    
    @editable
    CineLobby_Sequence: cinematic_sequence_device = cinematic_sequence_device {}

    @editable
    Shrek_Cine: cinematic_sequence_device = cinematic_sequence_device {}

    @editable
    Shrek_1: animated_mesh_device = animated_mesh_device {}
    @editable
    Shrek_2: animated_mesh_device = animated_mesh_device {}
    @editable
    Shrek_3: animated_mesh_device = animated_mesh_device {}
    @editable
    Shrek_4: animated_mesh_device = animated_mesh_device {}
    @editable
    Shrek_5: animated_mesh_device = animated_mesh_device {}




    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin cinematic_manager======")
        Shrek_1.Play()
        Shrek_2.PlayReverse()
        Shrek_3.Play()
        Shrek_4.PlayReverse()
        Shrek_5.Play()
        LifeTimeSong.Play()
        CineLobby_Sequence.Stop() # Not implemented just yet need to make the cinematic sequence / level sequence
        Shrek_Cine.Play()
    
    OnEnd<override>():void=
        Print ("======Nexus OnEnd cinematic_manager======")
        LifeTimeSong.Stop()
        Shrek_Cine.Stop()

