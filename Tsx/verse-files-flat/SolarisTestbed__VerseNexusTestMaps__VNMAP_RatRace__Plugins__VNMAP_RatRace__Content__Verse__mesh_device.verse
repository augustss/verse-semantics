
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
mesh_device := class(creative_device):
    @editable
    TargetProp:creative_prop = creative_prop{}
    @editable
    SetMeshLoopIterations:int = 200

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - mesh_device")
        TargetProp.SetMesh(Meshes.Box_asset)
        # Testing SetMaterial in an async code block
        branch:
            for(X := 0..SetMeshLoopIterations):
                CycleMeshes()
        for(X := 0..SetMeshLoopIterations):
            CycleMeshes()
        Print("Finished looping through meshes {SetMeshLoopIterations} times. Prop should be a sphere.")
    
    # Cycle through setting all meshes
    # This will be so fast you never see any meshes but the last one, unless you add sleeps
    CycleMeshes()<suspends>:void=
        TargetProp.SetMesh(Meshes.Box_asset)
        TargetProp.SetMesh(Meshes.Sphere_asset)

    OnEnd<override>():void=
        Print("Nexus OnEnd - mesh_device")