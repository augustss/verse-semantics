using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }

physictreetags:= class (tag){}

ReleaseTheLogs():void=
    Print ("Release The LOGS")
    PhysicTrees:= GetCreativeObjectsWithTag(physictreetags{})

        for (AllPhysicTrees: PhysicTrees, aphysictree:= physics_tree_device[AllPhysicTrees]):
            aphysictree.ReleaseLog()
            # aphysictree.DestroyStump()

trash_tag_manager := class(creative_device):


    # Multi-dimensional array aka an array within a array within an array. An optional creative_prop
    # Trash variables
    var TrashGrid : [][][]?creative_prop = array {array{}}
    @editable
    var TrashAsset : creative_prop_asset := DefaultCreativePropAsset
    @editable
    var TrashToSpawn : creative_prop_asset := DefaultCreativePropAsset
    @editable
    physictree : physics_tree_device = physics_tree_device{}
    @editable
    physicboulder : physics_boulder_device = physics_boulder_device {}



    # Creating a prop at run time. With spacers between each prop spawned. If no prop return false.
    CreateTrashForIndex(XIdx : int, YIdx: int, ZIdx : int) : ?creative_prop=
        Spacer := 600.0
        if (XPos := float[XIdx * Spacer], YPos := float[YIdx * Spacer], ZPos := float[ZIdx * 3.0]):
            SpawnPropTuple := SpawnProp(TrashAsset, vector3{X := XPos, Y := YPos, Z := ZPos}, IdentityRotation())
            return SpawnPropTuple(0)
        else:
            return false
            
    GenerateTrash():void=
        Print ("Generating Trash")
        set TrashGrid = 
            for (Row := 0..10):
                for (Column := 0..10):
                    for (Height := 0..10):
                        CreateTrashForIndex(Row, Column, Height)

                        

    RemoveTrash():void=
        # Iterating through the MD array and Disposing the Creative Props
        if (NumberOfColumns : int = TrashGrid[0].Length):
            for (Row := 0..TrashGrid.Length - 1, Column := 0..NumberOfColumns, Height := 0..TrashGrid.Length):
                if (Trash := TrashGrid[Row][Column][Height]?):
                    Rand := GetRandomInt(0,1)
                    if (Rand = 0):
                        Trash.Dispose()
    RemoveTrashLoop()<suspends>:void=
        Print ("Disposing Trash")
        loop:
            Sleep(2.0)
            Print ("Removing Trash Loop starts NOW")
            RemoveTrash()
            return

    CreateOtherTrash()<suspends>:void=
        Print ("Creating a large bundle of trash")
        SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0}, IdentityRotation())
        for (Y := 1..10):
            for (Z := 1..10):
                SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 300.0}, IdentityRotation())
                Sleep(1.5)
                    for (A := 1..5):
                        for (B := 1..5):
                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 400.0}, IdentityRotation())
                            Sleep(1.5)
                            for (C := 1..10):
                                for (D := 1..10):
                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 500.0}, IdentityRotation())
                                    Sleep(1.5)
                                    for (E := 1..5):
                                        for (F := 1..5):
                                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 600.0}, IdentityRotation())
                                            Sleep(1.5)
                                            for (G := 1..10):
                                                for (H := 1..10):
                                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 700.0}, IdentityRotation())
                                                    Sleep(1.5)
                                                    for (I := 1..5):
                                                        for (J := 1..5):
                                                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 800.0}, IdentityRotation())
                                                            Sleep(1.5)
                                                            for (K := 1..10):
                                                                for (L := 1..5):
                                                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 900.0}, IdentityRotation())
                                                                    Sleep(1.5)
                                                                    for (M := 1..5):
                                                                        for (N := 1..5):
                                                                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1000.0}, IdentityRotation())
                                                                            Sleep(1.5)
                                                                            for (O := 1..5):
                                                                                for (P := 1..10):
                                                                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1100.0}, IdentityRotation())
                                                                                    Sleep(1.5)
                                                                                    for (Q := 1..10):
                                                                                        for (R := 1..5):
                                                                                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1200.0}, IdentityRotation())
                                                                                            Sleep(1.5)
                                                                                            for (S := 1..5):
                                                                                                for (T := 1..5):
                                                                                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1300.0}, IdentityRotation())
                                                                                                    Sleep(1.5)
                                                                                                    for (U := 1..5):
                                                                                                        for (V := 1..5):
                                                                                                            SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1400.0}, IdentityRotation())
                                                                                                            Sleep(1.5)
                                                                                                            for (W := 1..5):
                                                                                                                for (X := 1..5):
                                                                                                                    SpawnProp(TrashToSpawn, vector3{X := 0.0, Y := 0.0, Z := 0.0 + 1500.0}, IdentityRotation())


            
    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin trash_tag_manager======")
        Sleep(0.1)
        physicboulder.ReleaseRollingBoulder()
        Print ("Boulder released")
        Sleep(3.0)
        ReleaseTheLogs()
        physictree.ReleaseLog()
        Print("Log released")
        loop:
            Sleep(0.0)
            GenerateTrash()
            Print("Successfully created Trash")
            Sleep(1.5)
            Print ("Trash is about to remove")
            RemoveTrash()
            Print("Successfully ran 2")
            Sleep(1.0)
            GenerateTrash()
            Print("Successfully ran 3")
            Sleep(1.0)
            RemoveTrash()
            Print("Successfully ran 4")
            Sleep(1.0)
            RemoveTrash()
            Print("Successfully ran 5")
            Sleep(1.0)
            GenerateTrash()
            Print("Successfully ran 6")
            Sleep(1.0)
            RemoveTrash()
            Print("Successfully ran 7")
            Sleep(1.0)
            RemoveTrashLoop()
        CreateOtherTrash()
    OnEnd<override>():void=
        Print ("======Nexus OnEnd trash_tag_manager======")




