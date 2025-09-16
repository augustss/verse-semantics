# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Your new component
mesh_blinker_component := class<final_super>(component):

    # # A custom variable you can expose to the editor
    # @editable
    # var MyCustomInt<public>:int = 10

    @editable
    BlinkRate:float = 1.0

    var MeshComponent:?mesh_component = false
    
    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()

        if (not Entity.GetComponent[replication_component]):
            Print("Adding Blinker Replication")
            Entity.AddComponents(array{component(replication_component{Entity := Entity})})

        set MeshComponent = option{Entity.GetComponent[mesh_component]}
        if (MeshComp := MeshComponent?):
        else:
            Print("Adding Mesh")
            Entity.AddComponents(array{component(Meshes.SM_Armor_Lvl01{Entity := Entity})})
            set MeshComponent = option{Entity.GetComponent[mesh_component]}

        if (MeshComp := MeshComponent?):
            Print("Enabling Mesh")
            MeshComp.Enable()

    OnSimulate<override>()<suspends>:void=
        loop:
            Sleep(BlinkRate)
            if:
                MeshComp := MeshComponent?
            then:
                if(MeshComp.IsEnabled[]):
                    MeshComp.Disable()
                else:
                    MeshComp.Enable()

    OnEndSimulation<override>():void=
        if (MeshComp := MeshComponent?):
            Print("Disabling Mesh")
            MeshComp.Disable()
        (super:)OnEndSimulation()

