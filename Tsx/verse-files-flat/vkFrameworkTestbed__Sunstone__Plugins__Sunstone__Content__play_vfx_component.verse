# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Your new component
play_vfx_component := class<final_super>(component):

    # # A custom variable you can expose to the editor
    # @editable
    # var MyCustomInt<public>:int = 10

    var VFXComponent:?particle_system_component = false

    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()

        if (not Entity.GetComponent[replication_component]):
            Print("Adding VFX Replication")
            Entity.AddComponents(array{component(replication_component{Entity := Entity})})

        set VFXComponent = option{Entity.GetComponent[particle_system_component]}
        if (VFX := VFXComponent?):
        else:
            Print("Adding VFX")
            Entity.AddComponents(array{component(VFXAssets.Niagara.FireTower.NS_FireTower_Core{Entity := Entity})})
            set VFXComponent = option{Entity.GetComponent[particle_system_component]}

        if (VFX := VFXComponent?):
            Print("Activating VFX")
            VFX.Enable()

    # OnSimulate<override>()<suspends>:void=
    #     # TODO: Place simple suspends logic to run for this componente here

    OnEndSimulation<override>():void=
        if (VFX := VFXComponent?):
            Print("Deactivating VFX")
            VFX.Disable()
        (super:)OnEndSimulation()

