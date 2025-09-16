using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
using { /Verse.org/SceneGraph/KeyframedMovement }
using { /Verse.org/Simulation }

@experimental
rigid_body_test_component := class<final_super>(component):

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        if (RigidbodyComponent := Entity.GetComponent[rigid_body_component]):
            set RigidbodyComponent.MassInKg = 1.0
            set RigidbodyComponent.GravityGroupIndex = 0
            set RigidbodyComponent.MaxAngularVelocityInRadians = 5.0

    OnEndSimulation<override>():void =
        (super:)OnEndSimulation()
