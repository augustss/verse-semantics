using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
@experimental
entity_lifetime_record<public> := struct<concrete>:
    @editable
    EntityToSpawn:entity = entity{}
    @editable
    LocalSpawnTransform:?transform = false
    @editable
    SpawnDelay:float = 0.0
    @editable
    DestroyDelay:float = 0.0
    @editable
    Respawn:logic = false

@experimental
lifetime_component<public> := class<final_super>(component):

    @editable
    var EntityLifetimes<public>:[]entity_lifetime_record = array{}

    @editable
    var Parent:entity = entity{}

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()

        if (ParentEntity := Entity.GetParent[]):
            set Parent = ParentEntity

    OnSimulate<override>()<suspends>:void =
        Print("Starting Entity Lifetime Test")
    
        Sleep(0.0)
       
        for (LifetimeRecord : EntityLifetimes):
            spawn{ ProcessLifetimeRecord(LifetimeRecord)}

    ProcessLifetimeRecord(LifetimeRecord:entity_lifetime_record)<suspends>:void=
        if (LifetimeRecord.Respawn?):
            loop:
                LifetimeRecord.EntityToSpawn.RemoveFromParent()
                Sleep(LifetimeRecord.SpawnDelay)
                Parent.AddEntities(array{LifetimeRecord.EntityToSpawn})
                if:
                    SpawnTransform:transform = LifetimeRecord.LocalSpawnTransform?
                    TransformComp := LifetimeRecord.EntityToSpawn.GetComponent[transform_component]
                then:
                    set TransformComp.LocalTransform = SpawnTransform

                if (LifetimeRecord.DestroyDelay > 0.0):
                    Sleep(LifetimeRecord.DestroyDelay)
                    LifetimeRecord.EntityToSpawn.RemoveFromParent()