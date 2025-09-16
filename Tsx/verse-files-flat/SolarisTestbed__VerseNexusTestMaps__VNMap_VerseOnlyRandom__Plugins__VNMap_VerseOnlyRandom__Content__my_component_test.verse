using { /UnrealEngine.com/Temporary/SceneGraph }


using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }


my_component_test := class<direct>(component):


    @editable
    var MyCustomInt<public>:int = 10
  

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()

    OnSimulate<override>()<suspends>:void =
        {}

