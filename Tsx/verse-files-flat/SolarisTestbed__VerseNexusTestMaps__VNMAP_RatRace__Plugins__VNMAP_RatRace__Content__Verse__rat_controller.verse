using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { /Fortnite.com/Vehicles }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Assets }
using { /Verse.org/Colors }

player_rat_data := class<final><persistable>:
    RatsSpawned:int = 0

    IncrementRatsSpawned():int=
        NewRatsSpawned := RatsSpawned + 1
        return NewRatsSpawned

MakeNewRatData<constructor>(OldData:player_rat_data) := player_rat_data:
    RatsSpawned := OldData.IncrementRatsSpawned()

var PlayerDataMap:weak_map(player, player_data_table) = map{}

rat := class:
    Prop:creative_prop
    Speed:float
    var HomePoint:vector3 = vector3{X:=0.0,Y:=0.0,Z:=0.0}
    var Destination:vector3 = vector3{X:=0.0,Y:=0.0,Z:=0.0}

    GetTransform():transform=
        return Prop.GetTransform()

    GetHomePoint():vector3=
        return HomePoint

    SetHomePoint(Home:vector3):vector3=
        set HomePoint = Home

    GetDestination():vector3=
        return Destination

    SetDestination(Dest:vector3):void=
        set Destination = Dest

NewRat<constructor>(BaseProp:creative_prop) := rat:
    Prop := BaseProp
    Speed := GetRandomFloat(0.05,0.1)

RatPropTag := class(tag){}
RatBodyTag := class(tag){}

##########################################################################
# Future Plans:                                                          #
#                                                                        #
# Make rats into SceneGraph prefabs and spawn in more of them at runtime #
# Apply paramaterized materials to body and change colors while moving   #
# Persistable positions?
##########################################################################
rat_controller := class(creative_device):
    #@editable
    #RatProp:creative_prop = creative_prop {}
    @editable
    VehicleSpawner:vehicle_spawner_sports_car_device = vehicle_spawner_sports_car_device{}
    @editable
    VFXSpawner:vfx_spawner_device = vfx_spawner_device {}
    @editable
    TestProp:creative_prop = creative_prop{}
    
    var RatArray:[]rat = array{}
    DestinationOffset:vector3 = vector3{X:=0.0,Y:=0.0,Z:=200.0}
    DestinationLeeway:float = 60.0
    FrameTime:float = 0.0166666667
    #ParamMaterial:Materials.Material4_material = Materials.Material4_material{}

    var Speed:float = 0.1
    var FirstRun:logic = true
    var RotationDifference:float = 0.0
    var LerpParameter:float = 0.0
    var GoingToDestination:logic = true
    var AttachedVehicle:?fort_vehicle = false

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - rat_controller")
        
        VehicleSpawner.SpawnedEvent.Subscribe(AttachToVehicle)

        RatProps := GetCreativeObjectsWithTag(RatPropTag{})
        RatBodies := GetCreativeObjectsWithTag(RatBodyTag{})

        for(RatPropList:RatProps, Prop := creative_prop[RatPropList]):
            Rat:rat = NewRat(Prop)

            if(NewArray := RatArray.Insert[0, array{Rat}]):
                set RatArray = NewArray

            else:
                Print("NexusError - Failed to insert new element in RatArray in rat_controller.OnBegin()")
        
        Sleep(2.0)

        for(RatBodyList:RatBodies, Body := creative_prop[RatBodyList]):
            Print("RatBody setting material")
            #Body.SetMaterial(ParamMaterial)

        for(Rats:RatArray, Rat := rat[Rats]):
            spawn{HandleMovement(Rat)}

    HandleMovement(Rat:rat)<suspends>:void=
        var SavedRotation:rotation = MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0}, 0.0)

        if(Vehicle := AttachedVehicle?):
            VehicleTransform := Vehicle.GetTransform()
            Rat.SetHomePoint(VehicleTransform.Translation + DestinationOffset)

        var Home:vector3 = Rat.GetHomePoint()
        #Print("Rat Home Point: {Home}")

        MoveToHome := spawn{Rat.Prop.MoveTo(Home, Rat.Prop.GetTransform().Rotation, 0.1)}
        MoveToHome.Await()

        loop:
            ############
            # Rotation #
            ############
            NewRotation:rotation = MakeRandomRotation()

            if(FirstRun = false):
                set SavedRotation = NewRotation
                set FirstRun = false
            
            else:
                set RotationDifference = Distance(SavedRotation, NewRotation)
                #Print("Rotation Difference: {RotationDifference}")
                set SavedRotation = NewRotation
            
            ############
            # Movement #
            ############
            Destination:vector3 = MakeRandomVector() + Home
            Rat.SetDestination(Destination)

            if(Vehicle := AttachedVehicle?):
                VehicleTransform := Vehicle.GetTransform()
                Rat.SetHomePoint(VehicleTransform.Translation + DestinationOffset)
                set Home = Rat.HomePoint

            var IntermediateDestination:vector3 = vector3{}

            loop:
                if(GoingToDestination = false):
                    set IntermediateDestination = Lerp(Rat.Destination, Home, LerpParameter)
    
                else:
                    set IntermediateDestination = Lerp(Home, Rat.Destination, LerpParameter)
                    #set IntermediateDestination = IntermediateDestination + DestinationOffset

                #Print("IntermediateDestination: X = {IntermediateDestination.X}, Y = {IntermediateDestination.Y}, Z = {IntermediateDestination.Z}")
                MoveTask := spawn{Rat.Prop.MoveTo(IntermediateDestination, MakeRandomRotation(), Rat.Speed)}
                set LerpParameter += FrameTime
                #Print("LerpParameter = {LerpParameter}")
                MoveTask.Await()

                #set ParamMaterial.VectorParam = color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
                #set ParamMaterial.ScalarParam = 1.0

                if(GoingToDestination = false):
                    if(IsAlmostEqual[IntermediateDestination, Home, DestinationLeeway]):
                        #Print("Boom!")
                        VFXSpawner.Enable()
                        VFXSpawner.Restart()
                        break
                else:
                    if(IsAlmostEqual[IntermediateDestination, Rat.Destination, DestinationLeeway]):
                        break
            
            #########################
            # Cleanup for next loop #
            #########################
            set LerpParameter = 0.0

            if(GoingToDestination = false):
                set GoingToDestination = true
            else:
                set GoingToDestination = false

    AttachToVehicle(Vehicle:fort_vehicle):void=
        Print("rat_controller: AttachToVehicle")
        set AttachedVehicle = option{Vehicle}
            
    MakeRandomRotation():rotation=
        RandomRotation:rotation = MakeRotation(MakeRandomVector(), GetRandomFloat(0.0,359.0))
        
        return RandomRotation

    MakeRandomVector():vector3=
        RandomVector:vector3 = vector3{X:=GetRandomFloat(-600.0,600.0),Y:=GetRandomFloat(-600.0,600.0),Z:=GetRandomFloat(-200.0,600.0)}
        #Print("MakeRandomVector result: X = {RandomVector.X}, Y = {RandomVector.Y}, Z = {RandomVector.Z}")

        return RandomVector

    OnEnd<override>():void=
        Print("Nexus OnEnd - rat_controller")