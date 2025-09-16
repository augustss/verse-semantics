using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

aquariumtank := class(tag){}


#Structs

tank := struct:
    Length : float = 50.0
    Width : float = 50.0
    Height : float = 50.0
    Name : string = "Default Tank"


#Create an Aquarium Tank Class
aquarium_tank := class<unique>:
    AquariumTank : tank
    Volume : float

    var TankPopulation : []marine_animal = array{}

    Water : water_habitat

    AddToTank(Animal : marine_animal)<transacts> : void =
        set TankPopulation += array{Animal}
        #Print("New Tank Population = {TankPopulation.Length}")

    RemoveFromTank(Index : int)<transacts> : void =
        NewPopulation : []marine_animal = array{}
        if(TankPopulation.Length < 2):
            #There is currently only one value here. Since it's being removed, the tank's population can be reset entirely.
            set TankPopulation = NewPopulation


        
BuildAquarium<constructor>(NewTank : tank, WaterType : water_habitat) := aquarium_tank:
    AquariumTank := NewTank
    Water := WaterType
    Volume := NewTank.Length * NewTank.Width * NewTank.Height



# A Verse-authored creative device that can be placed in a level
tank_builder_device := class(nexus_test_device):
    
    #Create a map for each tank and its vector 3 location
    var TankMap : [aquarium_tank]vector3 = map{}

    #Switch
    @editable
    WaterSwitch : switch_device = switch_device{}

    #Trigger
    @editable
    CountTrigger : trigger_device = trigger_device{}

    #BP Props for Tanks
    @editable
    DefaultTankAsset:creative_prop_asset = DefaultCreativePropAsset


    #For switching water
    var ChosenWater : water_habitat = water_habitat.Saltwater

    @editable
    var PropSpawnLoc : vector3 = vector3{X:=0.0, Y:=0.0, Z:=0.0}

    var AquariumTanks : []aquarium_tank = array{}

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Create Aquarium Tank Class Instance")
    #     MakeNexusTest("Add Class Instance to Array")
    #     MakeNexusTest("Spawn Custom Prop")
    #     MakeNexusTest("Create Prop from Spawned Optional Prop")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - tank_builder_device")

        (super:)OnBegin() # * All devices will need this line

        # SetupTest("Create Aquarium Tank Class Instance", false)
        # SetupTest("Add Class Instance to Array", false)
        # SetupTest("Spawn Custom Prop", false)
        # SetupTest("Create Prop from Spawned Optional Prop", false)

        # CompleteTest("Create Aquarium Tank Class Instance", "Create Aquarium Test completed in OnBegin", false)
        # CompleteTest("Add Class Instance to Array", "Add Class to Array Test completed in OnBegin", false)
        # CompleteTest("Spawn Custom Prop", "Custom Prop Spawn Test completed in OnBegin", false)
        # CompleteTest("Create Prop from Spawned Optional Prop", "Optional Fortnite Prop Spawn Test completed in OnBegin", false)

        WaterSwitch.TurnedOffEvent.Subscribe(ChooseSaltwater)
        WaterSwitch.TurnedOnEvent.Subscribe(ChooseFreshwater)
        CountTrigger.TriggeredEvent.Subscribe(CountTanks)

        WaterLoop()

        

    OnEnd<override>():void =
        Print("Nexus OnEnd - tank_builder_device")
        (super:)OnEnd() # * All devices will need this line


    BuildTank<public>(InTank : tank, InWater : water_habitat) : void =
        #Create class instance with constructor
        MyTank := BuildAquarium(InTank, InWater)
        AddPass("Create Aquarium Tank Class Instance", "New Tank object created") # Add an if statement here to clarify? How can we push something like this

        #Add class instance to array
        set AquariumTanks += array{MyTank}
        AddPass("Add Class Instance to Array", "New Tank added to Aquarium Tanks") # Add an if statement here to clarify? How can we push something like this
        
        #Spawn the BP Prop into the level
        SpawnTankLocation := PropSpawnLoc

        if(set TankMap[MyTank] = SpawnTankLocation):
            #Print("Set tank location to map")

        SpawnedTank : tuple(?creative_prop, spawn_prop_result)= SpawnProp(DefaultTankAsset, SpawnTankLocation, IdentityRotation())

        defer:
            # Make it so that tanks spawn in multiple directions
            MoveAmount : float = 1000.0 
            LocRNG : int = GetRandomInt(1,2)
            case(LocRNG):
                1 =>
                    set PropSpawnLoc.X = SpawnTankLocation.X - MoveAmount
                2 =>
                    set PropSpawnLoc.Y = SpawnTankLocation.Y - MoveAmount
                _=>
                    Print("Location RNG spawned a {LocRNG}")

        if(SpawnedTank(0)?):
            AddPass("Spawn Custom Prop", "Custom Tank Prop was spawned successfully")
            if(TankProp : creative_prop = SpawnedTank(0)?):
                AddPass("Create Prop from Spawned Optional Prop", "Created prop based off of spawned optional prop")
                case(InWater):
                    water_habitat.Saltwater => 
                        TankProp.SetMaterial(Materials.MAT_Inst_Saltwater)
                    water_habitat.Freshwater => 
                        TankProp.SetMaterial(Materials.MAT_Inst_Freshwater)

                case(InTank.Name):
                    "Medium Tank" =>
                        TankProp.SetMesh(Meshes.MediumTank)
                    "Large Tank" =>
                        TankProp.SetMesh(Meshes.LargeTank)
                    "Maximum Tank" => 
                        TankProp.SetMesh(Meshes.MaxTank)
                    _=>
                        #Print("Default Tank")  
            else:
                #Print("NexusError - Failed to create prop based off of spawned optional prop | Occurs in BuildTank()")
                AddFail("Create Prop from Spawned Optional Prop", "Failed to create prop based off of spawned optional prop in BuildTank()")
        else:
            #Print("NexusError - Failed to Spawn Tank | Occurs in BuildTank()")
            AddFail("Spawn Custom Prop", "Custom Tank Prop was failed to spawn in BuildTank()")
            return

    WaterLoop()<suspends>: void =
        loop:
            Sleep(2.0)
            case(ChosenWater):
                water_habitat.Saltwater =>
                    set ChosenWater = water_habitat.Freshwater
                water_habitat.Freshwater =>
                    set ChosenWater = water_habitat.Saltwater


    ChooseSaltwater(InAgent : agent) : void =
        set ChosenWater = water_habitat.Saltwater
        Print("Building Saltwater tanks...")

    ChooseFreshwater(InAgent : agent) : void =
        set ChosenWater = water_habitat.Freshwater
        Print("Building Freshwater tanks...")


    GetTankCount(): int =
        TaggedTanks := GetCreativeObjectsWithTag(aquariumtank{})
        var TankCount : int = 0
        for (Tank : TaggedTanks) :
            set TankCount += 1
        return TankCount


    CountTanks(InAgent : ?agent) : void =
        TaggedTanks := GetCreativeObjectsWithTag(aquariumtank{})
        var TankCount : int = 0
        for (Tank : TaggedTanks) :
            set TankCount += 1
        Print("The Aquarium currently has {TankCount} tank(s)")

        set TankCount = 0
        for (Tank : AquariumTanks) :
            set TankCount += 1
            #Print("Tank {TankCount} has a Volume of {Tank.Volume}")


        

    
