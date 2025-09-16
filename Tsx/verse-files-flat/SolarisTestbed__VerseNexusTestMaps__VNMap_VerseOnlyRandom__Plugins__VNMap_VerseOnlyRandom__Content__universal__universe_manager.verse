using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Random }

# Global Debug Functions #
min_max_struct := struct:
    Minimum : vector3, Maximum : vector3

universe := class(galaxy):

    @editable
    Galaxies : []galaxy = array {}

### Immediate child of universe ###
galaxy := class(solar_system):
    
    @editable
    SolarSystems : []solar_system = array {}

solar_system := class(celestial_body):
### Immediate child of galaxy ###
star := class(celestial_body):
    
blackhole := class (celestial_body):

    @editable
    CelestialBodies : []celestial_body = array{}

celestial_body := class (planet):
    @editable
    Planets : []planet= array{}

planet := class (continent):
    @editable
    Continents : []continent = array{}
    @editable
    Oceans : []ocean = array{}


### Immediate child of planet ###
ocean := class (lake):
    @editable
    Lakes : []lake = array {}
lake := class(river):
    @editable
    Rivers : []river = array {}

river := class(pond):
    @editable
    Ponds : []pond = array {}
    @editable
    Mammals : []mammal = array {}

#this is working
pond := class(water):
    @editable
    Fishes : []fish = array {}
water := class(object):
    @editable
    WaterType : watertype = watertype.Salt
watertype := enum:
    Salt
    Fresh


####################################################################################################
####################################################################################################
object := class<concrete>():

    var TotalHP<private> : int = 100
    var CurrentHP<private> : int = 100

    var ObjectVisual : creative_prop = creative_prop {}
    var RandomPoint1 : vector3 = vector3{}
    var RandomPoint2 : vector3 = vector3{}


    Cost : int = 0
farm := class (object):

    @editable
    Barns : barn = barn {}

barn := class (object):

    Animals : animal = animal {}


vehicle := class<concrete>(object):
    @editable
    Color : colour = colour.Red
    @editable
    Speed : float = 20.0
    @editable
    Year : int = 2023


automobile := class<concrete>(vehicle):
    @editable
    Wheels : int = 4
    @editable
    Brand : brand = brand.Honda
    @editable
    MPG : float = 33.3
    @editable
    HorsePower : int = 158

truck := class(automobile):
    @editable
    Model : truckmodel = truckmodel.Hybrid

car := class<concrete>(automobile):
    @editable
    Model : model = model.Standard

### automobile enum's ###
colour := enum:
    Red
    Green
    Blue
    Yellow
    Orange
    Purple
    Pink
    White
    Black

brand := enum:
    Honda
    Toyota
    MercedesBenz
    Hyundai
    Chevrolet
    Nissan
    Ford
    Fiat
    Jeep
    Volkswagen
    Volvo
    Jaguar
    Audi
    LandRover
    Lexus
    Porsche
    Bugatti
    Bentley
    RollsRoyce
    BMW
    Tesla

model := enum:
    Sport
    Premium
    Standard
    Electric
    Hybrid

truckmodel := enum:
    Compact
    Fullsize
    Midsize
    Mini
    ElectricHeavyDuty
    Hybrid
    Coupe
    LuxuryHeavyDuty

#################################################
# Lifeform enums
diet := enum:
    Carnivore
    Omnivore
    Herbivore
    FilterFeeding
#################################################
structurename := enum:
    Garage
    Road
    Building
    House
    Parks

architecture := class(object):
    Structure : structurename = structurename.Building

city := class(object):
    @editable
    Architectures : []architecture = array {}
    @editable
    Dealerships : []automobile_dealership = array{}

county := class(object):
    @editable
    Cities : []city = array{}

state := class(object):
    @editable
    Counties : []county = array {}

country := class(object):
    @editable
    States : []state = array{}

continent := class(object):
    @editable
    Countries :[]country = array{}

automobile_dealership := class<concrete>(object):
    @editable
    Cars : []car = array{}
    @editable
    Trucks : []truck = array {}

### Immediate child of object ###
lifeform := class(object):
    @editable
    Name : string = "Default"

### Immediate child of lifeform ###
animal := class<concrete>(lifeform):


    @editable
    AnimalProp : creative_prop = creative_prop {}
    @editable
    AnimalRunAnim : cinematic_sequence_device = cinematic_sequence_device {}
    @editable
    AnimalJumpAnim : cinematic_sequence_device = cinematic_sequence_device {}
    @editable
    AnimalAttackAnim : cinematic_sequence_device = cinematic_sequence_device {}
    @editable 
    DamageVolume : damage_volume_device = damage_volume_device {}
    @editable
    Diet : diet = diet.Omnivore



    MoveToPlayer():void=
        block:

        
    Roam()<suspends>:void=

        # Print("Animal Roam called")

        loop:
            Sleep(3.0)
                if (not ObjectVisual.IsValid[]):
                    Print ("not valid, did a Animal get destroyed somehow? | Roam() if 1")
                    break

                else:
                    if (not ObjectVisual.IsValid[]):
                        Print ("not valid, did a Animal get destroyed somehow? | Roam() if 2")
                        break
                    else:
                        var CurrentLocation : vector3 = ObjectVisual.GetTransform().Translation
                        var RandomXPos : float = GetRandomFloat(RandomPoint1.X, RandomPoint2.X)
                        var RandomYPos : float = GetRandomFloat(RandomPoint1.Y, RandomPoint2.Y)
                        var NewLocation2 : vector3 = vector3 {X:= RandomXPos, Y:= RandomYPos, Z:= CurrentLocation.Z}
                        
                        ObjectVisual.MoveTo(NewLocation2, IdentityRotation(), 3.0)
                        # Print ("Random XPos is: {RandomXPos}")
                        # Print ("Random YPos is: {RandomYPos}")
                        # Print ("MoveTo new location, which is: {NewLocation2}")

    Attack():void=
        block:
        
    Run():void=
        AnimalRunAnim.Play()

    Jump():void=
        AnimalJumpAnim.Play()


mammal := class(animal):

    @editable
    SeaDweller : logic = false

fish := class(animal):
    block:


### Immediate child of object ###
platform := class(object):
    StartingPosition : vector3 = vector3 {}
    EndingPosition : vector3 = vector3 {}

    Move<public>()<suspends>:void=
        loop:
            if (not ObjectVisual.IsValid[])
                {break}
            ObjectVisual.MoveTo(EndingPosition, IdentityRotation(), 5.0)
            if (not ObjectVisual.IsValid[])
                {break}
            ObjectVisual.MoveTo(StartingPosition, IdentityRotation(), 5.0)


universe_manager<public> := class(creative_device):

    # We define and pass in our creative props to what we want here
    @editable 
    FarmAsset : creative_prop_asset = DefaultCreativePropAsset 
    @editable
    BarnAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    AnimalAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    PlatformAsset : creative_prop_asset = DefaultCreativePropAsset

    # Array(s) of our various classes. We will define what the arrays here and define what they can do

    var Farms : []farm = array{}
    var Animals : []animal = array {}
    var Barns : []barn = array {}
    var Cities : []city = array{}
    
    # This should be the only editable array's, everything should come from the Universe
    @editable
    Universes : []universe = array {}

    # Spawn Locations for the farms, wilil replace this to reflect where the Universes get spawned later but same logic
    @editable
    var FarmSpawnPoint1 : vector3 = vector3{}
    @editable
    var FarmSpawnPoint2 : vector3 = vector3{}
    @editable
    var IterationAmount : int = 100
    


    BuyFarmsLoop()<suspends>:void=
        loop:
            Sleep(0.5)
            if (Animals.Length >= IterationAmount, Farms.Length >= IterationAmount, Barns.Length >= IterationAmount):
                Print ("We're done buying farms! Animals array is: {Animals.Length}, Barns array is: {Barns.Length}, Farms array is: {Farms.Length}")
                break

            else:
                PurchaseFarm()

            
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - universe_manager")
        InitFarms()
        Sleep(1.0)
        MoveAllAnimals()
        spawn:
            BuyFarmsLoop()
        

    OnEnd<override>():void=
        Print ("Nexus OnEnd - universe_manager")

    # Initializing the farms because i haven't set up the entire hierarchy to work yet
    InitFarms():void=
        Print("Initializing farms, barns, and animals")

        for (IDX := GetRandomInt(0,1)..GetRandomInt(10,30)):
            var FarmSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var FarmSpawnTrans2 : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var BarnSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var AnimalSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z + 50.0}

            if (SpawnedFarm := SpawnProp(FarmAsset, FarmSpawnTrans, IdentityRotation()) (0)?):
                Farm := farm{ObjectVisual:= SpawnedFarm, RandomPoint1:= FarmSpawnTrans, RandomPoint2:= FarmSpawnTrans2}
                set Farms += array{Farm}
                # Print ("Farms array is {Farms.Length}")
                # Print ("Farm spawned at:{Farm.RandomPoint1} and {Farm.RandomPoint2}")
            else:
                Print ("NexusError - universe_manager.InitFarms() | SpawnProp failed, Farms failed to spawn")
            if (SpawnedBarn := SpawnProp(BarnAsset, FarmSpawnTrans, IdentityRotation()) (0)?):
                Barn := barn{ObjectVisual:= SpawnedBarn, RandomPoint1:= FarmSpawnTrans, RandomPoint2:= BarnSpawnTrans}
                set Barns += array{Barn}
                # Print ("Barns array is {Barns.Length}")
                # Print ("Barn spawned at:{Barn.RandomPoint1} and {Barn.RandomPoint2}")
            else:
                Print ("NexusError - universe_manager.InitFarms() | SpawnProp failed, Barns failed to spawn")
            if (SpawnedAnimal := SpawnProp(AnimalAsset, AnimalSpawnTrans, IdentityRotation()) (0)?):
                Animal := animal{ObjectVisual:= SpawnedAnimal, RandomPoint1:= AnimalSpawnTrans, RandomPoint2:= FarmSpawnTrans}
                set Animals += array{Animal}
                # Print ("Animal spawned at:{Animal.RandomPoint1} and {Animal.RandomPoint2}")
                # Print ("Animals array is {Animals.Length}")
            else:
                Print ("NexusError - universe_manager.InitFarms() | SpawnProp failed, Animals failed to spawn")

    MoveAllAnimals():void=

        for (Animal : Animals):
            # Print ("The animals should be roaming now")
            spawn:
                Animal.Roam()

    PurchaseFarm<public>():void=
        for (IDX := GetRandomInt(0,1)..GetRandomInt(1,3)):
            var FarmSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var FarmSpawnTrans2 : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var BarnSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z}
            var AnimalSpawnTrans : vector3 = vector3 {X:= GetRandomFloat (FarmSpawnPoint1.X, FarmSpawnPoint2.X), Y:= GetRandomFloat (FarmSpawnPoint1.Y, FarmSpawnPoint2.Y), Z:= FarmSpawnPoint2.Z + 50.0}
            
            if (SpawnedFarm := SpawnProp(FarmAsset, FarmSpawnTrans, IdentityRotation()) (0)?):
                Farm := farm{ObjectVisual:= SpawnedFarm, RandomPoint1:= FarmSpawnTrans, RandomPoint2:= FarmSpawnTrans2}
                set Farms += array{Farm}
                Print ("Farms array is {Farms.Length}")
                # Print ("Farm spawned at:{Farm.RandomPoint1} and {Farm.RandomPoint2}")
            else:
                Print ("NexusError - universe_manager.PurchaseFarm() | SpawnProp failed, Farms failed to spawn")
            if (SpawnedBarn := SpawnProp(BarnAsset, FarmSpawnTrans, IdentityRotation()) (0)?):
                Barn := barn{ObjectVisual:= SpawnedBarn, RandomPoint1:= FarmSpawnTrans, RandomPoint2:= BarnSpawnTrans}
                set Barns += array{Barn}
                # Print ("Barns array is {Barns.Length}")
                # Print ("Barn spawned at:{Barn.RandomPoint1} and {Barn.RandomPoint2}")
            else:
                Print ("NexusError - universe_manager.PurchaseFarm() | SpawnProp failed, Barns failed to spawn")
            if (SpawnedAnimal := SpawnProp(AnimalAsset, AnimalSpawnTrans, IdentityRotation()) (0)?):
                Animal := animal{ObjectVisual:= SpawnedAnimal, RandomPoint1:= AnimalSpawnTrans, RandomPoint2:= FarmSpawnTrans}
                set Animals += array{Animal}
                MoveAllAnimals()
                # Print ("Animal spawned at:{Animal.RandomPoint1} and {Animal.RandomPoint2}")
                # Print ("Animals array is {Animals.Length}")
            else:
                Print ("NexusError - universe_manager.PurchaseFarm() | SpawnProp failed, Animals failed to spawn")
