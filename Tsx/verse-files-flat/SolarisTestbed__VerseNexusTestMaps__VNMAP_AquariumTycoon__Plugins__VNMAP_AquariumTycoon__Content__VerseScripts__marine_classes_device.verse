
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.


#############################################################################################
# Enumerators #
#############################################################################################

#Habitat
water_habitat := enum{Saltwater, Freshwater}
habitat := enum{OpenOcean, KelpForest, SaltMarsh, IntertidalZone, DeepSea, Twilight, CoralReef, Mudflat, SeaGrass, Glacial, Mangrove, Lake, River, Wetland, Cave}

#Animal Classes
animal_class<public> := enum{Mammal, Bird, Fish, Reptile, Amphibian, Arthropod, Mollusk}
#Animal Subclasses
mammal_subclass := enum{Whale, Dolphin, Manatee, Seal, SeaLion, Otter}
bird_subclass := enum{Penguin, Puffin, Pelican, Gull, Duck}
fish_subclass := enum{Shark, Ray, Eel, Catfish, Tuna, Seahorse}
reptile_subclass := enum{Turtle, Snake, Iguana}
amphibian_subclass := enum{Frog, Toad, Salamander, Olm}
arthropod_subclass := enum{Crab, Lobster, Isopod, Urchin}
mollusk_subclass := enum{Octopus, Cuttlefish, Snail, Slug, Squid}

#To String function for water enumerator
(Water : water_habitat).WaterHabitatToString() : string =
    case(Water):
        water_habitat.Saltwater => "Saltwater"
        water_habitat.Freshwater => "Freshwater"


#Interface and Absctract Classes
#############################################################################################
# Marine Life Interface #
#############################################################################################
marine_life_interface<public> := interface() :
    Identify() : string

    ObserveHabitat() : void

    ObserveSize() : void


#############################################################################################
# Marine Animal Class #
#############################################################################################
marine_animal := class<abstract> (marine_life_interface):

    #reference to purchase_device
    var AnimalPurchases : purchase_device = purchase_device{}

    AnimalName: string
    AnimalClass : animal_class

    Lifespan : int
    AnimalLength : float
    AnimalWeight : float

    WaterHabitat : water_habitat
    Habitat : habitat

    Price : int
    BestTankSize: int

    AnimalScale : vector3

    var StartTranslation : vector3 = vector3{}
    AmountToMove : float = 100.0

    var PropReference : creative_prop = creative_prop{}

    TriggerDevice()<suspends> : void =
        TaggedTriggers := GetCreativeObjectsWithTag(device_trigger{})
        TriggerIndex := GetRandomInt(0, TaggedTriggers.Length - 1)
        if(RandomTrigger := TaggedTriggers[TriggerIndex], ChosenTrigger := trigger_device[RandomTrigger]):
            Sleep(0.5)
            ChosenTrigger.Trigger()


    SetPurchaseDevice(): void =
        #Find with Verse gameplay Tag
        TaggedPurchaseDevices := GetCreativeObjectsWithTag(purchasing{})
        if(TaggedPurchaseDeviceObj := TaggedPurchaseDevices[0], NewPurchaseDevice := purchase_device[TaggedPurchaseDeviceObj]):
            set AnimalPurchases = NewPurchaseDevice

    SetPropReference(SpawnedProp : creative_prop): void =
        set PropReference = SpawnedProp

    SetStartingPosition(AnimalProp : creative_prop) : void =
        set StartTranslation = AnimalProp.GetTransform().Translation

    ObserveHabitat<override>() : void =
        Print("{AnimalName} lives in the {HabitatToString(Habitat)} habitat, which is a {WaterHabitat.WaterHabitatToString()} environment.")

    ObserveSize<override>() : void =
        Print("{AnimalName} has an average length {AnimalLength} feet, and weight of {AnimalWeight} pounds.")

    Identify<override>() : string = 
        return "{AnimalName} is a(n) {AnimalClassToString(AnimalClass)}"

    Feed(FoodPurchase : purchase) : void =
        AnimalPurchases.PurchaseFood(FoodPurchase, Self)

    FoodLoop(Owner : fort_character)<suspends>: void =
        loop:
            NewPurchase := purchase{
                Buyer := Owner
                Price := 10
            }
            Sleep(1.0)
            Feed(NewPurchase)

    SpinLoop(SpawnedProp : creative_prop)<suspends>: void =
        loop:
            #   Without the Sleep(0.0), destroying this object causes a Runaway Loop and crashes the server: FORT-744225
            Sleep(0.0)
            Transform := SpawnedProp.GetTransform()
            Position := Transform.Translation
            Rotation := Transform.Rotation
            NewRotation := Rotation .ApplyYaw(180.0)
            SpawnedProp.MoveTo(Position, NewRotation, 3.0)

    AnimalClassToString(AnimalClassIn : animal_class) : string =
        case(AnimalClass):
            animal_class.Mammal => "Mammal"
            animal_class.Bird => "Bird"
            animal_class.Fish => "Fish"
            animal_class.Reptile => "Reptile"
            animal_class.Amphibian => "Amphibian"
            animal_class.Arthropod => "Arthropod"
            animal_class.Mollusk => "Mollusk"

    HabitatToString(InHabitat : habitat) : string =
        case(InHabitat):
            habitat.OpenOcean => "Open Ocean"
            habitat.KelpForest => "Kelp Forest"
            habitat.SaltMarsh => "Salt Marsh"
            habitat.IntertidalZone => "Intertidal Zone"
            habitat.DeepSea => "Deep Sea"
            habitat.CoralReef => "Coral Reef"
            habitat.Mudflat => "Mudflat"
            habitat.SeaGrass => "Sea Grass"
            habitat.Twilight => "Twilight"
            habitat.Glacial => "Glacial"
            habitat.Mangrove => "Mangrove"
            habitat.Lake => "Lake"
            habitat.River => "Lake"
            habitat.Wetland => "Wetland"
            habitat.Cave => "Cave"

    
#############################################################################################
# Marine Animal Subclasses #
#############################################################################################

marine_mammal := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Mammal
    MammalSubclass : mammal_subclass

    MinutesHoldingBreath: int

    Identify<override>() : string =
        return "{AnimalName} is a type of {MammalSubclassToString(MammalSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    ObserveBreath() : void =
        Print("{AnimalName} can hold its breath for {MinutesHoldingBreath} minutes!")

    MammalSubclassToString(Mammal : mammal_subclass) : string =
        case(Mammal):
            mammal_subclass.Whale => "Whale"
            mammal_subclass.Dolphin => "Dolphin"
            mammal_subclass.Manatee => "Manatee"
            mammal_subclass.Seal => "Seal"
            mammal_subclass.SeaLion => "Sea Lion"
            mammal_subclass.Otter => "Otter"


marine_bird := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Bird
    BirdSubclass : bird_subclass

    Wingspan : int

    Identify<override>() : string =
        return "{AnimalName} is a type of {BirdSubclassToString(BirdSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    BirdSubclassToString(Bird :bird_subclass) : string =
        case(Bird):
            bird_subclass.Penguin => "Penguin"
            bird_subclass.Puffin => "Puffin"
            bird_subclass.Pelican => "Pelican"
            bird_subclass.Gull => "Gull"
            bird_subclass.Duck => "Duck"


marine_fish := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Fish
    FishSubclass : fish_subclass

    Identify<override>() : string =
        return "{AnimalName} is a type of {FishSubclassToString(FishSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    FishSubclassToString(Fish : fish_subclass) : string =
        case(Fish):
            fish_subclass.Shark => "Shark"
            fish_subclass.Ray => "Ray"
            fish_subclass.Eel => "Eel"
            fish_subclass.Catfish => "Catfish"
            fish_subclass.Tuna => "Tuna"
            fish_subclass.Seahorse => "Seahorse"

marine_reptile := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Reptile
    ReptileSubclass : reptile_subclass

    MinutesHoldingBreath: int

    Identify<override>() : string =
        return "{AnimalName} is a type of {ReptileSubclassToString(ReptileSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    ReptileSubclassToString(Reptile : reptile_subclass) : string =
        case(Reptile):
            reptile_subclass.Turtle => "Turtle"
            reptile_subclass.Snake => "Snake"
            reptile_subclass.Iguana => "Iguana"

marine_amphibian := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Amphibian
    AmphibianSubclass : amphibian_subclass

    Identify<override>() : string =
        return "{AnimalName} is a type of {AmphibianSubclassToString(AmphibianSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    AmphibianSubclassToString(Amphibian : amphibian_subclass) : string =
        case (Amphibian):
            amphibian_subclass.Frog => "Frog"
            amphibian_subclass.Toad => "Toad"
            amphibian_subclass.Salamander => "Salamander"
            amphibian_subclass.Olm => "Olm"



marine_arthropod := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Arthropod
    ArthropodSubclass : arthropod_subclass

    Identify<override>() : string =
        return "{AnimalName} is a type of {ArthropodSubclassToString(ArthropodSubclass)}, classified as a {AnimalClassToString(AnimalClass)}"

    ArthropodSubclassToString(Arthropod : arthropod_subclass) : string =
        case(Arthropod):
            arthropod_subclass.Crab => "Crab"
            arthropod_subclass.Lobster => "Lobster"
            arthropod_subclass.Isopod => "Isopod"
            arthropod_subclass.Urchin => "Urchin"

marine_mollusk := class(marine_animal):
    AnimalClass<override> : animal_class = animal_class.Mollusk
    MolluskSubclass : mollusk_subclass

    Limbs : int

    Identify<override>() : string =
        return "{AnimalName} is a type of {MolluskSubclassToString(MolluskSubclass)}, classified as a {AnimalClassToString(AnimalClass)}. It has {Limbs} limbs!"

    MolluskSubclassToString(Mollusk : mollusk_subclass) : string =
        case(Mollusk):
            mollusk_subclass.Octopus => "Octopus"
            mollusk_subclass.Cuttlefish => "Cuttlefish"
            mollusk_subclass.Snail => "Snail"
            mollusk_subclass.Slug => "Slug"
            mollusk_subclass.Squid => "Squid"

# A Verse-authored creative device that can be placed in a level
marine_classes_device := class(creative_device):

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        # TODO: Replace this with your code
        Print("Hello from interfaces and marine classes!")