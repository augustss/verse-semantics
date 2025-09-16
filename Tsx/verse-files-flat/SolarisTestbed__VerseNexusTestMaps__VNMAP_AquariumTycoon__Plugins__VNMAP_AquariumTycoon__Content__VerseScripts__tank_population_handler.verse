
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Characters }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device

# A Verse-authored creative device that can be placed in a level
tank_population_handler_device := class(nexus_test_device):

    #Verse Device References
    @editable
    MyTanks : tank_builder_device = tank_builder_device{}
    @editable
    MyPurchases : purchase_device = purchase_device{}

    # End Game Reference
    @editable
    MyEndGame : end_game_device = end_game_device{}
    @editable
    EndAnimalCount : int = 200

    #Prop Asset References
    @editable
    DefaultAnimalAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    SharkAnimalAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    SquidAnimalAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    SpawnAnimalButton : button_device = button_device{}

    #Device References
    @editable
    Spawners : []player_spawner_device = array{}

    #Used to reference the tank array in the tank_builder script
    var AnimalIndex : int = 0
    var AnimalPrice : int = 0

    #Scale Vectors
    @editable
    TinySize : vector3 = vector3{X:=0.05, Y:=0.05, Z:=0.05}
    @editable
    SmallSize : vector3 = vector3{X:=0.15, Y:=0.15, Z:=0.15}
    @editable
    LargeSize : vector3 = vector3{X:=2.0, Y:=2.0, Z:=2.0}


    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - tank_population_handler_device")
        SpawnAnimalButton.InteractedWithEvent.Subscribe(OnSpawnButtonInteract)
        for(playerspawn : Spawners):
            playerspawn.SpawnedEvent.Subscribe(OnPlayerSpawnedEvent)

        (super:)OnBegin() # * All devices will need this line


    OnEnd<override>():void =
        Print("Nexus OnEnd - tank_population_handler_device")
        (super:)OnEnd() # * All devices will need this line

    OnSpawnButtonInteract(InAgent : agent): void = 
        if(MyCharacter := InAgent.GetFortCharacter[]):
            SpawnAnimal(MyCharacter)

            
    ################
    #Passive Play Loop #
    ################

    OnPlayerSpawnedEvent(InAgent : agent): void =
        if(MyCharacter := InAgent.GetFortCharacter[]):
            spawn{ SpawnAnimalLoop(MyCharacter) }

    SpawnAnimalLoop(InCharacter : fort_character)<suspends>: void =
        Print("Animal Loop Starting")
        loop:
            Sleep(0.7)
            SpawnAnimal(InCharacter)

    #############################################################################################
    # Spawning the Animal #
    #############################################################################################
        
    SpawnAnimal(InCharacter : fort_character): void =
        if(NewTankRef := MyTanks.AquariumTanks[AnimalIndex]):

            #Create reference for the new animal
            var MaybeAnimal : ?marine_animal = false

            #spawn the Animal at the tank's location
            if(TankLocation := MyTanks.TankMap[MyTanks.AquariumTanks[AnimalIndex]]):
                #spawn a class instance, then use that to decide which prop to use


                var TankSize : int = 0
                var AnimalProp : creative_prop_asset = DefaultAnimalAsset
                var SizeModifier : int = 0
                var IdentificationString : string = ""

                var AnimalTankSize : int = 4

                var NewAnimalScale : vector3 = vector3{X:=0.0, Y:=0.0, Z:=0.0}
                var ScaleChanged : logic = false

                case(NewTankRef.AquariumTank.Name):
                    "Medium Tank" =>
                        set TankSize = 1
                    "Large Tank" =>
                        set TankSize = 2
                    "Maximum Tank" =>      
                        set TankSize = 3
                    _ =>
                        set TankSize = 0

                var TankMatch : logic = false  #If set to true, breaks the following loop

                ################
                # Loop Spawning Animal Class instance until the Animal fits the tank #
                ################
                loop: 
                    AnimalRNG : int = GetRandomInt(1,2)
                    case(AnimalRNG):
                        1 =>
                            NewShark := CreateShark()
                            if(MyTanks.AquariumTanks[AnimalIndex].AddToTank(NewShark)):
                                set AnimalProp = SharkAnimalAsset
                                #Print("Added {NewShark.AnimalName} to tank.")
                            set SizeModifier = GetSizeModifier(NewShark)
                            set AnimalTankSize = NewShark.BestTankSize 
                            set AnimalPrice = NewShark.Price
                            set IdentificationString = NewShark.Identify()
                                
                        2=>
                            NewSquid := CreateSquid()
                            if(MyTanks.AquariumTanks[AnimalIndex].AddToTank(NewSquid)):
                                set AnimalProp = SquidAnimalAsset
                                #Print("Added {NewSquid.AnimalName} to tank.")
                            set SizeModifier = GetSizeModifier(NewSquid)
                            set AnimalTankSize = NewSquid.BestTankSize
                            set AnimalPrice = NewSquid.Price
                            set IdentificationString = NewSquid.Identify()
                        _=>
                            Print("Animal RNG spawned a {AnimalRNG}")
                            set AnimalTankSize = 0

                    if(AnimalTankSize = TankSize):
                            set TankMatch = true

                    #If the animal matches the tank, purchase it and spawn in a creative_prop
                    if(TankMatch = true):
                        AnimalPurchase := purchase{
                            Buyer := InCharacter
                            Price := AnimalPrice
                        }
                        MyPurchases.PurchaseAnimal(AnimalPurchase)
                        #Print(IdentificationString)
                        
                        #Start Feeding Loop
                        if(MyAnimal : marine_animal = MyTanks.AquariumTanks[AnimalIndex].TankPopulation[0]):
                            AddPass("Reference Nested Class Instance", "Referenced marine_animal class within the array of aquarium_tank class")
                            set NewAnimalScale = MyAnimal.AnimalScale
                            set MaybeAnimal = option{MyAnimal}
                            if(NewAnimalScale.X = 0.0):
                                Print("Scale set to 0.0")
                            else:
                                set ScaleChanged = true

                            MyAnimal.SetPurchaseDevice()
                            spawn{MyAnimal.TriggerDevice()}
                            spawn{MyAnimal.FoodLoop(InCharacter)}
                            break
                        else:
                            AddFail("Reference Nested Class Instance", "Unable to reference marine_animal class within AquariumTank array")
                    else:
                        #If the animal doesn't fit in the tank, remove it from the tank's array and loop again

                        #Print("Wrong Size - Tank Size: {TankSize} vs Animal's Tank Size: {AnimalTankSize} | Looping again.")
                        if(MyTanks.AquariumTanks[AnimalIndex].RemoveFromTank(0)):
                        set TankMatch = false


                ################
                # Spawn in the creative_prop for the Animal #
                ################

                SpawnedAnimal := SpawnProp(AnimalProp, TankLocation, IdentityRotation())

                if(SpawnedAnimal(0)?):
                    # Check what kind of prop this is for test results
                    if(AnimalReference : marine_animal = MaybeAnimal?):
                        if(AnimalReference.AnimalClass = animal_class.Mollusk): 
                            AddPass("Spawn Fortnite Prop", "Successfully Spawned Squid Fortnite Prop")
                        else:
                            AddPass("Spawn Custom Animal Prop", "Successfully Spawned Shark Custom Prop")
                    
                    if(var AnimalPropSpawned : creative_prop = SpawnedAnimal(0)?):
                        
                        AnimalTransform := AnimalPropSpawned.GetTransform()

                        AnimalScale := AnimalTransform.Scale
                        AnimalTranslation := AnimalTransform.Translation
                        AnimalRotation := AnimalTransform.Rotation

                        if(ScaleChanged = true):
                            NewAnimalTransform : transform = transform{Scale := NewAnimalScale, Rotation := AnimalRotation, Translation := AnimalTranslation}

                            if(AnimalPropSpawned.TeleportTo[NewAnimalTransform]):
                                AddPass("TeleportTo Transform", "Successfully called TeleportTo on Prop's Transform")
                            else:
                                AddFail("TeleportTo Transform","Failed to call TeleportTo on Prop's Transform")

                        else:
                            Print("Scale was not changed by Animal Class")
                            TinyTransform : transform = transform{Scale := TinySize, Rotation := AnimalRotation, Translation := AnimalTranslation}
                            SmallTransform : transform = transform{Scale := SmallSize, Rotation := AnimalRotation, Translation := AnimalTranslation}
                            LargeTransform : transform = transform{Scale := LargeSize, Rotation := AnimalRotation, Translation := AnimalTranslation}

                            case(SizeModifier):
                                0 =>
                                    if(AnimalPropSpawned.TeleportTo[TinyTransform]):
                                2 =>
                                    if(AnimalPropSpawned.TeleportTo[SmallTransform]):
                                3 =>
                                    if(AnimalPropSpawned.TeleportTo[LargeTransform]):
                                _=>
                                    #Default Size
                        if(MyAnimal : marine_animal = MyTanks.AquariumTanks[AnimalIndex].TankPopulation[0]):
                            MyAnimal.SetPropReference(AnimalPropSpawned)
                            spawn:
                                MyAnimal.SpinLoop(AnimalPropSpawned)
            
                    set AnimalIndex += 1
                    CheckForEndGame(AnimalIndex, InCharacter)
                else:
                    # Check what kind of prop this is for test results
                    if(AnimalReference : marine_animal = MaybeAnimal?):
                        if(AnimalReference.AnimalClass = animal_class.Mollusk): 
                            AddFail("Spawn Fortnite Prop","Failed to spawn Squid Fortnite Prop")
                        else:
                            AddFail("Spawn Custom Animal Prop","Failed to spawn Shark Custom Prop")
        else:
            Print("A Tank for this animal does not exist")

    CheckForEndGame(Animals : int, InCharacter: fort_character) : void =
        if(Animals > EndAnimalCount):
            if(MyAgent := InCharacter.GetAgent[]):
                MyEndGame.Activate(MyAgent)


    GetSizeModifier(InAnimal : marine_animal) : int =
        if(InAnimal.AnimalLength < 0.5):
            return 0
        else if (InAnimal.AnimalLength < 2.0):
            return 2
        else if(InAnimal.AnimalLength > 6.0):
            return 3
        else:
            return 1    



    #############################################################################################
    # Class Instances #
    #############################################################################################
            #Return a random marine_animal object

    CreateShark()<transacts> : marine_animal =

        SharkRNG : int = GetRandomInt(1,3)

        Catshark := shark{
                    AnimalName := "Catshark"
                    Lifespan := 20
                    AnimalLength := 2.5
                    AnimalWeight := 0.75
                    WaterHabitat := water_habitat.Saltwater
                    Habitat := habitat.IntertidalZone
                    Price := 80
                    BestTankSize := 1
                    AnimalScale := vector3{X:=1.75, Y:=1.5, Z:=1.0}
        }

        SpeartoothShark := shark{
            AnimalName := "Speartooth Shark"
            Lifespan := 25
            AnimalLength := 8.5
            AnimalWeight := 110.0
            WaterHabitat := water_habitat.Freshwater
            Habitat := habitat.River
            Price := 350
            BestTankSize := 2
            AnimalScale := vector3{X:=4.5, Y:=3.5, Z:=3.0}
        }

        DwarfLanternshark := shark{
            AnimalName := "Dwarf Lantern Shark"
            Lifespan := 25
            AnimalLength := 0.75
            AnimalWeight := 0.25
            WaterHabitat := water_habitat.Saltwater
            Habitat := habitat.OpenOcean
            Price := 70
            BestTankSize := 0
            AnimalScale := vector3{X:=0.15, Y:=0.15, Z:=0.15}
        }

        case(SharkRNG):
            1 =>
                return Catshark
            2 => 
                return SpeartoothShark
            _ =>
                return DwarfLanternshark



    CreateSquid()<transacts> : marine_animal =

        SquidRNG : int = GetRandomInt(1,3)

        NonSquid := squid{
            AnimalName := "Default Animal : this should not be showing"
            Lifespan := 0
            AnimalLength := 0.0
            AnimalWeight := 0.0
            Limbs := 0
            WaterHabitat := water_habitat.Saltwater
            Habitat := habitat.OpenOcean
            Price := 0
            BestTankSize := 0
            AnimalScale := vector3{X:=0.15, Y:=0.15, Z:=0.15}

        }

        case(SquidRNG):
            1 =>
                FireflySquid := squid{
                    AnimalName := "Firefly Squid"
                    Lifespan := 1
                    AnimalLength := 0.25
                    AnimalWeight := 0.01
                    Limbs := 10
                    WaterHabitat := water_habitat.Saltwater
                    Habitat := habitat.Twilight
                    Price := 70
                    BestTankSize := 0
                    AnimalScale := vector3{X:=0.15, Y:=0.15, Z:=0.15}
                }
                return FireflySquid
            2 =>
                GiantSquid := squid{
                    AnimalName := "Giant Squid"
                    Lifespan := 5
                    AnimalLength := 40.0
                    AnimalWeight := 110.0
                    Limbs := 10
                    WaterHabitat := water_habitat.Saltwater
                    Habitat := habitat.DeepSea
                    Price := 500
                    BestTankSize := 3
                    AnimalScale := vector3{X:=1.5, Y:=1.5, Z:=2.0}
                }
                return GiantSquid

            3 =>
                FlyingSquid := squid{
                    AnimalName := "Flying Squid"
                    Lifespan := 1
                    AnimalLength := 1.0
                    AnimalWeight := 1.1
                    Limbs := 10
                    WaterHabitat := water_habitat.Saltwater
                    Habitat := habitat.OpenOcean
                    Price := 50
                    BestTankSize := 1
                    AnimalScale := vector3{X:=0.25, Y:=0.25, Z:=0.25}
                }
                return FlyingSquid

            _ =>
                Print("Squid RNG returned {SquidRNG}")
                return NonSquid

    CreateTurtle()<transacts>: marine_animal =

        SeaTurtle := turtle{
            AnimalName := "Default Animal : this should not be showing"
            Lifespan := 0
            AnimalLength := 0.0
            AnimalWeight := 0.0
            MinutesHoldingBreath := 420
            WaterHabitat := water_habitat.Saltwater
            Habitat := habitat.OpenOcean
            Price := 0
            BestTankSize := 0
            AnimalScale := vector3{X:=0.15, Y:=0.15, Z:=0.15}

        }


#Animal Classes
shark := class(marine_fish):
    FishSubclass<override> : fish_subclass = fish_subclass.Shark

squid :=class(marine_mollusk):
    MolluskSubclass<override> : mollusk_subclass = mollusk_subclass.Squid

turtle := class(marine_reptile):
    ReptileSubclass<override> : reptile_subclass = reptile_subclass.Turtle