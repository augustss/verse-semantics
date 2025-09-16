
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/UI }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

#This converts string into a message in order to pass as an argument to the SetText function
StringToMessage<localizes>(InString:string) : message = "{InString}"

# A Verse-authored creative device that can be placed in a level
hud_device := class(nexus_test_device):

    @editable
    MyTanks : tank_builder_device = tank_builder_device{}

    SmallAquarium<localizes> : message = "Your humble Aquarium houses less than 50 Animals!"
    MedAqauarium<localizes> : message = "Your well-kept Aquarium houses between 50 and 100 Animals!"
    LargeAquarium<localizes> : message = "Your extravagant Aquarium houses over 100 animals!"

    var UIPerPlayer : [player]?text_block = map{}

    @editable
    Spawners : []player_spawner_device = array{}

    @editable
    AnimalDisplayBoard : billboard_device = billboard_device{}


    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - hud_device")
        for(playerspawn : Spawners):
            playerspawn.SpawnedEvent.Subscribe(OnPlayerSpawn)
        spawn{BillboardLoop()}

    OnEnd<override>():void =
        Print("Nexus OnEnd - hud_device")
        (super:)OnEnd() # * All devices will need this line

    OnPlayerSpawn(InAgent : agent): void =
        spawn{ WidgetLoop(InAgent) }

    WidgetLoop(Agent : agent)<suspends> : void =
        loop:
            Sleep(5.0)
            ShowWidget(Agent)
            Sleep(2.0)
            ShowWidget(Agent)

    CountAnimals() : int =
        var AnimalsCounted : int = 0
        for (Tank : MyTanks.AquariumTanks):
            if (Tank.TankPopulation.Length > 0):
                set AnimalsCounted += 1
        return AnimalsCounted

    BillboardLoop()<suspends> : void =
        loop:
            Sleep(0.0)
            DisplayAnimalsOnBillboard()


    DisplayAnimalsOnBillboard() : void =
        var BillboardAnimalString : string = ""
        for (Tank : MyTanks.AquariumTanks):
            # Put a SortBy here, print all of the animals in some order
            # Can I check for the first letter and assign a value to sort by? hmm...
            if (TankAnimal := Tank.TankPopulation[0]):
                set BillboardAnimalString += "{TankAnimal.AnimalName}, "
        AnimalDisplayBoard.SetText(StringToMessage(BillboardAnimalString))
                


    ShowWidget(Agent : agent) : void =
        
        NumberOfAnimals := CountAnimals()

        if(NumberOfAnimals < 50):
            if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):

                if(MyUI := UIPerPlayer[InPlayer]?):
                    PlayerUI.RemoveWidget(MyUI)
                    if (set UIPerPlayer[InPlayer] = false ) {}
                else:
                    NewUI := text_block{DefaultText := SmallAquarium}
                    PlayerUI.AddWidget(NewUI)
                    if (set UIPerPlayer[InPlayer] = option{NewUI}) {}

        else if(NumberOfAnimals < 100):
            if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):

                if(MyUI := UIPerPlayer[InPlayer]?):
                    PlayerUI.RemoveWidget(MyUI)
                    if (set UIPerPlayer[InPlayer] = false ) {}
                else:
                    NewUI := text_block{DefaultText := MedAqauarium}
                    PlayerUI.AddWidget(NewUI)
                    if (set UIPerPlayer[InPlayer] = option{NewUI}) {}
        else:
            if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):

                if(MyUI := UIPerPlayer[InPlayer]?):
                    PlayerUI.RemoveWidget(MyUI)
                    if (set UIPerPlayer[InPlayer] = false ) {}
                else:
                    NewUI := text_block{DefaultText := LargeAquarium}
                    PlayerUI.AddWidget(NewUI)
                    if (set UIPerPlayer[InPlayer] = option{NewUI}) {}

    

