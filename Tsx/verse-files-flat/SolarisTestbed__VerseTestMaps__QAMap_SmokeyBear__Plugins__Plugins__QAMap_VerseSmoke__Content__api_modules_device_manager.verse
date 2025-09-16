
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Random }
using { /Fortnite.com/Game }

# The devices in this verse file seek to cover funcitonality from several different modules that are commonly used in development by the community and internally

# This device seeks to cover most of the fort_character API within the Characters module using various methods
fort_character_device_manager := class(creative_device):
    # Array of all players
    var Players : []player = array {}

    # Array of our player_spawner_devices
    @editable
    var Spawners : []player_spawner_device = array {}

    OnBegin<override>()<suspends>:void=
        Print("OnBegin fort_character_device_manager")

        InitSpawners()
        DeterminePlayers()
        LoopPlayers()

    OnEnd<override>():void=
        Print("OnEnd fort_character_device_manager")

    # Getting all the players within the team and setting them to our Players array
    DeterminePlayers():void=
        set Players = GetPlayspace().GetPlayers()

    # Looping through the each individual player within our Players array
    LoopPlayers()<suspends>:void=
        loop:
            Sleep(3.0)
            for (Player : Players, FortCharacter := Player.GetFortCharacter[]):
                if (FortCharacter.IsActive[]):
                    Print("[SUCCESS] IsActive returned true | LoopPlayers()", ?Color:=NamedColors.LightGreen)
                    ToggleVisibility(FortCharacter)
                else:
                    Print ("[FAILURE] IsActive returned false, FortCharacter is not active for some reason | LoopPlayers()", ?Color:=NamedColors.Red)

                    break

    # Ensuring the fort_character is still active and hiding/showing them on loop
    ToggleVisibility(Player : fort_character)<suspends>:void=
        loop:
            Sleep(3.0)
            if (Player.IsActive[]):
                Print("[SUCCESS] IsActive returned true | Calling Hide() | ToggleVisibility()", ?Color:=NamedColors.LightGreen)
                Player.Hide()
                Sleep(3.0)
                Print("[SUCCESS] IsActive returned true | Calling Show() | ToggleVisibility()", ?Color:=NamedColors.LightGreen)
                Player.Show()
            else:
                Print("[FAILURE] IsActive returned false, FortCharacter is not acive for some reason | ToggleVisibility()", ?Color:=NamedColors.Red)
                break

    # Initializing our spawners to check for some api
    InitSpawners():void=
        Print("InitSpawners() called | InitSpawners()")
        for (Spawner : Spawners):
            Spawner.SpawnedEvent.Subscribe(CheckIfVulnerable)
            Spawner.SpawnedEvent.Subscribe(CheckInstigator)

    # Checking if IsVulnerable works for the agent
    CheckIfVulnerable(Agent : agent):void=
        Print("CheckIfVulnerable() called")
        if (FortCharacter:= Agent.GetFortCharacter[]):
            if(FortCharacter.IsVulnerable[]):
                Print("[SUCCESS] The FortCharacter is vulnerable | IsVulnerable returned true | CheckIfVulnerable()")
            else:
                Print("[FAILURE] The FortCharacter is not vulnerable for some reason | IsVulnerable returned false | CheckIfVulnerable()")
        else:
            Print("[FAILURE] We failed to GetFortCharacter from the Agent | CheckIfVulnerable()")

    # Checking if GetInstigatorAgent works from the instigator interface returned
    CheckInstigator(Agent : agent):void=
        Print("CheckInstigator() called")
        ReturnedInstigator:= Agent.GetInstigator()

        if(InstigatorAgent := ReturnedInstigator.GetInstigatorAgent[]):
            Print("[SUCCESS] The InstigatorAgent was obtained | GetInstigatorAgent[] | CheckInstigator()")
        else:
            Print("[FAILURE] The InstigatorAgent was not obtained for some reason | GetInstigatorAgent[] | CheckInstigator()")

            
