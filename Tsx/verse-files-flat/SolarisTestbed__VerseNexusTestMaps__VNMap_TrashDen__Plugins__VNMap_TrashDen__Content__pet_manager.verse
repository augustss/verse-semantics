using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }

# //////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////
# Creating a player class to house a multiplayer friendly pet for all players that will respawn when it dies.

trash_player:= class():
    Player: player
    PetAsset : creative_prop_asset
    var PetProp : creative_prop = creative_prop {}

    Init():void=
        if:
            Agent:= agent[Player]
            FortPlayer:= Agent.GetFortCharacter[]
        then:
            Scale := GetRandomFloat(1.0,2.0)
            Tran := transform{Rotation:= FortPlayer.GetViewRotation(),
            Translation:=FortPlayer.GetTransform().Translation + vector3{Y:=-120.0},
            Scale := vector3{X:=Scale,Y:=Scale, Z:= Scale}}
            if (Prop := SpawnProp(PetAsset, Tran)(0)?):
                set PetProp = Prop
                Print ("Successfully init player Pet")
                spawn {MovePet(FortPlayer)}

    Dispose():void=
        PetProp.Dispose()

    MovePet<private>(FortCharacter : fort_character)<suspends>: void=
        loop:
            # Breaking if the pet is not valid but restarting another prop pet
            if (not PetProp.IsValid[]):
                Init()
                break

            Scale:= 1.0
            Tran := transform{Rotation:= FortCharacter.GetViewRotation(),Translation:=FortCharacter.GetTransform().Translation + vector3{Y:=-120.0},Scale := vector3{X:=Scale, Y:=Scale, Z:= Scale}}

            PetProp.MoveTo(Tran, 0.5)

# //////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////
# Using tags to track all of the spawners in the game for Initialization
spawner := class(tag){}
# //////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////
# Pet_Manager that controls the actual spawning of the pets
pet_manager := class(creative_device):
    # Array of creativepropassets that we can choose from to be spawned for each player
    @editable
    PetAssets : []creative_prop_asset = array{}

    # Store the players coming in and out of the game
    var trash_players : [player]trash_player = map{}

    # Trash Prop companion variables
    @editable
    TrashCharProp : creative_prop = creative_prop{}
    @editable
    RunningSequence : cinematic_sequence_device = cinematic_sequence_device{}
    @editable
    IdleSequence : cinematic_sequence_device = cinematic_sequence_device{}
    @editable
    DistanceAllowedNearPlayer : float = 400.0
    @editable
    var PropSpeed : float = 300.0
    var IsAlreadyPlayingRunning : logic = false
    var IsAlreadyPlayingIdle : logic = false
    var IsMeshSet : logic = false
    var IsMaterialSet : logic = false
    # //////////////////////////////////////////////////////////
    # //////////////////////////////////////////////////////////
    # Very Basic Trash Prop companion following script for the first player only

    FollowPlayer (Agent : agent):void=
        if(FortCharacter := Agent.GetFortCharacter[]):
            RunningSequence.Play()
            set IsAlreadyPlayingRunning = true
            spawn:
                PropFollowsPlayer(FortCharacter, TrashCharProp)

    PropFollowsPlayer (FortniteCharacter : fort_character, CreativeProp : creative_prop)<suspends>:void=
        Print ("PropFollowsPlayer method called")
        loop:
            Sleep (0.2)
            PlayerPos := FortniteCharacter.GetTransform().Translation
            PropPos := CreativeProp.GetTransform().Translation
            PlayerForward := FortniteCharacter.GetViewRotation().GetLocalForward()
            DistanceFromPlayer := Distance(PlayerPos, PropPos)

            DirectionPropToPlayer := PlayerPos - PropPos
            Angle := ArcTan(DirectionPropToPlayer.X, DirectionPropToPlayer.Y) - 3.14 / 2.0
            NewRotation := MakeRotation (vector3{X := 0.0, Y := 0.0, Z := -1.0}, Angle)
            NoJumpPlayerPos := vector3{X := PlayerPos.X + PlayerForward.X * DistanceAllowedNearPlayer * 0.75, Y := PlayerPos.Y + PlayerForward.Y * DistanceAllowedNearPlayer * 0.75, Z := 80.0}


            # Checking if the Distance from the prop and player is greater than the distance allowed
            if (DistanceFromPlayer > DistanceAllowedNearPlayer):

                Time := DistanceFromPlayer / PropSpeed
                CreativeProp.MoveTo(PlayerPos + PlayerForward * DistanceAllowedNearPlayer, NewRotation, Time)
                Print ("Prop is moving to the player POS")
                # Running animation logic with bool
                if (not IsAlreadyPlayingRunning?):
                    IdleSequence.Stop()
                    RunningSequence.Play()
                    set IsAlreadyPlayingRunning = true
                    set IsAlreadyPlayingIdle = false

            else:
                # Running idle animation logic with bool
                if (not IsAlreadyPlayingIdle?):
                    RunningSequence.Stop()
                    IdleSequence.Play()
                    set IsAlreadyPlayingIdle = true
                    set IsAlreadyPlayingRunning = false
    # //////////////////////////////////////////////////////////
    # //////////////////////////////////////////////////////////
    # Giving players access to a Pet

    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin pet_manager======")
        Players:[]player = Self.GetPlayspace().GetPlayers()
            for(TestPlayer : Players, PlayerAgent: agent = agent(TestPlayer)):
                FollowPlayer(TestPlayer)
        InitSpawners()
        Sleep(2.0)
        SetPetMesh()
        SetPetMaterial()
    OnEnd<override>():void=
        Print ("======Nexus OnEnd pet_manager======")

    # //////////////////////////////////////////////////////////
    # //////////////////////////////////////////////////////////
    # Randomly changing the TrashCharProp's mesh using the SetMesh function based on a boolean 
    SetPetMesh()<suspends>:void=
        Print("Pet Set Mesh Loop activated")
        loop:
            Sleep(1.0)
            if (not IsMeshSet?):
                TrashCharProp.SetMesh(Meshes.Galaxy_Sphere)
                set IsMeshSet = true
                Print ("Setting a new pet mesh")
            else:
                if ( IsMeshSet?):
                    TrashCharProp.SetMesh(Meshes.Galaxy_Box)
                    set IsMeshSet = false
                    Print ("Setting the original pet mesh")
    SetPetMaterial()<suspends>:void=
        Print("Pet Set Material Loop activated")
        loop:
            Sleep(1.5)
            if (not IsMaterialSet?):
                TrashCharProp.SetMaterial(Materials.Mat_Blue)
                set IsMaterialSet = true
            else:
                if (IsMaterialSet?):
                    TrashCharProp.SetMaterial(Materials.Mat_Red)
                    set IsMaterialSet = false
    # Intializing the player spawnpads with the tags
    InitSpawners():void=
        Spawners := GetCreativeObjectsWithTag(spawner{})
        for (Obj : Spawners):
            if (Spawner := player_spawner_device[Obj]):
                Spawner.SpawnedEvent.Subscribe(OnPlayerSpawned)
                Print ("Successfully init player spawners")
    OnPlayerSpawned(Agent: agent):void=
        if (Player:= player[Agent]):
            InitPlayer(Player)
    # Initializing the player with a random Pet
    InitPlayer(Player: player):void =
        Rand := GetRandomInt(0, PetAssets.Length - 1)
        if:
            PetAsset := PetAssets[Rand]
            Agent := agent[Player]
            Fort := Agent.GetFortCharacter[]
        then:
            if (Existing := trash_players[Player]){}
            else:
                CustomPlayer := trash_player{Player := Player, PetAsset := PetAsset}
                CustomPlayer.Init()
                if (set trash_players[Player] = CustomPlayer) {}
        