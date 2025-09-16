using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Game }

tycoon_manager := class(creative_device):

    StringToMessage<localizes>(Value : string)<computes> : message = "{Value}"

    var PlayersMap: [player]tycoon_player = map {}

    @editable
    MessageDevice : hud_message_device = hud_message_device {}

    @editable
    ResourceTimer : timer_device = timer_device {}

    @editable
    PassiveStatTimer : timer_device = timer_device {}

    @editable
    GoldGranter : item_granter_device = item_granter_device {}


    var ExpAmountMin : float = 500.0
    var ExpAmountMax : float = 1000.0
    var ExpMultiplier : float = 3.0

    PassiveMoneyAmount : int = 100
    GoldCostAmount : int = 300

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - tycoon_manager")

        ResourceTimer.SuccessEvent.Subscribe(PassiveIncome)
        PassiveStatTimer.SuccessEvent.Subscribe(PassiveStatGain)
        SetMessageText()
        InitSpawners()
        
    OnEnd<override>():void=
        Print ("Nexus OnEnd - tycoon_manager")

# Calls various functions from the hud_message_device API
    SetMessageText():void=

        MessageDevice.SetText(StringToMessage("Let's make that money!"))
        MessageDevice.Show()
        MessageDevice.SetDisplayTime(5.0)
# Initializing our tagged spawners with OnPlayerSpawned
    InitSpawners():void=

        Spawners := GetCreativeObjectsWithTag(spawner{})
        for (Obj : Spawners):
            if (Spawner := player_spawner_device[Obj]):
                Spawner.SpawnedEvent.Subscribe(OnPlayerSpawned)
                
# This actually starts our passive stat gain logic and allows us to pass in the variables for our tycoon_player
    OnPlayerSpawned(Agent: agent):void=
        if:
            PlayerObj := player[Agent]

        then:

            if(PlayerExists:= PlayersMap[PlayerObj]):

            else:

                if:

                    FortChar := Agent.GetFortCharacter[]

                then:

                    TycoonPlayer: tycoon_player = tycoon_player {MyAgent := Agent, Strength:= 0.0, Agility:= 0.0, Mining:= 0.0, Fishing:= 0.0}
                    option:
                        set PlayersMap[PlayerObj] = TycoonPlayer
                        
                    TycoonPlayer.InitiateUI()
                    PassiveGold(Agent)
        else:
            Print ("NexusError -  tycoon_manager.OnPlayerSpawned(Agent:agent) | PlayerObj not successfully obtained from the Agent")

# Function that provide Passive Money for all players within the PlayersMap
    PassiveIncome(Agent: ?agent):void=
        if (CastAgent := agent[Agent?]):
            for (TycoonPlayer : PlayersMap):
                TycoonPlayer.AddMoney(PassiveMoneyAmount)
# Function that provide Passive Gold resource for all players within the PlayersMap
    PassiveGold(Agent: agent):void=
        
        for (TycoonPlayer : PlayersMap):
            GoldGranter.GrantItem(Agent) 
# Generates a random EXP Amount for our PassiveStatGain
    GenerateExpAmount():float=

        NewExpAmount := GetRandomFloat(ExpAmountMin, ExpAmountMax)
        FinalExpAmount := NewExpAmount * ExpMultiplier
        
        return FinalExpAmount
# Function that provides a Passive Exp gain for all players within the PlayersMap
    PassiveStatGain(Agent: ?agent):void=

        for (TycoonPlayer : PlayersMap):
            TycoonPlayer.IncreaseExperience(GenerateExpAmount())






            
# Struct that represents very large numbers with a value and an exponent
large_number := struct<concrete>:
    @editable
    Amount : float = 0.0
    @editable
    Exponent : int = 0

# Multiplies a large_number with a float
LargeNumber_Multiply(Number1 : large_number, Number2 : large_number):large_number =
    NewAmount := Number1.Amount * Number2.Amount
    NewExponent := Number1.Exponent + Number2.Exponent

    LargeNumber_Convert(large_number{Amount := NewAmount, Exponent := NewExponent})

# Takes a large_number, and converts it to ensure a single-digit whole-number component
LargeNumber_Convert(Number : large_number):large_number=
    var NewExponent : int = Number.Exponent
    var NewAmount : float = Number.Amount

    if (NewAmount <= 0.0):
        return NewResult := large_number{}
    
    loop:
        if (NewAmount < 10.0):
            break
        set NewAmount *= 0.1
        set NewExponent += 1

    loop:
        if (NewAmount >= 1.0):
            break
        set NewAmount *=10.0
        set NewExponent -= 1

    Result := large_number:
        Amount := NewAmount
        Exponent := NewExponent



