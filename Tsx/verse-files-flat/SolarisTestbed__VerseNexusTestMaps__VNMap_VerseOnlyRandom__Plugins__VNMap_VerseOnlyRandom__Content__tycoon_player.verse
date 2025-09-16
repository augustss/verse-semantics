using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
universal<public>:= module{}


universe_manager_tag<public> := class (tag) {}
# A class for players that has various ui_buttons and tracks money, char level, and future implementation of stats using float values
tycoon_player := class():

    StringToMessage<localizes>(Value : string)<computes> : message = "{Value}"

# universe_manager reference
    var UniverseManagerRef :  []creative_object_interface = array {}
    var UniverseMan : universal.universe_manager = universal.universe_manager {}

    MyAgent : agent
# Stats
    var Strength : float
    var Fishing : float
    var Mining : float
    var Agility : float
# Different children of the widget class for our stat ui representation
    var StrengthExpText : text_block = text_block {}
    var ExpBarStrength : slider_regular = slider_regular 

    {
        DefaultValue := 0.0
        DefaultMinValue := 10000.0
        DefaultMaxValue := 100000.0
        DefaultStepSize := 0.01
    }
# Different children of the widget class for our buttons and money ui representation
    var MakeMoneyButton : button_regular = button_regular {}
    var SpendMoneyButton : button_regular = button_regular {}
    var MoneyText : text_block = text_block {}
    var Money : int = 0
    var CharLevel : int = 0

# Initializing our universe_manager to our player so we can reference a specific function
    OnBegin():void=
        Print ("Nexus OnBegin - tycoon_player")
        set UniverseManagerRef = GetCreativeObjectsWithTag (universe_manager_tag{})
            for (Obj : UniverseManagerRef):
                if (Device := universal.universe_manager[Obj]):
                    set UniverseMan = Device
                else:
                    Print ("Nexus Error - tycoon_player.OnBegin() | The tagged universe_manager device was not passed into the array of UniverseManagerRef ")
    OnEnd():void=
        Print ("Nexus OnEnd - tycoon_player")
# Function to enabling UI Input mode so manually users can press Buttons. Not being used
    TurnOffInputMode(Widget : widget_message): void =
        if (PlayerObj:= player [MyAgent], PlayerUI := GetPlayerUI [PlayerObj]):
            player_ui_slot{InputMode:=ui_input_mode.All}
# Initiating our UI and setting our values, then creating our canvas and passing in those values we've set
    InitiateUI():void =
        set MoneyText = text_block {DefaultText := StringToMessage("$$$:{Money}"), DefaultTextColor := NamedColors.LightGreen, DefaultShadowColor := NamedColors.Gold}
        set StrengthExpText = text_block {DefaultText := StringToMessage("Strength"), DefaultTextColor := NamedColors.Ivory, DefaultShadowColor := NamedColors.LightBlue}
        set MakeMoneyButton = button_regular{DefaultText := StringToMessage("Click me to make that Money!!!")}
        set SpendMoneyButton = button_regular{DefaultText := StringToMessage("Click me to spend that Money!!!")}

        MakeMoneyButton.OnClick().Subscribe(MakeMoneyButtonClick)
        MakeMoneyButton.SetEnabled(true)

        SpendMoneyButton.OnClick().Subscribe(SpendMoneyButtonClick)
        SpendMoneyButton.SetEnabled(true)

        PlayerMoneyWidget : canvas = CreateUserMoneyUI(MoneyText, StrengthExpText, ExpBarStrength, MakeMoneyButton, SpendMoneyButton)
# Getting the UI of the current player and enabling UI Input mode so manually users can press Buttons
        if (PlayerObj:= player [MyAgent], PlayerUI := GetPlayerUI [PlayerObj]):
            PlayerUI.AddWidget(PlayerMoneyWidget, player_ui_slot{InputMode:=ui_input_mode.All})
            Print ("Tycoon Money UI's have been established")

    CreateUserMoneyUI(MoneyTextBlock : text_block, StrengthTextBlock : text_block, StrengthProgressBar : slider_regular, MakeMoney : button_regular, SpendMoney : button_regular): canvas=

        MoneyCanvas : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum:= vector2 {X := 0.8, Y := 0.2}, Maximum := vector2 {X := 0.8, Y := 0.2} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2 {X := 0.5, Y := 0.0}
                    SizeToContent := true
                    ZOrder := 1
                    Widget := overlay:
                        Slots := array:

                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.Black
                                    DefaultOpacity := 0.3
                                    DefaultDesiredSize := vector2 {X := 300.0, Y := 40.0}

                            overlay_slot:
                                Widget := MoneyTextBlock
                canvas_slot:
                    Anchors := anchors{Minimum:= vector2 {X := 0.75, Y := 0.42}, Maximum := vector2 {X := 0.75, Y := 0.42} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2 {X := 0.5, Y := 0.0}
                    SizeToContent := true
                    ZOrder := 1
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := StrengthTextBlock
                canvas_slot:
                    Anchors := anchors{Minimum:= vector2 {X := 0.75, Y := 0.5}, Maximum := vector2 {X := 0.75, Y := 0.5} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2 {X := 0.5, Y := 0.5}
                    SizeToContent := true
                    ZOrder := 0
                    Widget := StrengthProgressBar
                canvas_slot:
                    Anchors := anchors{Minimum:= vector2 {X := 0.20, Y := 0.95}, Maximum := vector2 {X := 0.20, Y := 0.95} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2 {X := 0.5, Y := 0.5}
                    SizeToContent := true
                    ZOrder := 2
                    Widget := MakeMoney
                canvas_slot:
                    Anchors := anchors{Minimum:= vector2 {X := 0.20, Y := 0.80}, Maximum := vector2 {X := 0.20, Y := 0.80} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2 {X := 0.5, Y := 0.5}
                    SizeToContent := true
                    ZOrder := 2
                    Widget := SpendMoney




                                



        return MoneyCanvas
# Returns a random int that is clamped for AddMoney(Amount : int)
    MakeMoneyButtonClick(WidgetMsg : widget_message) : void=
        Print("Make Money Button Clicked successfully")

            SomeInt := GetRandomInt(50,100) * GetRandomInt(50,100)
            SomeBiggerInt := SomeInt * GetRandomInt(20,40) * GetRandomInt(20,40) * GetRandomInt(20,40) * GetRandomInt(20,40)
            AddMoney(Clamp(SomeBiggerInt, 1000000000, 0))

# Returns a random int that is clamped for RemoveMoney(Amount : int)
    SpendMoneyButtonClick(WidgetMsg : widget_message) : void =
        if(Money >= 100000000): 
            Print ("Spend Money Button Clicked successfully")

                SomeInt := GetRandomInt(50,100) * GetRandomInt(50,100)
                SomeBiggerInt := SomeInt * GetRandomInt(20,40) * GetRandomInt(20,40) * GetRandomInt(20,40) * GetRandomInt(20,40)
                RemoveMoney(Clamp(SomeBiggerInt, 100000000, 0))
                UniverseMan.PurchaseFarm()
        else:
            Print ("Not enough money")






# This will give us experience in all of the categories and updating the UI
    IncreaseExperience(Amount : float):void=
        set Strength += Amount
        set Agility += Amount
        set Mining += Amount
        set Fishing += Amount
        UpdateUI()
# Increasing our money from the passed in Amount
    AddMoney(Amount : int):void=
        Clamp(Money, 1000000000000, 0)
        set Money += Amount
        UpdateUI()
        if:
            CheckSaveDataForPlayer[MyAgent]
            _Player := player[MyAgent]
            SourceData := PlayerInfo[_Player]
        then:
            if:
                var UpdatedPlayerData : player_data = SourceData
                set UpdatedPlayerData = SavePlayerInfo(UpdatedPlayerData, Clamp(Money, 1000000000000, 0) + Amount)
                set PlayerInfo[_Player] = player_data:
                    GetPlayerInfo<constructor>(SourceData)
                    CurrentMoney := SourceData.CurrentMoney + Amount


# Decreasing our money from the passed in Amount
    RemoveMoney(Amount : int):void=
        Clamp(Money, 1000000000000, 0)
        set Money -= Amount
        UpdateUI()
        if:
            CheckSaveDataForPlayer[MyAgent]
            _Player := player[MyAgent]
            SourceData := PlayerInfo[_Player]
        then:
            if:
                var UpdatedPlayerData : player_data = SourceData
                set UpdatedPlayerData = SavePlayerInfo(UpdatedPlayerData, Clamp(Money, 1000000000000, 0) - Amount)
                set PlayerInfo[_Player] = player_data:
                    GetPlayerInfo<constructor>(SourceData)
                    CurrentMoney := SourceData.CurrentMoney - Amount

# Returning true or false if our money is lower or larger than the passed in Amount
    CheckMoney(Amount : int):logic=
        if (Money < Amount):
            return false
        else:
            return true
    ReturnMoney():int=
        return Money

# Updates or sets the stats and money amount
    UpdateUI():void=
        if:
            CheckSaveDataForPlayer[MyAgent]
            _Player := player[MyAgent]
            SourceData := PlayerInfo[_Player]

            set PlayerInfo[_Player] = player_data:
                GetPlayerInfo<constructor>(SourceData)
                CurrentMoney := SourceData.CurrentMoney

        MoneyText.SetText(StringToMessage("$$$:{Money}"))
        ExpBarStrength.SetValue(Strength)
        Print ("My Money is: {Money}")
        Print ("My Strength Exp is: {Strength}")
