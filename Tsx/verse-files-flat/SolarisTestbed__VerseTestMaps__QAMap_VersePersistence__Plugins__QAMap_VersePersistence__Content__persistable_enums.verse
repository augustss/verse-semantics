using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }

# This persistence script works around enums and saving/loading persistable data

var bear_data : weak_map(player,bear_class) = map {}


bear_class := class<final><persistable>:
    BearRank : bear_rank
    BearType : bear_type
    BearIndex : int = 0
    BearHoney : bear_honey = bear_honey.LittleHoney
    NewInt : int = 1
    SomeInt : int = 2


bear_type := enum<persistable>
    {GrizzlyBear, PandaBear, BrownBear, BlackBear, PolarBear, SunBear, KodiakBear, SlothBear, SpectacledBear}

bear_rank := enum<persistable>
    {King, General, Captain, Soldier}

bear_honey := enum<persistable>
    {ManyHoney, MuchHoney, LittleHoney, SuperHoney}

persistable_enums_device := class(creative_device):

    ToMessage<localizes>(InString : string):message = "{InString}"

    @editable
    PersistenceMessageDevice : hud_message_device = hud_message_device {}

    @editable
    SaveDataButton : button_device = button_device {}

    @editable
    LoadDataButton : button_device = button_device {}

    @editable
    ChangeRankButton : button_device = button_device {}

    @editable
    ChangeTypeButton : button_device = button_device {}

    @editable
    ChangeHoneyButton : button_device= button_device {}

    var MyBearRank : int = -1
    var MyBearType : int = -1
    var MyBearHoney : int = -1

    OnBegin<override>()<suspends>:void=
        Print ("OnBegin | persistable_enums_device")

        SaveDataButton.InteractedWithEvent.Subscribe(SaveData)
        LoadDataButton.InteractedWithEvent.Subscribe(LoadData)
        ChangeRankButton.InteractedWithEvent.Subscribe(ChangeRank)
        ChangeTypeButton.InteractedWithEvent.Subscribe(ChangeType)
        ChangeHoneyButton.InteractedWithEvent.Subscribe(ChangeHoney)

    OnEnd<override>():void=
        Print ("OnEnd | persistable_enums_device")


    SaveData(InAgent : agent):void=
        NewRank := ChooseRankBasedOnInt(MyBearRank)
        NewType := ChooseTypeBasedOnInt(MyBearType)
        NewHoney := ChooseHoneyBasedOnInt(MyBearHoney)

        BearClass := bear_class{BearRank := NewRank, BearType := NewType, BearIndex := MyBearType, BearHoney := NewHoney}
        PlayerClass := BearClass

        if (Player := player[InAgent], set bear_data[Player] = PlayerClass):
            BearRankString : string = BearRankToString(NewRank)
            BearTypeString : string = BearTypeToString(NewType)
            BearHoneyString : string = BearHoneyToString(NewHoney)

            PersistenceMessageDevice.Show(InAgent, ToMessage("[SUCCESS] Saved Bear Rank as: {BearRankString} | SaveData() \n [SUCCESS] Saved Bear Type as: {BearTypeString} | SaveData() \n [SUCCESS] Saved BearHoney as: {BearHoneyString} SaveData()"))
        else:
            Print ("[FAILURE] We failed to cast the Agent to the Player or saving to our bear_data weak_map Failed | SaveData()")
            PersistenceMessageDevice.Show(InAgent, ToMessage("[FAILURE] We failed to cast the Agent to the Player or saving to our bear_data weak_map Failed | SaveData()"))
        
        
    LoadData(InAgent : agent):void=
        if (Player := player[InAgent], PlayerClass := bear_data[Player]):

            BearClassRankString : string = BearClassToRankToString(PlayerClass)
            BearClassTypeString : string = BearClassToTypeToString(PlayerClass)
            BearClassHoneyString : string = BearClassHoneyToString(PlayerClass)

            PersistenceMessageDevice.Show(InAgent, ToMessage("[SUCCESS] Loaded BearRank as: {BearClassRankString} | LoadData() \n [SUCCESS] Loaded BearType as: {BearClassTypeString} | LoadData() \n [SUCCESS] Loaded BearHoney as: {BearClassHoneyString}"))
        else:
            Print("No Data was present to load. Please Save Data first. | LoadData()")
            PersistenceMessageDevice.Show(InAgent, ToMessage("[FAILURE] No Data was present to save. Please Save Data first. | LoadData()"))

    ChangeRank(InAgent : agent):void=
        var TempBearRank : int = MyBearRank + 1

        if (DesiredRank := Mod[TempBearRank, 4]):
            set MyBearRank = DesiredRank
            Print ("[SUCCESS] Chosen BearRank is: {DesiredRank} | ChangeRank()")
        else:
            Print ("[FAILURE] We failed to find a DesiredRank using Mod([]) | ChangeRank()")
        
        ChosenBearRank : bear_rank = ChooseRankBasedOnInt(MyBearRank)
        MyChosenBearRank : string = BearRankToString(ChosenBearRank)
        PersistenceMessageDevice.Show(InAgent, ToMessage(" \n Chosen BearRank is: {MyChosenBearRank}"))

    ChangeType(InAgent : agent):void=
        var TempBearType : int = MyBearType + 1

        if (DesiredType := Mod[TempBearType, 9]):
            set MyBearType = DesiredType
            Print ("[SUCCESS] DesiredType for Chosen BearType is: {DesiredType} | ChangeType()")
        else:
            Print ("[FAILURE] We failed to find a DesiredType  using Mod([]) | ChangeType()")
        
        ChosenBearType : bear_type = ChooseTypeBasedOnInt(MyBearType)
        MyChosenBearType : string = BearTypeToString(ChosenBearType)
        PersistenceMessageDevice.Show(InAgent, ToMessage("Chosen BearType is: {MyChosenBearType}"))

    ChangeHoney(InAgent : agent):void=
        var TempHoney : int = MyBearHoney + 1

        if (DesiredHoney := Mod[TempHoney, 4]):
            set MyBearHoney = DesiredHoney
            Print ("[SUCCESS] Chosen Honey is: {DesiredHoney} | ChangeHoney()")
        else:
            Print("[FAILURE] We failed to find a DesiredHoney using Mod([]) | ChangeHoney()")

        ChosenHoney : bear_honey = ChooseHoneyBasedOnInt(MyBearHoney)
        MyChosenHoney : string = BearHoneyToString(ChosenHoney)
        PersistenceMessageDevice.Show(InAgent, ToMessage("Chosen BearHoney is: {MyChosenHoney}"))
        
    BearClassHoneyToString(InBearClass : bear_class): string =
        case(InBearClass.BearHoney):
            bear_honey.LittleHoney => "LittleHoney"
            bear_honey.SuperHoney => "SuperHoney"
            bear_honey.MuchHoney => "MuchHoney"
            bear_honey.ManyHoney => "ManyHoney"

    # Convert our bear_rank from bear_class into a string. 
    BearClassToRankToString(InBearClass : bear_class): string = 
        case(InBearClass.BearRank):
            bear_rank.King => "King"
            bear_rank.General => "General"
            bear_rank.Captain => "Captain"
            bear_rank.Soldier => "Soldier"

    BearClassToTypeToString(InBearClass : bear_class): string =
        case(InBearClass.BearType):
            bear_type.GrizzlyBear => "GrizzlyBear"
            bear_type.PandaBear => "PandaBear"
            bear_type.BrownBear => "BrownBear"
            bear_type.BlackBear => "BlackBear"
            bear_type.PolarBear => "PolarBear"
            bear_type.SunBear => "SunBear"
            bear_type.KodiakBear => "KodiakBear"
            bear_type.SlothBear => "SlothBear"
            bear_type.SpectacledBear => "SpectacledBear"


    # Providing a list of bear_rank values for each of our choices and returning a String
    BearRankToString(BearRank : bear_rank):string =
        case(BearRank):
            bear_rank.King => "King"
            bear_rank.General => "General"
            bear_rank.Captain => "Captain"
            bear_rank.Soldier => "Soldier"
    BearHoneyToString(BearHoney : bear_honey): string =
        case(BearHoney):
            bear_honey.LittleHoney => "LittleHoney"
            bear_honey.SuperHoney => "SuperHoney"
            bear_honey.MuchHoney => "MuchHoney"
            bear_honey.ManyHoney => "ManyHoney"
    # Providing a list of bear_type values for each of our choices and returning a String
    BearTypeToString(BearType : bear_type):string = 
        case(BearType):
            bear_type.GrizzlyBear => "GrizzlyBear"
            bear_type.PandaBear => "PandaBear"
            bear_type.BrownBear => "BrownBear"
            bear_type.BlackBear => "BlackBear"
            bear_type.PolarBear => "PolarBear"
            bear_type.SunBear => "SunBear"
            bear_type.KodiakBear => "KodiakBear"
            bear_type.SlothBear => "SlothBear"
            bear_type.SpectacledBear => "SpectacledBear"
    ChooseHoneyBasedOnInt(InBearHoney : int): bear_honey =
        case(InBearHoney):
            0 => bear_honey.LittleHoney
            1 => bear_honey.SuperHoney
            2 => bear_honey.MuchHoney
            3 => bear_honey.ManyHoney
            _ => bear_honey.LittleHoney
    ChooseRankBasedOnInt(InBearRank : int): bear_rank =
        case(InBearRank):
            0 => bear_rank.Soldier
            1 => bear_rank.Captain
            2 => bear_rank.General
            3 => bear_rank.King
            _ => bear_rank.Soldier
            
    ChooseTypeBasedOnInt(InBearType : int): bear_type =
        case(InBearType):
            0 => bear_type.GrizzlyBear
            1 => bear_type.PandaBear
            2 => bear_type.BrownBear
            3 => bear_type.BlackBear
            4 => bear_type.PolarBear
            5 => bear_type.SunBear
            6 => bear_type.KodiakBear
            7 => bear_type.SlothBear
            8 => bear_type.SpectacledBear
            _ => bear_type.GrizzlyBear

