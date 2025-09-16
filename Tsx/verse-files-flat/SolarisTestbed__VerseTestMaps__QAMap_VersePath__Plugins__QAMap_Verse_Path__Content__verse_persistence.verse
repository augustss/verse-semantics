using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Random }
 
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Random }
 
var MyPersistenceMap:weak_map(player, my_persistence_class) = map{}
 
my_persistence_class := class<final><persistable>:
    MyPersistenceInt:int
 
test_device := class(creative_device):
 
    @editable
    MessageDevice<private> : hud_message_device = hud_message_device{}
 
    IntToText<localizes><private>(Number:int):message = "Loaded: {Number}"
 
    OnBegin<override>()<suspends>:void=
        if (Player := GetPlayspace().GetPlayers()[0]):
            if:
                not MyPersistenceMap[Player]
                set MyPersistenceMap[Player] = my_persistence_class{MyPersistenceInt := GetRandomInt(1,10)}
            else:
                Print("Player exists in our map already, we do nothing but their value is saved")
            if (MyPersistenceClass := MyPersistenceMap[Player]):
                MessageDevice.SetText(IntToText(MyPersistenceClass.MyPersistenceInt))
                MessageDevice.Show(Player)


using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }

PersistenceModule := module:
    var bear_data : weak_map(player,bear_class) = map {}


    bear_class := class<final><persistable><public>:
        BearRank : bear_rank
        BearType : bear_type
        BearIndex : int = 0


    bear_type := enum<persistable><private>
        {GrizzlyBear, PandaBear, BrownBear, BlackBear, PolarBear, SunBear, KodiakBear, SlothBear, SpectacledBear}

    bear_rank := enum<persistable><public>
        {King, General, Captain, Soldier}

    persistence_manager_device := class(creative_device):

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

        var MyBearRank : int = -1
        var MyBearType : int = -1


        OnBegin<override>()<suspends>:void=
            Print ("OnBegin persistence_manager_device")

            SaveDataButton.InteractedWithEvent.Subscribe(SaveData)
            LoadDataButton.InteractedWithEvent.Subscribe(LoadData)
            ChangeRankButton.InteractedWithEvent.Subscribe(ChangeRank)
            ChangeTypeButton.InteractedWithEvent.Subscribe(ChangeType)

        OnEnd<override>():void=
            Print ("OnEnd persistence_manager_device")

        FooFunc():void=
            Print("Foo Func | persistence_manager_device")
            
        SaveData(InAgent : agent):void=
            NewRank := ChooseRankBasedOnInt(MyBearRank)
            NewType := ChooseTypeBasedOnInt(MyBearType)
            BearClass := bear_class{BearRank := NewRank, BearType := NewType, BearIndex := MyBearType}
            PlayerClass := BearClass

            if (Player := player[InAgent], set bear_data[Player] = PlayerClass):
                BearRankString : string = BearRankToString(NewRank)
                BearTypeString : string = BearTypeToString(NewType)
                PersistenceMessageDevice.Show(InAgent, ToMessage("[SUCCESS] Saved Bear Rank as: {BearRankString} | SaveData() \n [SUCCESS] Saved Bear Type as: {BearTypeString} | SaveData()"))
            else:
                Print ("[FAILURE] We failed to cast the Agent to the Player or saving to our bear_data weak_map Failed | SaveData()")
                PersistenceMessageDevice.Show(InAgent, ToMessage("[FAILURE] We failed to cast the Agent to the Player or saving to our bear_data weak_map Failed | SaveData()"))
            
            
        LoadData(InAgent : agent):void=
            if (Player := player[InAgent], PlayerClass := bear_data[Player]):

                BearClassRankString : string = BearClassToRankToString(PlayerClass)
                BearClassTypeString : string = BearClassToTypeToString(PlayerClass)

                PersistenceMessageDevice.Show(InAgent, ToMessage("[SUCCESS] Loaded BearRank as: {BearClassRankString} | LoadData() \n [SUCCESS] Loaded BearType as: {BearClassTypeString} | LoadData()"))
            else:
                Print("No Data was present to save. Please Save Data first. | LoadData()")
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
                
                
                

