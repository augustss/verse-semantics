using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Colors}

# This persistence script works around different value types in arrays/maps while saving/loading persistable data

persistable_class := class<persistable><final>:
    PersistableArray : []string = array{} 
    PersistableMap : [string]int = map{"apple" => 11, "pear" => 7} 
    IntValues:[]int = array{-1}
    IntValue:int = 0
    TheStructs: []SomeStructs = array {}

    Integer : int = 0
    Float : float = 2.1
    Char : []char = "H"

SomeStructs := struct<persistable>:

var ClassMap:weak_map(player, persistable_class) = map{}

persistable_values_device := class(creative_device):

    @editable
    Messager:hud_message_device = hud_message_device{}

    MakeMessage<localizes>(InString:string):message = "{InString}"

    @editable
    SaveButtonOne:button_device = button_device{}

    @editable
    SaveButtonTwo:button_device = button_device{}

    @editable
    LoadButton:button_device = button_device{}


    PersistableClassOne : persistable_class = persistable_class{Integer := 1, Float := 1.0, Char := "One", PersistableArray := array{"One"}, PersistableMap:= map{"One" => 1, "Two" => 1} }
    PersistableClassTwo : persistable_class = persistable_class{Integer := 2, Float := 22.0, Char := "Two", PersistableArray := array{"Two"}, PersistableMap:= map{"One" => 2, "Two" => 2} }

    OnBegin<override>()<suspends>:void=
        Print("OnBegin | persistable_values_device")

        SaveButtonOne.InteractedWithEvent.Subscribe(SaveData1)
        SaveButtonTwo.InteractedWithEvent.Subscribe(SaveData2)
        LoadButton.InteractedWithEvent.Subscribe(LoadData)

    ToString(PersistableClass : persistable_class):string =
        var FinalString : string = "\nInt: {PersistableClass.Integer},\n Float: {PersistableClass.Float}, \nChar: {PersistableClass.Char}, \nMap: "
        for (Key->Value : PersistableClass.PersistableMap):
            set FinalString += " {Value} in map at key {Key} "
        set FinalString += "Array: "    
        for (Index := 0..PersistableClass.PersistableArray.Length - 1):
            if (Element := PersistableClass.PersistableArray[Index]):
                set FinalString += "{Element} in Persistable Array at index {Index}"
        return FinalString        

    SaveData1(InAgent:agent):void=
        if(P:=player[InAgent], set ClassMap[P] = PersistableClassOne)
        then {
            Messager.Show(InAgent, MakeMessage("[SUCCESS] | Data Saved | SaveData2()"))
        }
        else {
            Messager.Show(InAgent, MakeMessage("[FAILURE] Casting Agent to Player OR Saving to map | SaveData1()"))
        }

    SaveData2(InAgent:agent):void=
        if(P:=player[InAgent], set ClassMap[P] = PersistableClassTwo)
        then {
            Messager.Show(InAgent, MakeMessage("[SUCCESS] | Data Saved | SaveData2()"))
        }
        else {
            Messager.Show(InAgent, MakeMessage("[FAILURE] Casting Agent to Player OR Saving to map | SaveData2()"))
        }

    LoadData(InAgent:agent):void=
        if(P:=player[InAgent], PC := ClassMap[P])
        then {
            Messager.Show(InAgent, MakeMessage("[SUCCESS] | Data Loaded | LoadData() " + ToString(PC)))
        }
        else {
            Messager.Show(InAgent, MakeMessage("[FAILURE] | Casting Agent to Player OR Reading from map (maybe empty) | LoadData()"))
        }

    OnEnd<override>():void=
        Print ("OnEnd | persistable_values_device")