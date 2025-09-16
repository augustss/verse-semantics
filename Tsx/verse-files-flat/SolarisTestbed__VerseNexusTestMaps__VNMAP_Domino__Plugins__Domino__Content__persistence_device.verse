using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Game }
using { /Verse.org/Colors }

persistable_struct := struct<persistable>:
    PersistableStructArray<public> : []string = array{"😁", "😁", "😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁","😁", "😁"} 
    G<public>:int = 0
    B<public>:int = 0

persistable_enum := enum<persistable>{
    First,
    Second,
    Third
}

persistable_class := class<persistable><final>:
    Integer : int = 1
    Float : float = 1.1
    Void : void = {} # Will be not visible in message
    String : string = "🐈"
    Char32 : char32 = 'é' # Will be not visible in message
    Char8 : char =  'e'
    Option : ?persistable_class = false 

    Vector2 : vector2 = vector2{X:= 0.0, Y:= 0.0}
    Vector3 : vector3 = vector3{X:= 0.0, Y:= 0.0, Z:= 0.0}
    Vector2i : vector2i = vector2i{X:= 0, Y:= 0} 
    Color : color = color{R:=255.0, G:=255.0, B:=255.0}

    Tuple : tuple(int, float, string) = (1, 2.0, "three")
    
    PersistableArray : []string = array{"🐈", "😁"} 
    PersistableMap : [string]int = map{"one" => 1, "two" => 2} 
    PersistableStruct : persistable_struct = persistable_struct{}
    PersistableEnum : persistable_enum = persistable_enum.First
    PersistableChildClass : persistable_class_child = persistable_class_child{IntegerChild := 0}

persistable_class_child := class<persistable><final>:
    IntegerChild : int = 1
    GetValue():int=
        return IntegerChild

MakeComplexClass<constructor>() := persistable_class:

var ClassMap:weak_map(player, persistable_class) = map{}

persistence_complex_class_device := class(creative_device):

    @editable
    BillboardWithComplexClass: billboard_device = billboard_device{}

    @editable
    BillboardWithStructArray: billboard_device = billboard_device{}

    MakeMessage<localizes>(InString:string):message = "{InString}"

    ShowOnComplexClassBillboard(Message : string):void=
        BillboardWithComplexClass.SetText(MakeMessage(Message))
        BillboardWithComplexClass.UpdateDisplay()

    ShowOnStructArrayBillboard(Message : string):void=
        BillboardWithStructArray.SetText(MakeMessage(Message))
        BillboardWithStructArray.UpdateDisplay()

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - persistence_complex_class_device")
        Playspace := GetPlayspace()
        if (_Player := Playspace.GetPlayers()[0]):
            if(ComplexClass := ClassMap[_Player])
            then {
                Print("Class loaded")
                ShowOnComplexClassBillboard(ToString(ComplexClass))
                ShowOnStructArrayBillboard(ToStringArrayStruct(ComplexClass))
            }
            else {
                ShowOnComplexClassBillboard("Class not loaded")
                ShowOnStructArrayBillboard("Class not loaded")
            }

    OnEnd<override>():void=
        Print("Nexus OnEnd - persistence_complex_class_device")
        Playspace := GetPlayspace()
        if (_Player := Playspace.GetPlayers()[0]):
            ComplexClass := MakeComplexClass()
            if(FitsInPlayerMap[ComplexClass], set ClassMap[_Player] = ComplexClass)
            then {
                Print("Class saved")
            }
            else {
                Print("NexusError - complex class should be available to save in map in OnEnd()")
            }


    ToStringArrayStruct(PersistableClass : persistable_class):string =
        var FinalString : string = ""
        
        set FinalString += "Stuct Array: "    
        for (Index := 0..PersistableClass.PersistableStruct.PersistableStructArray.Length - 1):
            if (Element := PersistableClass.PersistableStruct.PersistableStructArray[Index]):
                set FinalString += "{Element}, "         
        return FinalString      

    ToString(PersistableClass : persistable_class):string =
        var FinalString : string =
            "Int: {PersistableClass.Integer}, Float: {PersistableClass.Float}, Char8: {PersistableClass.Char8}, String: {PersistableClass.String}, Map: "
        for (Key->Value : PersistableClass.PersistableMap):
            set FinalString += " {Value} in map at key {Key} "
        set FinalString += "Array: "    
        for (Index := 0..PersistableClass.PersistableArray.Length - 1):
            if (Element := PersistableClass.PersistableArray[Index]):
                set FinalString += "{Element} in PersistableArray " 

        var TupleInt: int = PersistableClass.Tuple(0)
        var TupleFloat: float = PersistableClass.Tuple(1)
        var TupleString: string = PersistableClass.Tuple(2)

        set FinalString += "TupleInt: {TupleInt}, TupleFloat: {TupleFloat}, TupleString: {TupleString}, "    

        set FinalString += "Enum: {GetEnumString(PersistableClass.PersistableEnum)}, "    
        
        if (Option :=PersistableClass.Option?):
            set FinalString += "Option: {Option}, "

        set FinalString += "Struct elements: {PersistableClass.PersistableStruct.G}, {PersistableClass.PersistableStruct.B}, "

        set FinalString += "Vectors elements: {PersistableClass.Vector2}, {PersistableClass.Vector3}, {PersistableClass.Vector2i}, "
        set FinalString += "Colors elements: {PersistableClass.Color.R}, {PersistableClass.Color.G}, {PersistableClass.Color.B}, "

        set FinalString += "Stuct Array: "    
        for (Index := 0..PersistableClass.PersistableStruct.PersistableStructArray.Length - 1):
            if (Element := PersistableClass.PersistableStruct.PersistableStructArray[Index]):
                set FinalString += "{Element} in PersistableStructArray " 

        set FinalString += "ChildClass: {PersistableClass.PersistableChildClass.GetValue()}, "

        return FinalString      

    GetEnumString(Enum:persistable_enum)<computes>:string=
        case(Enum):
            persistable_enum.First =>
                "First"
            persistable_enum.Second =>
                "Second"
            persistable_enum.Third =>
                "Third"

var PersistableCollectablesMap:weak_map(player, int) = map{}

persistence_collectibles_device := class(creative_device):

    @editable
    Messager:hud_message_device = hud_message_device{}

    var TotalAmountOfCollectibles : int = 0

    MakeMessage<localizes>(InString:string):message = "{InString}"

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - persistence_collectibles_device")
        Playspace := GetPlayspace()
        if (_Player := Playspace.GetPlayers()[0]):
            ShowValue(_Player)    

    ShowValue(_Player : player):void=
        if(PersistenceCollectiblesAmount := PersistableCollectablesMap[_Player])
        then {
            set TotalAmountOfCollectibles = PersistenceCollectiblesAmount
            Messager.Show(_Player, MakeMessage("Total amount of collectables: " + ToString(TotalAmountOfCollectibles)))
        }
        else {
            Messager.Show(_Player, MakeMessage("Reading from map - Failed"))
        }

    UpdatePersistableMap<public>(Agent : agent):void=
        if(PersistenceCollectiblesAmount := PersistableCollectablesMap[Agent])
        then {
            set TotalAmountOfCollectibles = PersistenceCollectiblesAmount
            set TotalAmountOfCollectibles += 1
        }
        else:
            set TotalAmountOfCollectibles = 1

        if(P:= player[Agent], set PersistableCollectablesMap[P] = TotalAmountOfCollectibles)
        then {
            Messager.Show(Agent, MakeMessage("A new collectables added!"))
            ShowValue(P)
        }
        else {
            Messager.Show(Agent, MakeMessage("Saving to map - Failed"))
            Print("NexusError - collectibles should be available to save in map in UpdatePersistableMap() of persistence_device")
        }

    OnEnd<override>():void=
        Print("Nexus OnEnd - persistence_collectibles_device")