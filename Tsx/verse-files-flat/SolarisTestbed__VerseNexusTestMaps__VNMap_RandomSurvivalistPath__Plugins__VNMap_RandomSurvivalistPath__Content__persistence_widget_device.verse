
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }

event_profile_data := class<final><persistable><computes>:
    EventAmount : int = 0
    FloatEventAmount : float = 0.0
    EventCaller : logic = false
    EventName:string = ""
    Vector2RandomGenerator:vector2 = vector2{X := 0.0, Y := 100.0}
    Vector3RandomGenerator:vector3 = vector3{X := 0.0, Y := 100.0, Z:= 100.0}

MakeEventProfileData<constructor>(Src:event_profile_data)<transacts> := event_profile_data:
    EventAmount := Src.EventAmount
    FloatEventAmount := Src.EventAmount * 1.0
    EventCaller := Src.EventCaller
    EventName := Src.EventName
    Vector2RandomGenerator := Src.Vector2RandomGenerator
    Vector3RandomGenerator := Src.Vector3RandomGenerator
 
MakeEventProfileData<constructor>(Src:string)<transacts> := event_profile_data:
    EventName := Src

var EventData:weak_map(player, event_profile_data) = map{}

DefaultEventProfileData:event_profile_data = event_profile_data{}

persistence_widget_device := class(creative_device):
    var EventDataMap<private> : [string]int = map{}
    PersistenceParameterCollection : UMG.PersistenceParameterCollectionObject = UMG.PersistenceParameterCollectionObject{ Material := Assets.MaterialAsset, Texture := Assets.TextureAsset}

    # Fill parameter collection object
    ToWidgetTypes(Src:event_profile_data):void=
        set PersistenceParameterCollection.IntEventAmount =  Src.EventAmount
        set PersistenceParameterCollection.FloatEventAmount = Src.FloatEventAmount

    # Update widget due to event    
    MakeEventCall<public>(EventName : string)<suspends>:void=
        for (Key->Value : EventDataMap):
            if (Key = EventName):
                if:
                    _Player := GetPlayspace().GetPlayers()[0]
                    SourceData := EventData[_Player]

                    set EventData[_Player] = event_profile_data:
                        MakeEventProfileData<constructor>(SourceData)
                        EventName := Key  
                        EventAmount := Value
                then:
                    Sleep(0.1)             
                    ToWidgetTypes(SourceData)

                    return
            Sleep(0.1)             
        if:
            _Player := GetPlayspace().GetPlayers()[0]
            SourceData := EventData[_Player]
                if:
                    set EventData[_Player] = event_profile_data:
                        MakeEventProfileData<constructor>(SourceData)
                        EventAmount := SourceData.EventAmount + 1 
                        EventName := SourceData.EventName  
                    set EventDataMap[EventName] = SourceData.EventAmount + 1       
        then:
            Sleep(0.1)                 
            ToWidgetTypes(SourceData)          
        else:
            ToWidgetTypes(MakeEventProfileData(EventName))          

  
    # Add widgets on the start        
    AddWidgetsToPlayer(PlayerUI : player_ui):void=
        WidgetsArray : []umg_widget = array{UMG.IntPersistenceWidget{PersistenceParameterCollectionObject := PersistenceParameterCollection }, 
        UMG.FloatPersistenceWidget{PersistenceParameterCollectionObject := PersistenceParameterCollection }}
        for (Widget : WidgetsArray, X := 0..10):
            MyCanvas : canvas = canvas:
                Slots := array:
                    canvas_slot:
                        Anchors := anchors{Minimum := vector2{X := X * 1.0 / 10.0, Y := 0.0}, Maximum := vector2{X := X * 1.0 / 10.0, Y := 0.0}}
                        Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                        Alignment := vector2{X := 0.0, Y := 0.0}
                        SizeToContent := true
                        Widget := Widget
            PlayerUI.AddWidget(MyCanvas)

    OnBegin<override>()<suspends>:void=
        Print("OnBegin - persistence_widget_device")

        if (_Player := GetPlayspace().GetPlayers()[0]):
            if (PlayerUI := GetPlayerUI[_Player]):
                AddWidgetsToPlayer(PlayerUI)
            else:
                Print("NexusError - cannot find PlayerUI in persistence_widget_device")    
                
            if:
                EventDataObject := EventData[_Player]
            then:
                if:
                    set EventDataMap[EventDataObject.EventName] = EventDataObject.EventAmount + 1    
                Sleep(0.1)             
                ToWidgetTypes(EventDataObject)
                

    OnEnd<override>():void=
        Print("OnEnd - persistence_widget_device")
        if (_Player := GetPlayspace().GetPlayers()[0]):
            if:
                not EventData[_Player]
                set EventData[_Player] = DefaultEventProfileData      
