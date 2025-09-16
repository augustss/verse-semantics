using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

collectible_devices := class(creative_device):

    @editable
    CollectibleArray : []collectible_object_device = array{}

    PersistenceCollectibles : persistence_collectibles_device := persistence_collectibles_device{}

    var Amount<private> : int = 0
    ExpectedCollectables : int = 3
    var CollectibleWidget<private> : collectible_widget = collectible_widget{}

    OnCollectibleCollected(Agent : agent):void=
        Print("Collectible found!")
        set Amount += 1
        PersistenceCollectibles.UpdatePersistableMap(Agent)
        CollectibleWidget.AddWidget(Agent, "Found {Amount} collectibles")

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - collectible_devices") 
        set CollectibleWidget = CreateCollectibleWidget()
        if (TempCollectible := CollectibleArray[0]):
            TempCollectible.Hide() 

        if (TempCollectible := CollectibleArray[1]):
            TempCollectible.Hide() 
            TempCollectible.Show()

        for (Collectible : CollectibleArray):
            Collectible.CollectedEvent.Subscribe(OnCollectibleCollected)

    OnEnd<override>():void=
        if (Amount <> ExpectedCollectables):
            Print("NexusError - The amount of collectibles should be {ExpectedCollectables}, but got {Amount} in OnEnd() of collectible_devices")
        Print("Nexus OnEnd - collectible_devices")                 