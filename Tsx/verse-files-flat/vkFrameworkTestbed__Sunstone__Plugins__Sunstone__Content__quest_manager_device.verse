
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


quest_difficulty<public> := enum:
    Easy,
    Medium,
    Hard

quest<public> := struct<concrete>:
    @editable
    Difficulty<public> : quest_difficulty = quest_difficulty.Easy
    @editable
    QuestTitle<public> : string = ""
    @editable
    QuestDescription<public> : string = ""
    @editable
    RewardDesciption<public> : string = ""
    @editable
    CompletionValue<public> :int = 0
    @editable
    ExpAwarded<public> : int = 0

quest_manager_device := class(creative_device):
    @editable
    Quests : []quest = array{}

    var EventsManagerRef : ?global_events_manager = false

    OnBegin<override>()<suspends>:void=        

        if (EventsManager := global_events_accessor{}.Instance[Self]):
            set EventsManagerRef = option{EventsManager}

            if:
                EventsManager.RegisterEvent[event_type.Player, UpdateQuestProgress]
            else:
                Print("Failed to Subscribe to Quest Complete events")

    UpdateQuestProgress(SubEvent: sub_events, Agent: ?agent, QuestIndex :?int):void=
        #check if completed
        if:
            SubEvent = sub_events.QuestComplete
            HandleQuestComplete(Agent? ,QuestIndex?)


    HandleQuestComplete(Agent: agent, QuestIndex :int)<transacts>:void=

        # fire character xp event
        return
