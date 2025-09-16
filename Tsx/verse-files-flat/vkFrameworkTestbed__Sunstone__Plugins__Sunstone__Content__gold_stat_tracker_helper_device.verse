
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


gold_stat_wrapper := class<concrete>:
    @editable
    GoldGranter : item_granter_device = item_granter_device{}
    @editable
    GoldCost : int = 0

gold_stat_tracker_helper_device := class(creative_device):

    @editable
    GoldGranters : []gold_stat_wrapper = array{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        for (Granter : GoldGranters):
            spawn{HandleGoldGrantedScored(Granter)}

    HandleGoldGrantedScored(GoldWrapper :gold_stat_wrapper)<suspends>:void=
        loop:
            Sleep(0.0)

            Agent := GoldWrapper.GoldGranter.ItemGrantedEvent.Await()


            Print("Gold granted event. Gold cost {GoldWrapper.GoldCost}")

            if (GEM := global_events_accessor{}.Instance[Self]):
                GEM.TriggerEvent(event_type.Player, sub_events.GoldEarned, ?Agent:= option{Agent}, ?IntArg := option{GoldWrapper.GoldCost})

            Sleep(0.0)

