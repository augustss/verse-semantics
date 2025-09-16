
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }


world_manipulator_device := class(creative_device):

    @editable
    DaySequencerTriggerDevice : trigger_device = trigger_device{}

    OnBegin<override>()<suspends>:void=
        Print("OnBegin - world_manipulator_device")
        if (MaybePlayer := GetPlayspace().GetPlayers()[0]):
            RandomValue := Shuffle(array{true, false})
            if (RandomValue[0]?):
                DaySequencerTriggerDevice.Trigger(MaybePlayer)
        