
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

data_layer_manager := class(creative_device):
    @editable
    ActivateDataLayerTrigger : trigger_device = trigger_device {}
    @editable
    DeactivateDataLayerTrigger : trigger_device = trigger_device {}

    @editable
    ActivateDataLayerCineSeqDevice : cinematic_sequence_device = cinematic_sequence_device {}
    @editable
    DeactivateDataLayerCineSeqDevice : cinematic_sequence_device = cinematic_sequence_device {}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        ActivateDataLayerTrigger.TriggeredEvent.Subscribe(ActivateDataLayer)
        DeactivateDataLayerTrigger.TriggeredEvent.Subscribe(DeactivateDataLayer)


    ActivateDataLayer(MaybeAgent : ?agent):void=
        if (Agent : agent = MaybeAgent?):
            Print("Calling | ActivateDataLayer(MaybeAgent : ?agent)")
            ActivateDataLayerCineSeqDevice.Play()

    DeactivateDataLayer(MaybeAgent : ?agent):void=
        if (Agent : agent = MaybeAgent?):
            Print("Calling | DeactivateDataLayer(MaybeAgent : ?agent)")
            DeactivateDataLayerCineSeqDevice.Play()

        