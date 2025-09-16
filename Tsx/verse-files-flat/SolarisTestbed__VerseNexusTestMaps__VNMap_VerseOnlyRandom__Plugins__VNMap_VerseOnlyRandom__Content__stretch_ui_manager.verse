using { /Fortnite.com/Devices}
using { /Verse.org/Simulation}
using { /Fortnite.com/Characters}
using { /Fortnite.com/Game}


stack<public>(t : type) := class:
    Elements<internal>:[]t = array {}

    Push<public>(NewElement : t):stack (t)= 
        stack(t){Elements := Elements + array{NewElement}}

    Pop<public>()<decides><transacts>:tuple(stack(t), t)= 
        FirstElement := Peek[]
        (stack(t){Elements := Elements.RemoveElement[Elements.Length - 1]}, FirstElement)

    Size<public>()<transacts>: int = 
        Elements.Length

    IsEmpty<public>()<decides><transacts>:void=
        Size() = 0

    Peek<public>()<decides><transacts>:t = 
        Elements[Elements.Length - 1]

CreateStack<public><constructor>(InitialElements : []t where t:type) := stack(t): 
    Elements := InitialElements


stretch_ui_manager := class(creative_device):
    @editable
    PlayerSpawners : []player_spawner_device = array {}

    @editable
    var Sequencers1024 : []cinematic_sequence_device = array {}

    var SequencerStack1024 : stack(cinematic_sequence_device) = stack(cinematic_sequence_device) {}

    var PlayerSequencerMap : [agent]cinematic_sequence_device = map {}


    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - stretch_ui_manager")
        set SequencerStack1024 = CreateStack(Sequencers1024)

        for (PlayerSpawner : PlayerSpawners):
            PlayerSpawner.SpawnedEvent.Subscribe(Apply1024)

    OnEnd<override>():void=
        Print("Nexus OnEnd - stretch_ui_manager")


    Apply1024(Agent : agent):void=
        Print("Applying Stretch Res | Apply1024 | stretch_ui_manager")
        Stop(Agent)

        if:
            Exists := PlayerSequencerMap[Agent]
        
        then:

        else:        
            
            if (1 + 1 = 2):
                if (2 + 2 = 4):
                    if (4 + 4 = 8):
                        if (8 + 8 = 16):
                            if (16 + 16 = 32):
                                if (1 + 1 = 2):
                                    if (2 + 2 = 4):
                                        if (4 + 4 = 8):
                                            if (8 + 8 = 16):
                                                if (16 + 16 = 32):
                                                    if (1 + 1 = 2):
                                                        if (2 + 2 = 4):
                                                            if (4 + 4 = 8):
                                                                if (8 + 8 = 16):
                                                                    if (16 + 16 = 32):
                                                                        if (1 + 1 = 2):
                                                                            if (2 + 2 = 4):
                                                                                if (4 + 4 = 8):
                                                                                    if (8 + 8 = 16):
                                                                                        if (16 + 16 = 32):
                                                                                            if (1 + 1 = 2):
                                                                                                if (2 + 2 = 4):
                                                                                                    if (4 + 4 = 8):
                                                                                                        if (8 + 8 = 16):
                                                                                                            if (16 + 16 = 32):
                                                                                                                if (1 + 1 = 2):
                                                                                                                    if (2 + 2 = 4):
                                                                                                                        if (4 + 4 = 8):
                                                                                                                            if (8 + 8 = 16):
                                                                                                                                if (16 + 16 = 32):
                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                                if (4 + 4 = 8):
                                                                                                                                                                    if (8 + 8 = 16):
                                                                                                                                                                        if (16 + 16 = 32):
                                                                                                                                                                            if (1 + 1 = 2):
                                                                                                                                                                                if (2 + 2 = 4):
                                                                                                                                                                                    if (4 + 4 = 8):
                                                                                                                                                                                        if (8 + 8 = 16):
                                                                                                                                                                                            if (16 + 16 = 32):
                                                                                                                                                                                                if (1 + 1 = 2):
                                                                                                                                                                                                    if (2 + 2 = 4):
                                                                                                                                                                                                        if (4 + 4 = 8):
                                                                                                                                                                                                            if (8 + 8 = 16):
                                                                                                                                                                                                                if (16 + 16 = 32):
                                                                                                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                                                                                                                if (4 + 4 = 8):
                                                                                                                                                                                                                                                    if (8 + 8 = 16):
                                                                                                                                                                                                                                                        if (16 + 16 = 32):
                                                                                                                                                                                                                                                            if (1 + 1 = 2):
                                                                                                                                                                                                                                                                if (2 + 2 = 4):
                                                                                                                                                                                                                                                                    if (4 + 4 = 8):
                                                                                                                                                                                                                                                                        if (8 + 8 = 16):
                                                                                                                                                                                                                                                                            if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                    if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                        if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                            if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                                if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                                                                if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                                                                    if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                                                                        if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                                                            if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                                                                if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                    if(ReturnTuple := SequencerStack1024.Pop[]):

                                                                                                                                                                                                                                                                                                                                                        set SequencerStack1024 = ReturnTuple(0)

                                                                                                                                                                                                                                                                                                                                                        MySequencer := ReturnTuple(1)

                                                                                                                                                                                                                                                                                                                                                        if (set PlayerSequencerMap[Agent] = MySequencer):
                                                                                                                                                                                                                                                                                                                                                            Print ("Sequencer is playing now | Apply1024 | stretch_ui_manager")
                                                                                                                                                                                                                                                                                                                                                            MySequencer.Play(Agent)
                                                                                                                                                                                                                                                                                                                                                    else:
                                                                                                                                                                                                                                                                                                                                                        Print("No Sequencers available | Apply1024() | stretch_ui_manager")
    
    Stop(Agent : agent):void=
        Print("Stopping | Stop() | stretch_ui_manager")
        if (1 + 1 = 2):
            if (2 + 2 = 4):
                if (4 + 4 = 8):
                    if (8 + 8 = 16):
                        if (16 + 16 = 32):
                            if (1 + 1 = 2):
                                if (2 + 2 = 4):
                                    if (4 + 4 = 8):
                                        if (8 + 8 = 16):
                                            if (16 + 16 = 32):
                                                if (1 + 1 = 2):
                                                    if (2 + 2 = 4):
                                                        if (4 + 4 = 8):
                                                            if (8 + 8 = 16):
                                                                if (16 + 16 = 32):
                                                                    if (1 + 1 = 2):
                                                                        if (2 + 2 = 4):
                                                                            if (4 + 4 = 8):
                                                                                if (8 + 8 = 16):
                                                                                    if (16 + 16 = 32):
                                                                                        if (1 + 1 = 2):
                                                                                            if (2 + 2 = 4):
                                                                                                if (4 + 4 = 8):
                                                                                                    if (8 + 8 = 16):
                                                                                                        if (16 + 16 = 32):
                                                                                                            if (1 + 1 = 2):
                                                                                                                if (2 + 2 = 4):
                                                                                                                    if (4 + 4 = 8):
                                                                                                                        if (8 + 8 = 16):
                                                                                                                            if (16 + 16 = 32):
                                                                                                                                if (1 + 1 = 2):
                                                                                                                                    if (2 + 2 = 4):
                                                                                                                                        if (4 + 4 = 8):
                                                                                                                                            if (8 + 8 = 16):
                                                                                                                                                if (16 + 16 = 32):
                                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                                                if (4 + 4 = 8):
                                                                                                                                                                                    if (8 + 8 = 16):
                                                                                                                                                                                        if (16 + 16 = 32):
                                                                                                                                                                                            if (1 + 1 = 2):
                                                                                                                                                                                                if (2 + 2 = 4):
                                                                                                                                                                                                    if (4 + 4 = 8):
                                                                                                                                                                                                        if (8 + 8 = 16):
                                                                                                                                                                                                            if (16 + 16 = 32):
                                                                                                                                                                                                                if (1 + 1 = 2):
                                                                                                                                                                                                                    if (2 + 2 = 4):
                                                                                                                                                                                                                        if (4 + 4 = 8):
                                                                                                                                                                                                                            if (8 + 8 = 16):
                                                                                                                                                                                                                                if (16 + 16 = 32):
                                                                                                                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                                                                                                                                if (4 + 4 = 8):
                                                                                                                                                                                                                                                                    if (8 + 8 = 16):
                                                                                                                                                                                                                                                                        if (16 + 16 = 32):
                                                                                                                                                                                                                                                                            if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                    if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                        if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                            if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                    if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                                        if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                                            if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                                                if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                                    if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                                        if (2 + 2 = 4):
                                                                                                                                                                                                                                                                                                                            if (4 + 4 = 8):
                                                                                                                                                                                                                                                                                                                                if (8 + 8 = 16):
                                                                                                                                                                                                                                                                                                                                    if (16 + 16 = 32):
                                                                                                                                                                                                                                                                                                                                        if (1 + 1 = 2):
                                                                                                                                                                                                                                                                                                                                            if (2 + 2 = 4):
                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                if (RemovePlayer : cinematic_sequence_device = PlayerSequencerMap[Agent]):
                                                                                                                                                                                                                                                                                                                                                    RemovePlayer.Stop()
                                                                                                                                                                                                                                                                                                                                                    RemovePlayer.SetPlaybackFrame(0)


    

        

