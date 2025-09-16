
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }

@experimental
repeating_sound_component := class<final_super>(component):
    var SoundComponent:?sound_component = false
    var Connection:?cancelable = false

    @editable
    RepeatTime<public>:float = 2.0

    @editable
    MaxAudioTime<public>:float = 1.0

    var AccumulatedTime:float = 0.0
    var IsPlaying:logic = false

    OnInitialized<override>():void=
        (super:)OnInitialized()
        set SoundComponent = option { Entity.GetComponent[sound_component] }

    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()
        set Connection = option{ TickEvents.PrePhysics.Subscribe(OnExecuteWithTime) }

    OnEndSimulation<override>():void=
        (super:)OnEndSimulation()
        option{ Connection?.Cancel() }

    OnExecuteWithTime(DeltaTime:float):void=
        if (S := SoundComponent?):
            set AccumulatedTime += DeltaTime
            if (not IsPlaying?):
                if (AccumulatedTime > RepeatTime):
                    set AccumulatedTime -= RepeatTime
                    S.Play()
                    set IsPlaying = true
            else:
                if (AccumulatedTime > MaxAudioTime):
                    set AccumulatedTime -= MaxAudioTime
                    S.Stop()
                    set IsPlaying = false

    