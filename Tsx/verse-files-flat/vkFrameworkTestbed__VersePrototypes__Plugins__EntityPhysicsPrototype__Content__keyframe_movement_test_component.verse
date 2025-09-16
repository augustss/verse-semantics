using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
using { /Verse.org/SceneGraph/KeyframedMovement }
using { /Verse.org/Simulation }

@experimental
keyframe_movement_test_component := class<final_super>(component):
    @editable
    Keyframes:[]keyframed_movement_delta = array{}
    
    @editable
    PlaybackMode:keyframed_movement_playback_mode = pingpong_keyframed_movement_playback_mode{}

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        if (KeyFramedMovementComponent := Entity.GetComponent[keyframed_movement_component]):
            KeyFramedMovementComponent.SetKeyframes(Keyframes, PlaybackMode)
            KeyFramedMovementComponent.Play()
    
    OnEndSimulation<override>():void =
        if (KeyFramedMovementComponent := Entity.GetComponent[keyframed_movement_component]):
            KeyFramedMovementComponent.Stop()
        (super:)OnEndSimulation()
