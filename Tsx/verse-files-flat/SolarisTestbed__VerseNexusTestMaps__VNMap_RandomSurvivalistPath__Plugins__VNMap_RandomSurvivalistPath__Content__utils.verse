# Copyright Epic Games, Inc. All Rights Reserved.
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Simulation }
using { /Fortnite.com/Game }
necessary_objects_nearcharacter := class(tag){}

# Wrappers for transfer event var in different class
(Listenable : listenable(agent)).SubscribeWithEvent(OutputFunc : tuple(agent, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := event_wrapper_agent(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
 
###################### fort_char utils  ########################################## 
(Listenable : listenable(damage_result)).SubscribeToDamageResultWithType(OutputFunc : tuple(damage_result, t)->void, ExtraData : t where t:type)<transacts> : cancelable =
    Wrapper := event_wrapper_damage_result(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)

(Listenable : listenable(healing_result)).SubscribeToHealingResultWithType(OutputFunc : tuple(healing_result, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := event_wrapper_healing_result(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc) 

event_wrapper_healing_result(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(healing_result, t) -> void
    InputFunc(DamageResult : healing_result):void = OutputFunc(DamageResult, ExtraData)

event_wrapper_damage_result(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(damage_result, t) -> void
    InputFunc(DamageResult : damage_result):void = OutputFunc(DamageResult, ExtraData)

######################

event_wrapper_agent(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(agent, t) -> void
    InputFunc(Agent : agent):void = OutputFunc(Agent, ExtraData)

function_wrapper(t : type) := class():
    ExtraData : t;

    OutputFunc : t -> void
    InputFunction():void = OutputFunc(ExtraData)    
    
random_event_caller := class():
    CallRandomEvent<public>(OutputFunc : t -> void, ExtraData : t where t:type):void=
        Wrapper := function_wrapper(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
        Wrapper.InputFunction() 