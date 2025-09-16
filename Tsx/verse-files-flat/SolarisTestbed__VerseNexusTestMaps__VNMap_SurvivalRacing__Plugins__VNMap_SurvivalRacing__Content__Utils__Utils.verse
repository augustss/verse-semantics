using { /Verse.org/Simulation }
using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/Diagnostics }


# Wrapping Subscribe() to pass additional data to listeners
# This is for when you want to Subscribe() to a listenable but you want to pass along your own data. This works for callbacks that take `agent` and those that take an empty tuple, ie. `tuple()`

# subscribing to an event that returns an agent
(Listenable:listenable(agent)).SubscribeAgentWithExtra<public>(OutputFunc : tuple(agent, t)->void, ExtraData : t where t:type) : cancelable=
    Wrapper := wrapper_agent(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)


wrapper_agent(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(agent, t) -> void
    InputFunc(Agent: agent):void = OutputFunc(Agent, ExtraData)


# subscribing to an event that returns tuple(), aka an empty tuple
(Listenable : listenable(tuple())).SubscribeEmptyWithExtra<public>(OutputFunc : t -> void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_empty(t) {ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
 

wrapper_empty(t : type) := class():
    ExtraData : t;
    OutputFunc : t -> void
    InputFunc():void = OutputFunc(ExtraData)


CustomLogChannel := class(log_channel){}

Logger<public> := class:
    ErrorLog<public>:log = log{Channel:=CustomLogChannel, DefaultLevel:=log_level.Error}
    InfoLog<public>:log = log{Channel:=CustomLogChannel, DefaultLevel:=log_level.Error}

