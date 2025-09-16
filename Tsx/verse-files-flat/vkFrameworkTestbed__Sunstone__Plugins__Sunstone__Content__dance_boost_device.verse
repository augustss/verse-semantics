
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }

dance_boost_device := class(creative_device):

    @editable
    DanceVolume:volume_device = volume_device{}

    #@editable_slider(float){SliderDelta:=option{0.5}}
    @editable
    BoostTime:float = 2.0
    
    #@editable_slider(float){SliderDelta:=option{5.0}}
    @editable
    HealAmount:float = 5.0

    #@editable_slider(float){SliderDelta:=option{5.0}}
    @editable
    ShieldRecoverAmount:float = 5.0

    var AgentsInVolume:[]agent = array{}

    OnBegin<override>()<suspends>:void=
        DanceVolume.AgentEntersEvent.Subscribe(OnAgentEnters)
        DanceVolume.AgentExitsEvent.Subscribe(OnAgentExits)

    OnAgentEnters(Agent:agent):void=
        IsRunning := if (AgentsInVolume.Length > 0) then true else false
        set AgentsInVolume += array{Agent}
        if (not IsRunning?):
            spawn{BoostPlayers()}

    OnAgentExits(Agent:agent):void=
        if (AgentsInVolume.RemoveFirstElement[Agent]):

    BoostPlayers()<suspends>:void=
        loop:
            Sleep(BoostTime)
            for (Agent:AgentsInVolume):
                if (Character := Agent.GetFortCharacter[]):
                    Character.Heal(HealAmount)
                    Character.SetShield(Character.GetShield() + ShieldRecoverAmount)
                    #TODO: Boost Character Speed

            if (AgentsInVolume.Length = 0):
                break
