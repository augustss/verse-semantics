
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

var data_map : weak_map(player, []string) = map{}

verse_runtime_error_device := class(creative_device):

    @editable
    EnableAutomation:logic=true

    @editable
    DelayTime:float = 0.0

    @editable
    VerseRuntimeTestInt : int = 0
    # Key: 
    # 0 = InfiniteLoop
    # 1 = IntOverflow
    # 2 = Internal_GetTransform
    # 3 = ErrorRequested
    # 4 = MemoryLimitExceeded

    OnBegin<override>()<suspends>:void=

        InfiniteLoopButton.InteractedWithEvent.Subscribe(OnButtonRuntimeError_InfiniteLoop)
        MemoryLimitExceededButton.InteractedWithEvent.Subscribe(OnButtonRuntimeError_MemoryLimitExceeded)
        Internal_GetTransformButton.InteractedWithEvent.Subscribe(OnButtonRuntimeError_Internal_GetTransform)
        ErrorRequestedButton.InteractedWithEvent.Subscribe(OnButtonRuntimeError_ErrorRequested)
        CauseIntOverflowButton.InteractedWithEvent.Subscribe(OnButtonRuntimeError_CauseIntOverflow)

        if(not EnableAutomation?){return}

        Sleep(DelayTime)
        case(VerseRuntimeTestInt):
            0=> RuntimeError_InfiniteLoop()
            1=> RuntimeError_Internal_GetTransform()
            2=> RuntimeError_CauseIntOverflow() 
            3=> RuntimeError_ErrorRequested()
            4=> RuntimeError_MemoryLimitExceeded()
            _=>

    # Confirmed
    RuntimeError_MemoryLimitExceeded()<suspends>:void=
        for(Index:=0..10000):
            spawn{BranchNodes()}

    # Confirmed
    RuntimeError_InfiniteLoop()<suspends>:void=
        loop:
            if(false?){break}
        
    # Confirmed
    RuntimeError_Internal_GetTransform():void=

        CreativeProp:creative_prop=creative_prop{}
        CreativePropTransform:=CreativeProp.GetTransform()
        TransformXFloat:=CreativePropTransform.Translation.X

    # Confirmed
    RuntimeError_CauseIntOverflow() : void =
            R:= 9223372036854775807 + 1

    # Confirmed
    RuntimeError_ErrorRequested():void=
        Err("My custom error message that can be anything in the world")

    # Not confirmed
    # TODO: Test limits of string being 100,000 chars
    TestRuntimeError(Agent:agent):void=
        if:
            # Player:=Self.GetPlayspace().GetPlayers()[0]
            # PlayerAgent:=agent[Player]
            Player:=player[Agent]
            not data_map[Player]
            set data_map[Player] = array{""}
        then:
            Print("Made it to the then")
            for(Index:=0..200):
                if:
                    set data_map[Player] += array{"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""}

    ###########################
    # *       Buttons       * #
    ###########################  
            
    @editable
    InfiniteLoopButton:button_device=button_device{}

    @editable
    MemoryLimitExceededButton:button_device=button_device{}

    @editable
    Internal_GetTransformButton:button_device=button_device{}

    @editable
    CauseIntOverflowButton:button_device=button_device{}

    @editable
    ErrorRequestedButton:button_device=button_device{}

    OnButtonRuntimeError_MemoryLimitExceeded(Agent:agent):void=
        spawn:
            RuntimeError_MemoryLimitExceeded()

    OnButtonRuntimeError_InfiniteLoop(Agent:agent):void=
        spawn:
            RuntimeError_InfiniteLoop()
        
    OnButtonRuntimeError_Internal_GetTransform(Agent:agent):void=
        RuntimeError_Internal_GetTransform()

    OnButtonRuntimeError_CauseIntOverflow(Agent:agent) : void =
        RuntimeError_CauseIntOverflow()

    OnButtonRuntimeError_ErrorRequested(Agent:agent):void=
        RuntimeError_ErrorRequested()
        
    ###########################
    # *       Helpers       * #
    ###########################
    BranchNodes()<suspends>:void={for(Index:=0..1000){spawn{MoreBranches()}}}
    MoreBranches()<suspends>:void={Sleep(60.0)}
    