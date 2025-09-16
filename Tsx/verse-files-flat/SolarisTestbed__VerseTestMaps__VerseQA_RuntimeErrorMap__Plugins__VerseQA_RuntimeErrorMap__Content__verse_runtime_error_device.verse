
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

var data_map : weak_map(player, []string) = map{}

verse_runtime_error_device := class(creative_device):

    @editable
    EnableAutomation:logic=true

    @editable
    VerseRuntimeTestInt : int = 0
    # Key: 
    # 0 = InfiniteLoop
    # 1 = IntOverflow
    # 2 = Internal_GetTransform
    # 3 = ErrorRequested
    # 4 = MemoryLimitExceeded

    OnBegin<override>()<suspends>:void=

        if(not EnableAutomation?){return}

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
        Err("Requested an error")

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
    # *       Helpers       * #
    ###########################
    BranchNodes()<suspends>:void={for(Index:=0..1000){spawn{MoreBranches()}}}
    MoreBranches()<suspends>:void={Sleep(60.0)}
    