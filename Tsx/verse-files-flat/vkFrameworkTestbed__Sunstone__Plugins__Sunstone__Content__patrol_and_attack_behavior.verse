# Copyright Epic Games, Inc. All Rights Reserved.
# Heavily based on the CompanionAI behaviour that responds to pings.
# - This is narrowed down to move along path & attack targets while doing so

using { /Verse.org/Random }
using { /Verse.org/Simulation }
using { /Verse.org/Colors }
using { /Verse.org/SceneGraph }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SpatialMath }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/AI }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Vehicles }
using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/SpatialMath }

log_patrol_and_attack_ai := class(log_channel){}

patrol_and_attack_ai_message_module:= module:
    SettingsCategory<public><localizes>:message = "Settings"

patrol_and_attack_behavior := class(npc_behavior):

    # Logger to record useful debug info
    Logger : log = log{Channel:=log_patrol_and_attack_ai}
    DebugDraw : debug_draw = debug_draw{}

    # global settings
    @editable:
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory}
    DebugDisplay<internal>:logic=false

    @editable:
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory}
    DebugScreenColor<internal>:color=NamedColors.Blue

    @editable_slider(float):
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory},
        MinValue:=option{0.5},
        MaxValue:=option{10.0}
    DebugScreenDuration<internal>:float=5.0

    @editable_number(float):
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory},
        MinValue:=option{0.0}
    GoToAttackTargetDistance<internal>:float=2000.0
    
    @editable:
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory}
    PatrolNodes: []ai_patrol_path_device = array{}

    @editable:
        Categories:=array{patrol_and_attack_ai_message_module.SettingsCategory}
    MainObjective: objective_device = objective_device{}

    # How far from a location does a target have to be to make the GoTo convert into an AttackCommand where the AI
    # fights the target, but doesn't try to reach the Ping's location
    Attack_TargetProximityRadius<internal>:float=1500.0
    
    OnBegin<override>()<suspends>:void=
        
        if (DebugDisplay?):
            DebugDraw.ShowChannel()

        if:
            Character:=GetAgent[].GetFortCharacter[]
            GuardActions:= GetEntity[].GetComponent[fort_guard_actions_component]
            GuardPerception:= GetEntity[].GetComponent[fort_guard_perception_component]
        then:

            AIRuntime:=patrol_and_attack_ai_runtime{
                AIBehaviorScript:=Self,
                Character:=Character,
                GuardActions:=GuardActions,
                Leashable:=GuardActions,
                GuardPerception:=GuardPerception,
                PatrolNodes:= PatrolNodes,
                MainObjective:= MainObjective}

            AIRuntime.GameLoop();

        else:
            WarningMsg:string = "Cannot start patrol AI, missing components"
            Logger.Print(WarningMsg)
            Print(WarningMsg, ?Color:=DebugScreenColor, ?Duration:=DebugScreenDuration)


@experimental
patrol_and_attack_ai_runtime<internal> := class:

    AIBehaviorScript:patrol_and_attack_behavior

    Character:fort_character

    GuardActions:fort_guard_actions_component

    Leashable:fort_leashable

    # to monitor perception
    GuardPerception:fort_guard_perception_component

    # Where we want to go
    PatrolNodes: []ai_patrol_path_device = array{}
    MainObjective: objective_device

    # Run time set of positions to move along
    var TargetMovementPositions: [](/Verse.org/SpatialMath:)vector3 = array{}

    var ShootTargetServiceActive: logic = false
    var CurrentGoToPosition: ?(/Verse.org/SpatialMath:)vector3 = false
    
    # For Debug logging & filtering out NPC decision making
    var DebugNpcID:string = "Default"
    FillTargetMovementPositions():void=
        for(TargetNode:PatrolNodes):
            set TargetMovementPositions = TargetMovementPositions + array{ConvertVector(TargetNode.GetTransform().Translation)}

        set TargetMovementPositions = TargetMovementPositions + array{ConvertVector(MainObjective.GetTransform().Translation)}


    GameLoop()<suspends>:void=

        # Initial Setup
        FillTargetMovementPositions()       
        set DebugNpcID = "{GetRandomInt(1, 10000)}"
 
        # Advance to Position And Attack if possible
        loop:
            HandleGoToPositionCommand(PickNextPositionToMoveTo())
            Sleep(0.1) # Infinite loop prevention

    PickNextPositionToMoveTo():(/Verse.org/SpatialMath:)vector3=
        if(NextPos:= TargetMovementPositions[0]):
            return NextPos

        LogAndDisplayOnScreen("No cached positions to move to! Staying at character position")
        return ConvertVector(Character.GetTransform().Translation)



    # Check if there is a known target near the movement position
    GetAttackTarget(Location:(/Verse.org/SpatialMath:)vector3)<suspends>:?fort_target_info=
        loop:
            if (GuardPerception.GetAlertLevel() = fort_guard_alert_level.Unaware):
                GuardPerception.AlertLevelChangedEvent.Await()
            else:
                var ClosestTargetDistance:float = AIBehaviorScript.Attack_TargetProximityRadius
                var ClosestTarget:?fort_target_info=false
                for(Target:GuardPerception.DetectedTargets):
                    if:
                        TargetLocation := ConvertVector(GuardPerception.GetLastKnownPosition[Target.Target])
                        TargetDistance := (/Verse.org/SpatialMath:)Distance(Location, TargetLocation)
                        TargetDistance < ClosestTargetDistance
                    then:
                        set ClosestTargetDistance = TargetDistance
                        set ClosestTarget = option{Target}

                if (AttackTarget:=ClosestTarget?):
                    LogAndDisplayOnScreen("Spotted target, attacking")
                    return option{AttackTarget}

                Sleep(0.5) # Give threats some time to move and retry


    HandleGoToPositionCommand(Location:(/Verse.org/SpatialMath:)vector3)<suspends>:void=    
        LogAndDisplayOnScreen("Entering HandleGoToPositionCommand")
        defer:
            LogAndDisplayOnScreen("Exiting HandleGoToPositionCommand")

        if:
            AlreadyNavigatingPosition:= CurrentGoToPosition?
            (/Verse.org/SpatialMath:)IsAlmostEqual[AlreadyNavigatingPosition, Location, 1.0]
        then:
            return

        set CurrentGoToPosition = option{Location}
        defer:
            set CurrentGoToPosition = false

        var AttackCommandTarget:?fort_target_info=false
        race:
            # monitor whether we should convert the GoTo to Attack
            set AttackCommandTarget = GetAttackTarget(Location)

            block:
                # Use a loop to retry to go to the position (in case of a new path, or if the NPC has moved)
                loop:
                    PositionResult:= HandleGoTo(Location)

                    # Once at destination, attack any targets, if no targets look around then move on
                    TargetInfo:?fort_target_info = option{GetTarget[]}
                    if(Target:= TargetInfo?):
                        race:
                            GetNewTarget()
                            GuardPerception.AlertLevelChangedEvent.Await()
                            if:
                                GuardPerception.GetAlertLevel() = fort_guard_alert_level.Alerted
                                GuardPerception.GetAlertLevel[Target.Target] = fort_guard_alert_level.Alerted
                            then:
                                LogAndDisplayOnScreen("Alerted target detected, attacking")
                                GoToAndAttackTask(Target)
                                return
                            else if (TargetInfo?):
                                LogAndDisplayOnScreen("Unalerted target detected, looking")
                                LookAtTargetTask()
                    else:
                        LogAndDisplayOnScreen("No target detected, looking around")
                        LookAroundAndCheckMovementTask(Location)
                        Sleep(0.1)
                    
                    if(PositionResult = true):
                        break

    # set the target and acceptable radius
    HandleGoTo(Location:(/Verse.org/SpatialMath:)vector3)<suspends>:logic= 
        LogAndDisplayOnScreen("Entering HandleGoTo")
        defer:
            LogAndDisplayOnScreen("Exiting HandleGoTo") 

        if (ShootTargetServiceActive = false):
            branch { ShootTargetService(AIBehaviorScript.GoToAttackTargetDistance) }    

        return GoToPositionTask(Location, Sprint:logic=true, ?ReachRadius:=GetRandomFloat(100.0, 500.0))



    #navigate to either the entity or the position    
    GoToPositionTask(Position:(/Verse.org/SpatialMath:)vector3, Sprint:logic, ?ReachRadius:float=10.0, ?AllowPartialPath:logic=true)<suspends>:logic=
        LogAndDisplayOnScreen("Entering GoToPosition {ToString(Position)}")
        defer:
            LogAndDisplayOnScreen("Exiting GoToPosition") 

        if (Sprint?):
            branch { TacticalSprintService() }

        Result := GuardActions.NavigateTo(MakeNavigationTarget(LegacyVector(Position)), ?ReachRadius:=ReachRadius, ?AllowPartialPath:=AllowPartialPath )
        if (Result<>navigation_result.Reached):
            LogAndDisplayOnScreen("GoToPosition error")
            return false
        
        # Succeded in movement, so remove first target IF within 2* ReachRadius of it to give some fuzziness for partial paths
        DistanceToPosition:= (/Verse.org/SpatialMath:)Distance(ConvertVector(Character.GetTransform().Translation), Position)
        if:
            DistanceToPosition <= (ReachRadius * 2.0 )
            PositionsLeft:= TargetMovementPositions.Length
            PositionsLeft > 0
            RemovedList:= TargetMovementPositions.RemoveElement[0]
        then:
            set TargetMovementPositions = RemovedList
            LogAndDisplayOnScreen("Successfully reached position and removed from targets. DistancetoPos {DistanceToPosition}, Positions Remaining {PositionsLeft - 1}")

        return true


    LookAroundAndCheckMovementTask(Location:(/Verse.org/SpatialMath:)vector3)<suspends>:void=
        race:
            LookAroundTask(Location, 0.25, 0.75)
            CheckIfCharacterHasMoved(Location)

    LookAroundTask(Position:(/Verse.org/SpatialMath:)vector3, MinTime:float, MaxTime:float)<suspends>:void=
        LogAndDisplayOnScreen("Entering LookAroundTask")
        defer:
            LogAndDisplayOnScreen("Exiting LookAroundTask")

        RandomOffset := (/Verse.org/SpatialMath:)vector3{ Forward:=GetRandomFloat(-1000.0, 1000.0), Right:=GetRandomFloat(-1000.0, 1000.0), Up:=GetRandomFloat(50.0, 200.0)}
        race:
            GuardActions.Wait(?Duration:=GetRandomFloat(MinTime, MaxTime))
            GuardActions.MaintainFocus(LegacyVector(Position + RandomOffset))

    CheckIfCharacterHasMoved(Position:(/Verse.org/SpatialMath:)vector3)<suspends>:void=
        loop:
            if ((/UnrealEngine.com/Temporary/SpatialMath:)DistanceXY(LegacyVector(Position), Character.GetTransform().Translation) > 200.0):
                break
            Sleep(1.0)


    GoToAndAttackTask(TargetInfo:fort_target_info)<suspends>:void=     
        LogAndDisplayOnScreen("Entering GoToAndAttackTask")
        defer:
            LogAndDisplayOnScreen("Exiting GoToAndAttackTask")
        
        race:
            GuardActions.Attack(TargetInfo)
            GuardActions.MoveInRangeToAttack()
            
    LookAtTargetTask()<suspends>:void=     
        LogAndDisplayOnScreen("Entering LookAtTargetTask")
        defer:
            LogAndDisplayOnScreen("Exiting LookAtTargetTask")
    
        loop:
            if:
                TargetInfo:=GetTarget[]
                LastKnownTargetPosition:=GuardPerception.GetLastKnownPosition[TargetInfo.Target]
            then:
                LogAndDisplayOnScreen("LookingAtTarget")
                race:
                    GetNewTarget()
                    block:
                        GuardActions.MaintainFocus(LastKnownTargetPosition)
                        Sleep(1.0)
            else:
                GetNewTarget()


    # Return the current threat
    GetTarget()<decides><transacts>:fort_target_info=
        GuardPerception.GetAlertLevel() <> fort_guard_alert_level.Unaware
        GuardPerception.GetCurrentTargetInfo[]

    # Wait until a new threat is detected
    GetNewTarget()<suspends>:?fort_target_info=
        if:
            GuardPerception.GetAlertLevel() = fort_guard_alert_level.Unaware
        then:
            GuardPerception.AlertLevelChangedEvent.Await()
            return option{GuardPerception.GetCurrentTargetInfo[]}

        TargetInfo:=GuardPerception.CurrentTargetChangedEvent.Await();
        return option{TargetInfo};


    TacticalSprintService()<suspends>:void=
        GuardActions.TacticalSprint()

    # attack any target within this distance  
    ShootTargetService(MinDistance:float)<suspends>:void=
        LogAndDisplayOnScreen("Starting ShootTarget")
        set ShootTargetServiceActive = true
        defer:
            LogAndDisplayOnScreen("Exiting ShootTarget")
            set ShootTargetServiceActive = false

        Sleep(0.5)
        loop:
            if:
                TargetInfo:=GetTarget[]
                LastKnownTargetPosition:=ConvertVector(GuardPerception.GetLastKnownPosition[TargetInfo.Target])
            then:
                race:
                    GetNewTarget()
                    GuardPerception.AlertLevelChangedEvent.Await()
                    block:
                        if:
                            GuardPerception.GetAlertLevel() = fort_guard_alert_level.Alerted
                            GuardPerception.GetAlertLevel[TargetInfo.Target] = fort_guard_alert_level.Alerted
                        then:
                            LogAndDisplayOnScreen("Found ShootTarget:Target")
                            loop:
                                if ((/Verse.org/SpatialMath:)Distance(LastKnownTargetPosition, ConvertVector(Character.GetTransform().Translation)) < MinDistance):
                                    # stop navigation, insert last known pos as movement option and tell to move in range to attack
                                    # Reduces chance of NPCs runnint to patrol point while attacking
                                    var Result:ai_action_result = ai_action_result.Canceled
                                    GuardActions.StopNavigation()
                                    set TargetMovementPositions = array{LastKnownTargetPosition} + TargetMovementPositions

                                    race:
                                        GuardActions.MoveInRangeToAttack()
                                        set Result =GuardActions.Attack(TargetInfo)

                                    if (Result<>ai_action_result.Success):
                                        Sleep(Inf)
                                    else:
                                        Sleep(0.0)
                                else:
                                    Sleep(0.5)
                        else:
                            Sleep(Inf)
            else:
                GetNewTarget()

    # Debug: Log & Onscreen debugging with filterable NPCID
    LogAndDisplayOnScreen(name:string):void=
        if (AIBehaviorScript.DebugDisplay?):
            AIBehaviorScript.Logger.Print("NPC {DebugNpcID} - {name}")
            Print(name, ?Color:=AIBehaviorScript.DebugScreenColor, ?Duration:=AIBehaviorScript.DebugScreenDuration)