using { /Fortnite.com/AI }
using { /Fortnite.com/Teams }
using { /Fortnite.com/Characters}
using { /Fortnite.com/Game}
using { /Fortnite.com/Playspaces}
using { /Verse.org/Simulation }
using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
 
# Getting started: https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/Verse/onboarding-guide-to-programming-with-verse-in-unreal-editor-for-fortnite

# little component for cube killing
shape_death_component := class<final_super>(component):
    
    var FortDamage:?fort_damageable_component = false
    var DamageEventHandle:?cancelable = false
    
    OnSimulate<override>()<suspends>:void=
        if:
            FortDamageComponent := Entity.GetComponent[fort_damageable_component]
        then:
            if (Handle := DamageEventHandle?):
                Handle.Cancel()
            set DamageEventHandle = option{FortDamageComponent.DamagedEvent.Subscribe(OnTakeDamage)}
            set FortDamage = option{FortDamageComponent}
    
    OnTakeDamage(DamageAmount:float):void=
        Print("Shape took {DamageAmount} damage")
        if:
            Damage:= FortDamage?
            Damage.Health <= 0.0
        then:
            Print("Killing Shape!")
            SubEntities := Entity.GetEntities()
            for (SubEntity:SubEntities):
                Components := SubEntity.GetComponents()
                for (TheComponent:Components):
                    if (EnableableComponent := enableable[TheComponent]):
                        EnableableComponent.Disable()

            Entity.RemoveFromParent()
                
    



# A Verse-authored NPC Behavior that can be used within an NPC Character Definition or an NPC Spawner device's NPC Behavior Script Override.
just_attack_behaviour<public> := class(npc_behavior):

    ToString(TargetInfo:fort_target_info):string=
        LOS:= if(TargetInfo.HasLineOfSight?) then "true" else "false"
        Attitude:= if(TargetInfo.Attitude <> team_attitude.Hostile) then "Friendly or neutral" else "Hostile"
        
        LogEntityFeaturesOfInterest(TargetInfo.Target, "")
        return "HasLineOfSight = {LOS}, Attitude {Attitude}"

    # Super basic loop, "Can I see a target? If so, attack the first one, re-evaluate in 5 seconds"
    OnBegin<override>()<suspends>:void=
        if:
            Agent:= GetAgent[]
            CharacterRef:= Agent.GetFortCharacter[]
            EntityRef:= CharacterRef.GetEntity[]
            Guard_Actions:=EntityRef.GetComponent[fort_guard_actions_component]
            Perception:=EntityRef.GetComponent[fort_guard_perception_component]
        then:
            loop:
                Targets:= Perception.DetectedTargets
                PrintDebuggingInfo(Targets, EntityRef, Perception)

                if:
                    Targets.Length > 0
                then:
                    TargetInfoOption:= GetClosestTarget()
                    if(TargetInfo:= TargetInfoOption?):
                        Print("Attacking! {TargetInfo}")
                        spawn:
                            Guard_Actions.Attack(TargetInfo)
                Sleep(5.0)

    GetClosestTarget():?fort_target_info=
        if:
            Agent:= GetAgent[]
            CharacterRef:= Agent.GetFortCharacter[]
            AgentTransform:= CharacterRef.GetTransform()
            EntityRef:= CharacterRef.GetEntity[]
            Perception:=EntityRef.GetComponent[fort_guard_perception_component]
            Targets:= Perception.DetectedTargets
            TargetCount:= Targets.Length
            TargetCount > 0
        then:
            var Closest:float = 1000000.0
            var ClosestTargetInfo:?fort_target_info = false
            for(TargetInfo:Targets):
                Print("TargetInfo is {TargetInfo}")
                if(TargetInfo.Target.GetComponent[ai_target_component]):
                    Print("Found Target with AI target component, should be a tower")
                
                if(TargetInfo.Target.GetComponent[mesh_component]):
                    Print("Found Target with Static Mesh, should be a tower")

                if(TargetInfo.Attitude = team_attitude.Hostile):
                    if(TargetPosition:= Perception.GetLastKnownPosition[TargetInfo.Target]):
                        TargetPos := ConvertVector(TargetPosition)
                        AgentPos := ConvertVector(AgentTransform.Translation)
                        DistanceToTarget:=  (/Verse.org/SpatialMath:)Distance(AgentPos, TargetPos)
                        Print("Target Last known pos is: {TargetPos}")
                        if(DistanceToTarget <= Closest):
                            set Closest = DistanceToTarget
                            set ClosestTargetInfo = option{TargetInfo}
                else :
                    

            return ClosestTargetInfo
        return false
            
    LogEntityFeaturesOfInterest(TargetEntity:entity, Prefix:string):void=
        if (PhysicsComp := TargetEntity.GetComponent[physics_component]):
            if(IsEnabled:= PhysicsComp.IsEnabled[]):
                Print("{Prefix} Physics Component is enabled")
            else:
                Print("{Prefix} Physics Component is NOT enabled")

        if(TransformTarget:= TargetEntity.GetComponent[transform_component], Agent:= GetAgent[], Character:=Agent.GetFortCharacter[], Location:= Character.GetTransform().Translation):
            LPosition := ConvertVector(Location)
            DistToTarget:= (/Verse.org/SpatialMath:)Distance(LPosition, TransformTarget.GetGlobalTransform().Translation)/100.0
            Print("{Prefix} Target Distance is {DistToTarget}m")
            if(SelfEntity:= Character.GetEntity[], TargetEntity = SelfEntity):
                Print("{Prefix} Target is Self") # this actually happened in testing at least once, appears to be from splash damage from towers

        if(ParentEntity:= TargetEntity.GetParent[]):
            Print("{Prefix} Checking Target Parent(s)")
            LongerPrefix:= "   "+ Prefix
            LogEntityFeaturesOfInterest(ParentEntity, LongerPrefix)

    PrintDebuggingInfo(PotentialTargets:[]fort_target_info, NPCEntity:entity, Perception:fort_guard_perception_component):void=
        Print("There are {PotentialTargets.Length} targets I can detect")
        if:
            PotentialTargets.Length = 0
            Playspace:= NPCEntity.GetPlayspaceForEntity[]
            TeamCollection:= Playspace.GetTeamCollection()
            SimulationEntity := NPCEntity.GetSimulationEntity[]
        then:
            var Count:int = 0

                for(Team:TeamCollection.GetTeams()):
                    set Count= Count +1

                    if(Agents:= TeamCollection.GetAgents[Team]):
                        Print("There are {Agents.Length} agents on Team {Count}")
    
                    if(TeamEntities:= TeamCollection.GetEntities[Team]):
                        Print("There are {TeamEntities.Length} entities on Team {Count}")
                        for(TeamEntity:TeamEntities):
                            var FoundEntity:logic = false
                            for (Target:Perception.DetectedTargets):
                                if (TeamEntity = Target.Target):
                                    Print(ToString(Target))
                            if (FoundEntity = false):
                                Print("No Detected target info on that Entity")
                
                TeamCompPossessors:=SimulationEntity.FindDescendantEntitiesWithComponent(team_component)
                var TeamCompCount:int = 0
                for(TeamCompEnt:TeamCompPossessors):
                    set TeamCompCount = TeamCompCount + 1
                
                Print("There are {TeamCompCount} entities with the team_component")

    # This function runs when the NPC is despawned or eliminated from the world.
    OnEnd<override>():void=
        # TODO: Replace this with your code
        Print("Goodbye, NPC!")