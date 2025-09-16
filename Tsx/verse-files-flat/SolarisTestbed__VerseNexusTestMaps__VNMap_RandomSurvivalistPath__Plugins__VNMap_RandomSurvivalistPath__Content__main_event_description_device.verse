
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Game }

# A Verse-authored creative device that can be placed in a level
main_event_description_device := class(creative_device):

    @editable
    CreatureSpawner : creature_spawner_device = creature_spawner_device{}

    @editable
    NpcSpawnerDevice : npc_spawner_device = npc_spawner_device{}

    @editable
    CampfireDevice : campfire_device = campfire_device{}

    @editable
    ExplosiveDevice : explosive_device = explosive_device{}

    @editable
    PersistenceWidgetDevice : persistence_widget_device = persistence_widget_device{}

    ObjectsNearCharacter<private> : objects_near_character = objects_near_character{}

    NpcBehavior : new_npc_behavior = new_npc_behavior{}
    RandomEventCallerClass : random_event_caller = random_event_caller{}
    var CancelEvent<private> : ?cancelable = false
    var _Player : player = player{}
    var CurrentCharacterHealth : float = 100.0
    var CurrentCreatureHealth : float = 100.0

    OnBegin<override>()<suspends>:void=
        Print("OnBegin - main_event_description_device")
        CampfireDevice.LitEvent.Subscribe(OnCampfireAgentEvent)
        CampfireDevice.AgentPulsedEvent.Subscribe(OnCampfireAgentEvent)
        CampfireDevice.AgentEntersEffectAreaEvent.Subscribe(OnCampfireAgentEvent)
        if (MaybePlayer := GetPlayspace().GetPlayers()[0]):
            set _Player = MaybePlayer
            if (FortCharacter := _Player.GetFortCharacter[]):
                set CurrentCharacterHealth = FortCharacter.GetHealth()
                FortCharacter.DamagedEvent().Subscribe(OnDamagedEvent)
                FortCharacter.HealedEvent().Subscribe(OnHealedEvent)

        CreatureSpawner.Disable()
        NpcSpawnerDevice.Disable()

    OnCampfireAgentEvent(Agent : agent):void=
        if:
            NpcAgent := NpcBehavior.GetAgent[]
            Agent <> _Player
            Agent <> NpcAgent
        then:
            Print("NexusError - _Player or npc isn't user of campfire which isn't expected in main_event_description_device")

    TeleportEventObjectsToPlayer(CreativeObject : creative_object):void=
        Print("NexusInfo - teleporting CreativeObject to player in main_event_description_device")
        spawn{ObjectsNearCharacter.TeleportObjectToPlayer(CreativeObject, _Player)}
        
 ###################### Fort char block  ########################################## 
    OnHealedEvent(HealingResult : healing_result):void=
        if (FortCharacter := _Player.GetFortCharacter[]):
            if (CurrentCharacterHealth > FortCharacter.GetHealth() and FortCharacter.GetHealth() <= FortCharacter.GetMaxHealth()):
                Print("NexusError - inappropriate healting applied on player in main_event_description_device. Current health: {FortCharacter.GetHealth()}, ExpectedHealth: {(CurrentCharacterHealth)}")

            if(Instigator := HealingResult.Instigator?, _Player = Instigator.GetInstigatorAgent[]):
                Print("NexusError - FortCharacter is Instigator in main_event_description_device")
            else:
                Print("NexusInfo - FortCharacter isn't Instigator for healing")
            
            set CurrentCharacterHealth = FortCharacter.GetHealth()    

    OnDamagedEvent(DamageResult : damage_result):void=
        if (FortCharacter := _Player.GetFortCharacter[]):
            ExpectedHealth := CurrentCharacterHealth - DamageResult.Amount
            if (((ExpectedHealth) <> FortCharacter.GetHealth()) and (FortCharacter.GetHealth() > 0.0) and (ExpectedHealth > 0.0)):
                Print("NexusError - inappropriate damage applied on player in main_event_description_device. Current health: {FortCharacter.GetHealth()}, ExpectedHealth: {ExpectedHealth}, Damage: {DamageResult.Amount}")

            if(Instigator := DamageResult.Instigator?, _Player = Instigator.GetInstigatorAgent[]):
                Print("NexusError - FortCharacter is Instigator in main_event_description_device")
            else:
                Print("NexusInfo - FortCharacter isn't Instigator for elimination")

            set CurrentCharacterHealth = FortCharacter.GetHealth()    

 ###################### explode all block  ########################################## 
    BombAllSuspends()<suspends>:void=
        Print("NexusInfo - bomb event called!")
        PersistenceWidgetDevice.MakeEventCall("BombAll")
        AttackOnThePlayerCamp()
        SpawnPeacefulNpc()
        loop:
            ExplosiveDevice.Hide()
            ObjectsNearCharacter.TeleportObjectNearToPlayer(ExplosiveDevice, _Player)
            ExplosiveDevice.Explode(_Player)
            ExplosiveDevice.Reset()
            if (FalseCondition := Shuffle(true, false), FalseCondition[0]?):
                break
            Sleep(0.1)

    BombAll():void=
        spawn{BombAllSuspends()}

    ############# Creature spawner block ##############        
    AttackOnThePlayerCamp<private>():void=
        Print("NexusInfo - camp attacked from main_event_description_device!")
        spawn{PersistenceWidgetDevice.MakeEventCall("AttackOnThePlayerCamp")}
        CreatureSpawner.SpawnedEvent.Subscribe(OnCreatureSpawnedEvent)
        CreatureSpawner.EliminatedEvent.Subscribe(CompareWithCharacter)
        TeleportEventObjectsToPlayer(CreatureSpawner)
        CreatureSpawner.Enable()
        
    OnCreatureSpawnedEvent(Agent : agent):void=
        if (FortCharacter := Agent.GetFortCharacter[]):
            CurrentHealth := FortCharacter.GetHealth()
            CancelVariable := FortCharacter.DamagedEvent().SubscribeToDamageResultWithType(OnCreatureDamagedEvent, (FortCharacter, CurrentHealth))
            set CancelEvent = option{CancelVariable}

    OnCreatureDamagedEvent(DamageResult : damage_result, CharacterComponent : tuple(fort_character, float)):void=
        #if (DamageResult.Target <> FortCharacter):
            #Print("NexusError - inappropriate creature caused event")

        if:
            CharacterComponent(1) - DamageResult.Amount <> CharacterComponent(0).GetHealth()
            CharacterComponent(0).GetHealth() > 0.0
        then:
            Print("NexusError - inappropriate damage applied on creature in main_event_description_device. Current health: {CharacterComponent(0).GetHealth()}, ExpectedHealth: {(CurrentCharacterHealth - DamageResult.Amount)}, Damage: {DamageResult.Amount}")
        if (EventCancel := CancelEvent?):
            Print("NexusInfo - OnCreatureDamagedEvent canceled")
            EventCancel.Cancel()

    CompareWithCharacter(DeviceAIInteractionResult : device_ai_interaction_result):void=
        if (_Player = DeviceAIInteractionResult.Target?):
            Print("NexusError - FortCharacter and DeviceAIInteractionResult.Target is same in main_event_description_device")
        else if(_Player = DeviceAIInteractionResult.Source?):
            Print("NexusError - FortCharacter and DeviceAIInteractionResult.Source is same in main_event_description_device")
        else:
            Print("NexusInfo - FortCharacter isn't responsible for elimination")
                    
    ############# All events call block ##############        
    AllEventsCall():void=
        Print("NexusInfo - all events start to execute in main_event_description_device")
        spawn{PersistenceWidgetDevice.MakeEventCall("AllEventsCall")}
        spawn{AllEventsCallSuspends()}
        
    AllEventsCallSuspends()<suspends>:void=
        loop:
            RandomEventCallerClass.CallRandomEvent(RunRandomizedEvent, ())
            Sleep(0.1)
            if (FalseCondition := Shuffle(true, false), FalseCondition[0]?):
                break

    ############# npc spawner block ##############        
    SpawnPeacefulNpc<private>():void=
        Print("NexusInfo - peaceful spawn of npc from main_event_description_device!")
        spawn{PersistenceWidgetDevice.MakeEventCall("SpawnPeacefulNpc")}
        TeleportEventObjectsToPlayer(NpcSpawnerDevice)
        NpcSpawnerDevice.Enable()
        NpcSpawnerDevice.Spawn()

    ############# random block ##############        
    EmptyFunction<private>():void= 
        Print("NexusError - RunRandomizedEvent return this in main_event_description_device")

    RunRandomizedEvent():void=
        Print("NexusInfo - call of RunRandomizedEvent in main_event_description_device")
        #AllEventsCall
        RandomizedArray := Shuffle(array{AttackOnThePlayerCamp, SpawnPeacefulNpc})
        if (RandomEvent := RandomizedArray[0]):
            RandomEvent()    
        else:
            EmptyFunction()
