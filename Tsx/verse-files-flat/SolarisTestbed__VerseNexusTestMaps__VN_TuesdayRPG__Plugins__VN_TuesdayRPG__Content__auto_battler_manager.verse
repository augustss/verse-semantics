
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/SpatialMath }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

fizz_buzz := class:


    RunFizzBuzz<public>(MaxInt:int)<suspends>:string=

        Print("FizzBuzz initiated with maximum int = {MaxInt}")

        var FizzBuzzResultString:string = ""

        for(IndexInt := 1..MaxInt)
        {
            var FizzString:string = ""

            if(ModIndex:= Mod[IndexInt,3]=0)
            {
        set FizzString += "Fizz"
            }
            
            if(ModIndex:= Mod[IndexInt,5]=0):

                set FizzString += "Buzz"


            if(ModIndexMod3:= Mod[IndexInt,3]<>0, 
            ModIndexMod5:=Mod[IndexInt,5]<>0):
                set FizzString = ToString(IndexInt)
            

            set FizzBuzzResultString += " {FizzString}"

            Sleep(0.1)
        }

     return FizzBuzzResultString


MinMax := struct<computes><concrete>
{
    @editable
    Minimum:int = 0 

    @editable
    Maximum:int = 0
}

character_moves := interface:

    Attack():void

    Attack(Char:character_data):void

    Attack(Char:character_data, DamageToTake:float):void

    TakeDamage<public>(DamageToTake:float)<transacts><decides>:void

    Die():void

PlayerDataConstructor<constructor>(AttackTime:float,
AttackPower:float, 
Defense:float, 
MaximumHealth:float,
CurrentHealth:float):= ally_data:
    AttackTime:=AttackTime
    AttackPower:=AttackPower
    Defense:=Defense
    MaximumHealth:=MaximumHealth
    CurrentHealth:=CurrentHealth


EnemyDataConstructor<constructor>(AttackTime:float,
AttackPower:float, 
Defense:float, 
MaximumHealth:float,
CurrentHealth:float):= enemy_data:
    AttackTime:=AttackTime
    AttackPower:=AttackPower
    Defense:=Defense
    MaximumHealth:=MaximumHealth
    CurrentHealth:=CurrentHealth

character_data := class<concrete><reads><allocates>(character_moves):
    
    var AttackTime : float = 0.5
    var AttackPower : float = 5.0
    var Defense : float = 5.0
    var MaximumHealth : float = 100.0
    var CurrentHealth : float = 100.0

    var XP :int= 0
    
    TakeDamage<override>(DamageToTake:float)<transacts><decides>:void=
        set CurrentHealth -= DamageToTake
        if(CurrentHealth>=0.0):
            Die

    GetHealth()<suspends>:float=
        CurrentHealth 

    GetMaxHealth()<suspends>:float=
        MaximumHealth 

    Die<override>():void=
        Print("Player Dead")

    Attack<override>():void=
        Print("PlayerAttack")
        
    Attack<override>(Char:character_data):void=
        Print("Character attack")

    Attack<override>(Char:character_data, DamageToTake:float):void=
        Print("Character attack: {DamageToTake}")
    
    GetAttackPower<public>():float=
        return AttackPower

    RandomizeAttack(AttackFloat:float):float=
        return GetRandomFloat(AttackFloat*0.75,1.25)

ally_data := class<concrete><reads><allocates>(character_data):

    Attack<override>():void=
        Print("Player attacks!")

    
    Attack<override>(Char:character_data):void=
        RandomAttackPower:=RandomizeAttack(AttackPower)
        if(Char.TakeDamage[RandomAttackPower]):

enemy_data := class<concrete><reads><allocates>(character_data):

    Attack<override>(Char:character_data):void=
        RandomAttackPower:=RandomizeAttack(AttackPower)
        if(Char.TakeDamage[RandomAttackPower]):

# stat_button_connecter := class<abstract>:

pop_up_damage<public> := struct<concrete>:
    @editable
    DamageBB:billboard_device = billboard_device{}

    @editable
    DamageSequence:cinematic_sequence_device=cinematic_sequence_device{}

auto_battler_manager := class<final>(creative_device):

    var NewTask: void = void

    HealthMessage<localizes>(CurrentHealth:float, MaxHealth:float):message = "HP: {CurrentHealth}/{MaxHealth}"

    HealthMessage<localizes>(DamageTakenInt:int):message = "-{DamageTakenInt}"

    FizzBuzz :fizz_buzz=fizz_buzz{}


    @editable
    FizzBuzzRandomMinMax:MinMax = MinMax{}

    var Player:ally_data = ally_data{}

    var Enemy:enemy_data = enemy_data{}

    @editable
    PlayerHealthText:billboard_device=billboard_device{}


    @editable
    EnemyHealthText:billboard_device=billboard_device{}

    @editable
    PlayerLight:customizable_light_device=customizable_light_device{}

    @editable
    EnemyLight:customizable_light_device=customizable_light_device{}
    #var BattleTask : any = any

    var BattleIsOver:logic = false

    # Stat points reflect the amount of HP/stats the ally/enemy are allocated
    @editable
    PlayerStartingStatPoints:int = 30

    @editable
    EnemyStartingStatPoints:int = 20

    # Prop Stuff
    @editable
    AllyProp:creative_prop = creative_prop{}

    @editable
    EnemyProp:creative_prop = creative_prop{}

    var AllyPropVector:vector3 = vector3{}
    var EnemyPropVector:vector3 = vector3{}

    @editable
    PropMovementDistance:float = 5.0
    @editable
    PropMovementFrequency:float = 0.0
    @editable
    PropMaxMovementDistance:float = 250.0

    @editable
    AllyVFXSpawner:vfx_creator_device=vfx_creator_device{}

    @editable
    EnemyVFXSpawner:vfx_creator_device=vfx_creator_device{}

    @editable
    AllyPopUpDamage:pop_up_damage=pop_up_damage{}

    @editable
    EnemyPopUpDamage:pop_up_damage=pop_up_damage{}
    
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - auto_battler_manager")

        # Initialize Prop Vectors
        set AllyPropVector = NewAllyPropVector := AllyProp.GetTransform().Translation
        set EnemyPropVector = NewEnemyPropVector := EnemyProp.GetTransform().Translation
        EnemyPopUpDamage.DamageBB.HideText()
        AllyPopUpDamage.DamageBB.HideText()

        block:
            branch{ 
                NewPlayerStats:=RandomizedStats(PlayerStartingStatPoints)
                # Setting player
                NewPlayer :ally_data= CreatePlayerData(2.0,10.0,10.0,250.0,250.0)
                # NewPlayer :ally_data= CreatePlayerData(NewPlayerStats(0),NewPlayerStats(1),NewPlayerStats(2),NewPlayerStats(3),NewPlayerStats(4))
                set Player = NewPlayer
                # Setting Enemy
                # NewEnemy :enemy_data= CreateEnemyData(RandomizedStats(EnemyStartingStatPoints))
                NewEnemy :enemy_data= CreateEnemyData(3.0,10.0,10.0,250.0,250.0)
                set Enemy = NewEnemy
                Sleep(0.5)
                BeginAutoBattle()

            }

            loop:
                MaxFizzBuzzes:=GetRandomInt(FizzBuzzRandomMinMax.Minimum,FizzBuzzRandomMinMax.Maximum)
                Print(FString:=FizzBuzz.RunFizzBuzz(MaxFizzBuzzes))

                #Beginning battle

    OnEnd<override>():void=
        Print("Nexus OnEnd - auto_battler_manager")
            
    PlayerAutoAttackBegin()<suspends>:void=

        if(BattleIsOver = true){return}

        OldEnemyHealth:= Enemy.GetHealth()
        Sleep(RandomizeAttack(Player.AttackTime))
        PlayerLight.DimLight()

        MoveTask:= spawn{MoveToOpponent(AllyProp)}
        MoveTask.Await()

        Player.Attack(Enemy)
        EnemyVFXSpawner.Begin()

        
        EnemyDamageTaken:=OldEnemyHealth-Enemy.GetHealth()
        if(EnemyDamageTakenInt:=Ceil[EnemyDamageTaken]):
            StartPopUpDamage(EnemyPopUpDamage,EnemyDamageTakenInt)
        SetEnemyHealthText(Enemy.CurrentHealth, EnemyDamageTaken)

        # Pop up damage
        ReturnTask:= spawn{MoveToOriginalSpot(AllyProp)}
        ReturnTask.Await()

        PlayerLight.UndimLight()

        if(Enemy.CurrentHealth <=0.0):
            EndBattle(true)

    EnemyAutoAttackBegin()<suspends>:void=

        if(BattleIsOver = true){return}

        OldPlayerHealth:=Player.GetHealth()
        Sleep(RandomizeAttack(Enemy.AttackTime))
        EnemyLight.DimLight()

        # Move towards player, attack, then move back
        ReturnTask:= spawn{MoveToOriginalSpot(EnemyProp)}
        ReturnTask.Await()

        Enemy.Attack(Player)
        AllyVFXSpawner.Begin()
        PlayerDamageTaken:=OldPlayerHealth-Player.GetHealth()
              # Pop up damage
        if(PlayerDamageTakenInt:=Ceil[PlayerDamageTaken]):
            StartPopUpDamage(AllyPopUpDamage,PlayerDamageTakenInt)

        SetPlayerHealthText(Player.CurrentHealth,PlayerDamageTaken)

  
        MoveTask:= spawn{MoveToOpponent(EnemyProp)}
        MoveTask.Await()
   
        EnemyLight.UndimLight()

  
        # DamageToTake: float = RandomizeAttack(Player.GetAttackPower())
        # if(Enemy.TakeDamage[DamageToTake]):
        #     SetEnemyHealthText(Enemy.CurrentHealth,DamageToTake)
        #     Print("Enemy took {DamageToTake} damage")

        if(Player.CurrentHealth <=0.0):
            EndBattle(false)

    BeginAutoBattle()<suspends>:void=

        Print("Auto Battle Began")
        SetPlayerHealthText(Player.GetHealth(),0.0)
        SetEnemyHealthText(Enemy.GetHealth(), 0.0)
        loop:
            sync:
                PlayerAutoAttackBegin()
                EnemyAutoAttackBegin()
            if(BattleIsOver = true){break}


    SetPlayerHealthText<protected>(CurrentHealth:float, DamageTaken:float)<suspends>:void=
        sync:
            Sleep(0.1)
            if(MaxHealthFloat:=Ceil[Player.MaximumHealth] * 1.0,
            DamageTakenRoundedUp:=  Ceil[DamageTaken] * 1.0,
            CurrentHealthRoundedUp:= Ceil[CurrentHealth] * 1.0):
                SetHealthTextsIncrementally(PlayerHealthText,CurrentHealthRoundedUp,MaxHealthFloat,DamageTakenRoundedUp)
    
    SetEnemyHealthText<internal>(CurrentHealth:float, DamageTaken:float)<suspends>:void=
        sync:
            Sleep(0.1)
            if(MaxHealthFloat:= Ceil[Enemy.MaximumHealth] * 1.0,
            DamageTakenRoundedUp:=  Ceil[DamageTaken] * 1.0,
            CurrentHealthRoundedUp:= Ceil[CurrentHealth] * 1.0):
                SetHealthTextsIncrementally(EnemyHealthText,CurrentHealthRoundedUp,MaxHealthFloat,DamageTakenRoundedUp)
        
    SetHealthTextsIncrementally(BBDevice:billboard_device, CurrentHealth:float, MaxHealth:float, DamageTaken:float)<suspends>:void=

        var CurrentHealthTakenIndex:float = 0.0
        loop:
            BBDevice.SetText(HealthMessage(CurrentHealth-CurrentHealthTakenIndex, MaxHealth))
            set CurrentHealthTakenIndex += 1.0

            if(CurrentHealthTakenIndex >DamageTaken or CurrentHealth-CurrentHealthTakenIndex <= 0.0){break}

            Sleep(0.1)

    CreatePlayerData<private>(AttackTime:float, 
    AttackPower:float, 
    Defense:float, 
    MaximumHealth:float, 
    CurrentHealth:float):ally_data=
        NewCharacterData := PlayerDataConstructor(AttackTime,AttackPower,Defense,MaximumHealth,CurrentHealth)

        return NewCharacterData

    CreateEnemyData(AttackTime:float, 
    AttackPower:float, 
    Defense:float, 
    MaximumHealth:float, 
    CurrentHealth:float):enemy_data=
        NewCharacterData := EnemyDataConstructor(AttackTime,AttackPower,Defense,MaximumHealth,CurrentHealth)

        return NewCharacterData


    UpdateHealthLightColor(LightDevice:customizable_light_device,CurrentHealth:float,MaxHealth:float):void=
        if(HealthPercentage:=GetPercentage[CurrentHealth,MaxHealth]):


    GetPercentage(Numerator:float,Denominator:float)<transacts><decides>:float=
        return Numerator/Denominator

    EndBattle(DidPlayerWin:logic):void=
        set BattleIsOver = true
        if(DidPlayerWin = true):
            # Player Win
        else:
            # Enemy Win
    LogicAsString(Logic:logic):string=if(Logic=true){ return "true" }else{ return "false"}

    RandomizedStats(StatPointsToAllocate:int):tuple(float,float,float,float,float)=

        var StatPointsToAllocateFloat:float= StatPointsToAllocate*1.0
        var StatPointsToAllocateInt :int= StatPointsToAllocate

        # Initialize the Stat Tuple 
        StatTupleToReturn:tuple(float,float,float,float,float)={
            0.0, # Attack Time
            0.0, # Attack Power
            0.0, # Defense
            0.0, # Max Health
            0.0  # CurrentHealth
        }
        
        AttackTimeFloat : float = StatPointsToAllocateFloat / GetRandomFloat(10.0,12.5)
        AttackPowerFloat : float = set StatPointsToAllocateFloat = StatPointsToAllocateFloat - GetRandomFloat(StatPointsToAllocateFloat/4.0,StatPointsToAllocateFloat/2.0)
        DefenseFloat : float = set StatPointsToAllocateFloat = StatPointsToAllocateFloat - GetRandomFloat(StatPointsToAllocateFloat/4.0,StatPointsToAllocateFloat/2.0)
        MaxHealthFloat : float =  StatPointsToAllocateFloat + GetRandomFloat(StatPointsToAllocate*1.0,StatPointsToAllocate*1.25)
        
        # tuple with a set amount of elements is still considered decides?
        if(StatTupleToReturn(0) = AttackTimeFloat,
        StatTupleToReturn(1) = AttackPowerFloat,
        StatTupleToReturn(2) = DefenseFloat,
        StatTupleToReturn(3) = MaxHealthFloat,
        StatTupleToReturn(4) = MaxHealthFloat):

        NewAttackTime: float = GetRandomFloat(0.0,0.1)

        return StatTupleToReturn


    RandomizeAttack(AttackFloat:float):float=
        return GetRandomFloat(AttackFloat*0.75,1.25)

    MoveToOpponent(Prop:creative_prop)<suspends>:void=

        ZeroRotation : rotation = Prop.GetTransform().Rotation
        var DistanceMovedFloat:float = 0.0
        # Move towards opponent
        loop:
            NewLocation:vector3=AddXToVector(Prop.GetTransform().Translation, PropMovementDistance)
            Prop.MoveTo(NewLocation, ZeroRotation, 0.01)
            set DistanceMovedFloat += PropMovementDistance
            # if(Prop.TeleportTo[NewLocation, ZeroRotation]):
            #     set DistanceMovedFloat += PropMovementDistance

            if(DistanceMovedFloat>=PropMaxMovementDistance){break}

            Sleep(PropMovementFrequency)


    MoveToOriginalSpot(Prop:creative_prop)<suspends>:void=

        ZeroRotation : rotation =  Prop.GetTransform().Rotation
        var DistanceMovedFloat:float = PropMaxMovementDistance

        # Move to original spot
        loop:
            NewLocation:vector3=AddXToVector(Prop.GetTransform().Translation, -PropMovementDistance)

            Prop.MoveTo(NewLocation, ZeroRotation, 0.01)
            set DistanceMovedFloat -= PropMovementDistance
            # if(Prop.TeleportTo[NewLocation, ZeroRotation]):
            #     set DistanceMovedFloat -= PropMovementDistance

            if(0.0>=DistanceMovedFloat){break}

            Sleep(PropMovementFrequency)

    AddXToVector(PropPosition:vector3,XFloatToAdd:float):vector3=
        return PropPosition + vector3{X:=XFloatToAdd,Y:=0.0,Z:=0.0}

    StartPopUpDamage(PopUpDamage:pop_up_damage, PopUpDamageInt:int)<suspends>:void=
        PopUpDamage.DamageBB.ShowText()
        PopUpDamage.DamageBB.SetText(HealthMessage(PopUpDamageInt))
        PopUpDamage.DamageSequence.Play()
        Sleep(0.5)
        PopUpDamage.DamageBB.HideText()
