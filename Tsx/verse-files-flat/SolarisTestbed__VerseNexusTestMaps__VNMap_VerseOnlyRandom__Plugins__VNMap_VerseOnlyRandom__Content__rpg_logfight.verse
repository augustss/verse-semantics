
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }

dnd_logfight := class(creative_device):
    var PlayerCurrentHealth : int = 100
    var PlayerMinHealth : int = 0
    var PlayerMaxHealth : int = 100
    var PlayerAttackDamage : int = 0

    var EnemyAttackDamage : int = 0
    var EnemyCurrentHealth : int = 100
    var EnemyMinHealth : int = 0
    var EnemyMaxHealth : int = 100

    var isEnemyDead :logic = false
    var isPlayerDead : logic = false


    OnBegin<override>()<suspends>:void=
        SimulateFight()

    SimulateFight()<suspends>:void=
        loop:
            var EnemyDiceResult : int = GetRandomInt(1,20)
            var PlayerDiceResult : int = GetRandomInt(1,20)

            if (PlayerDiceResult > EnemyDiceResult and isEnemyDead = false and isPlayerDead = false):
                Print ("Player rolled a {PlayerDiceResult} while the Enemy rolled a {EnemyDiceResult}")
                Print ("Player successfully attacks")
                AttackEnemy()
            else:
                if (PlayerDiceResult = EnemyDiceResult and isEnemyDead = false and isPlayerDead = false):
                    Print ("Player rolled a {PlayerDiceResult} while the Enemy also rolled a {EnemyDiceResult}")
                    Print ("No one successfully attacks")
                else:
                    Print ("Player rolled a {PlayerDiceResult} while the Enemy rolled a {EnemyDiceResult}")
                    Print ("Enemy successfully attacks")
                    AttackPlayer()
            if (isPlayerDead = true):
                break
                
            if (isEnemyDead = true):
                break


    ClampPlayerHealth(DamageAmount: int):int=
        set PlayerCurrentHealth = Clamp(PlayerCurrentHealth, PlayerMinHealth, PlayerMaxHealth) - DamageAmount

        return PlayerCurrentHealth

    ClampEnemyHealth(DamageAmount: int):int=
        set EnemyCurrentHealth = Clamp(EnemyCurrentHealth, EnemyMinHealth, EnemyMaxHealth) - DamageAmount

        return EnemyCurrentHealth

    AttackEnemy():void=
        set PlayerAttackDamage = GetRandomInt(1, 20)

        ClampEnemyHealth(PlayerAttackDamage)

        if (PlayerCurrentHealth <= 0):
            Print ("Enemy has died! \n PlayerCurrentHealth was: {PlayerCurrentHealth}")
            set isEnemyDead = true
        else:
            if (PlayerAttackDamage >= 20):
                Print ("Enemy was hit with a heavy attack! \n PlayerAttackDamage was: {PlayerAttackDamage}")

            else:
                if (PlayerAttackDamage >= 10):
                    Print ("Enemy was hit with a medium attack! \n PlayerAttackDamage was: {PlayerAttackDamage}")
                    
                else:
                    Print ("Enemy was hit with a light attack! \n PlayerAttackDamage was: {PlayerAttackDamage}")

        ClampEnemyHealth(PlayerAttackDamage)

        if (EnemyCurrentHealth <= 0):
            Print ("Enemy has died! \n EnemyCurrentHealth was: {EnemyCurrentHealth}")
            set isEnemyDead = true

    AttackPlayer():void=
        set EnemyAttackDamage = GetRandomInt(1, 20)

        ClampPlayerHealth(EnemyAttackDamage)

        if (PlayerCurrentHealth <= 0):
            Print ("Player has died! \n PlayerCurrentHealth was: {PlayerCurrentHealth}")
            set isPlayerDead = true
        else:
            if (EnemyAttackDamage >= 20):
                Print ("Player was hit with a heavy attack! \n EnemyAttackDamage was: {EnemyAttackDamage}")

            else:
                if (EnemyAttackDamage >= 10):
                    Print ("Player was hit with a medium attack! \n EnemyAttackDamage was: {EnemyAttackDamage}")
                    
                else:
                    Print ("Player was hit with a light attack! \n EnemyAttackDamage was: {EnemyAttackDamage}")
        
        ClampPlayerHealth(EnemyAttackDamage)

        if (PlayerCurrentHealth <= 0):
            Print ("Player has died! \n PlayerCurrentHealth was: {PlayerCurrentHealth}")
            set isPlayerDead = true
