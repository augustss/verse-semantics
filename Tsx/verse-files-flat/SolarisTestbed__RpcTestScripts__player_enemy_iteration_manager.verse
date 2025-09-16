using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }

player_enemy_iteration_manager := class(creative_device):
    var PlayerHealth : int = 100
    var EnemyHealth : int = 100
    var AttackDamage : int = 5
    
    OnBegin<override>()<suspends>:void=
        var CombatIteration : int = 0
        loop:
            set CombatIteration = CombatIteration + 1
            Print ("Loop Iteration: {CombatIteration}")
            var PlayerDiceResult : int = GetRandomInt(1,20)
            var EnemyDiceResult : int = GetRandomInt(1,20)
            Print ("Player Dice: {PlayerDiceResult}")
            Print ("Enemy Dice: {EnemyDiceResult}")

            if (EnemyDiceResult > PlayerDiceResult):
                set PlayerHealth = PlayerHealth - AttackDamage
                Print ("Enemy attack successful! Player takes {AttackDamage} damage to their HP!")
            else if (EnemyDiceResult = PlayerDiceResult):
                set PlayerHealth = PlayerHealth - 1
                set EnemyHealth = EnemyHealth - 1
                Print ("Roll was a tie. Player and Enemy lose 1 HP")
            else:
                set EnemyHealth = EnemyHealth - AttackDamage
                Print ("Player attack successful! The enemy takes {AttackDamage} damage to their HP!")
            if (PlayerHealth <= 0):
                Print ("Player has died")
                break
            else if (EnemyHealth <= 0):
                Print("Enemy has died")
                break

        Print ("The Game has ended!")