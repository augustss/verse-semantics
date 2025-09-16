using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }

attack_function_manager := class(creative_device):
    var PlayerHealth : int = 100
    var EnemyHealth : int = 1

    OnBegin<override>()<suspends>:void=
        StartGame()

    StartGame() : void =
        loop:
            AttackEnemy()
            EnemyAttackPlayer()
            var Fighting : logic = IsBattleInProgress()
            if (Fighting = false):
                break
    
    AttackEnemy() : void =
        set EnemyHealth -= GetRandomInt(1,10)
        Print("Enemy Health: {EnemyHealth}")

    EnemyAttackPlayer() : void =
        set PlayerHealth -= GetRandomInt(1,10)
        Print("Player Health: {PlayerHealth}")

    IsBattleInProgress() : logic =
        if (EnemyHealth <= 0):
            Print("Player defeated enemy")
            return false
        else if (PlayerHealth <= 0):
            Print ("The enemy has defeated player")
            return false
        else:
            # Print ("The battle continues!")
            return true
        

        