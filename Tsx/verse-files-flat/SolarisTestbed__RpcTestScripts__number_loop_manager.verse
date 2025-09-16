using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }

number_loop_manager := class(creative_device):
    var PlayerHealth : int = 100
    var EnemyHealth : int = 100
    var AttackDamage : int = 5
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        var Index : int = 0
        loop:
            if (Index >= 10):
                Print("Printed using Loop, Index: {Index}")
                break:
            set Index = Index + 1
        for (Number : int = 1..10):
            Print("Printed using ForLoop, Number: {Number}")
