
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

concurrency_spam_manager := class (creative_device):

    ConcurrentSpam ()<suspends>:void =
        Print("Concurrency loop begins in 1.5 seconds")
        Sleep(1.5)
        loop:
            Print("Concurrency Loop activated")
            Sleep(10.0)
            Print("Concurrency Loop is finished")
    OnBegin<override>()<suspends>:void=
        ConcurrentSpam()