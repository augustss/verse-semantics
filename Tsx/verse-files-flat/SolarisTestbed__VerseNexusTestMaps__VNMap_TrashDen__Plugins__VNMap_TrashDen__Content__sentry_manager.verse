using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }  

sentry_manager:=class (creative_device):
    @editable
    Sentry_1:sentry_device = sentry_device{}
    @editable
    Sentry_2:sentry_device = sentry_device{}
    @editable
    Sentry_3:sentry_device = sentry_device{}

    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin sentry_manager======")
            CreateDestroySentry()

    CreateDestroySentry()<suspends>:void=
        loop:
            Sleep(0.5)
            PrintStringMethod("Sentries spawned")
            Sentry_1.Spawn()
            Sentry_2.Spawn()
            Sentry_3.Spawn()
            Sleep(10.0)
            PrintStringMethod("Sentries destroyed")
            Sentry_1.DestroySentry()
            Sentry_2.DestroySentry()
            Sentry_3.DestroySentry()
            
    OnEnd<override>():void=
        Print ("======Nexus OnEnd sentry_manager======")