
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


sh_test_bed_device := class(creative_device):

    
    OnBegin<override>()<suspends>:void=
        Print("Hello, world!")
        Print("2 + 2 = {2 + 2}")