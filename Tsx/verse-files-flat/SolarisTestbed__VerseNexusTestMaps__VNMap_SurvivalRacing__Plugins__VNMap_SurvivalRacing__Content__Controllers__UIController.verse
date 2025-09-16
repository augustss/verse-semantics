
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { GameEvents }


UIController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}


    OnBegin<override>()<suspends>:void=
        Print("UI Controller Starts")