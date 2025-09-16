
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }



EventsBus := class(creative_device):

    GameFinishEvent:event(int) = event(int){}

    FigureTurnEvent:event(int) = event(int){}


        