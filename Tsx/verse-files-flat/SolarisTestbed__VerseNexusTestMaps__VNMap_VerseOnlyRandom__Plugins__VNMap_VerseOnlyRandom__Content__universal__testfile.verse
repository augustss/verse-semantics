using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }


VectorToString(Vector3:vector3):string=


    return "[X = {Vector3.X}] [Y = {Vector3.Y}] [Z = {Vector3.Z}]"
    
# test_manager := class(creative_device):
#     var OriginalHP : int = 100
#     var SomeNumber : int = 5
#     Destroyed<public>()<transacts><decides>:void=
#         if (OriginalHP < 0):
#         Print("Yes")
#         {}
#         # else:
#         # Print("No")
#         # {}

#     OnBegin<override>()<suspends>:void=
#         Destroyed()