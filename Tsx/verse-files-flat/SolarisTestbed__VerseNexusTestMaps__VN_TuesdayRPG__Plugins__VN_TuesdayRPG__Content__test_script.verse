
using { /Fortnite.com/Devices }
# using { /Fortnite.com/TestDevices }
# using { /Verse.org/Simulation }
# using { /UnrealEngine.com/Temporary/Diagnostics }

# learning_device := class(creative_device):

#     @editable
#     ButtonDevice:button_device=button_device{}

#     @editable
#     Test:button_device=button_device{}

 
#     OnBegin<override>()<suspends>:void=

#         ButtonDevice.Enable()

#         ButtonDevice.InteractedWithEvent.Subscribe(AddScore)

#         NewFloat : float = ButtonDevice.GetInteractionTime()

#     AddScore(Agent:agent):void=
#         Print("This will be called when the player interacts with the button")

# testdevice :=class(test_device):

#     TestingMyTestDevice()<suspends>:void=
#         ExpectedInt:int = 1
#         ActualInt:int = 1
#         TestRunner.CheckEqual(ExpectedInt, ActualInt, "My New Test values are equal")