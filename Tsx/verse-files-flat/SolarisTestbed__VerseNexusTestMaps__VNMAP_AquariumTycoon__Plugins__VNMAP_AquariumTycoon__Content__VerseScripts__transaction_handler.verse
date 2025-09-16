
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.



# A Verse-authored creative device that can be placed in a level
transaction_handler_device := class(nexus_test_device):

    var CurrentCharacter : ?fort_character = false
    var CurrentPurchase : ?purchase = false

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Get Purchase")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - transaction_handler_device")

        (super:)OnBegin() # * All devices will need this line

        # SetupTest("Get Purchase", false)
        # CompleteTest("Get Purchase", "Get Purchase Completed in OnBegin()", false)

    OnEnd<override>():void =
        Print("Nexus OnEnd - transaction_handler_device")
        (super:)OnEnd() # * All devices will need this line

    SetPurchase(InPurchase : purchase) : void =
        #Print("Sent in Purchse for {InPurchase.Price}")
        set CurrentPurchase = option{InPurchase}
        
    GetPurchase()<transacts> : ?purchase =
        #Print("Getting Purchase")
        if(MyPurchase : purchase = CurrentPurchase?):
            #AddResults("Get Purchase", true, "Successfully set MyPurchase to CurrentPurchase in GetPurchase()")
        else:
            #AddResults("Get Purchase", false, "CurrentPurchase does not equal purchase class instance when expected to be set in Transaction Handler | Occurs in GetPurchase()")

        if(CurrentPurchase = false):
            #AddResults("Get Purchase", false, "CurrentPurchase value is false when it is expected to be set in Transaction Handler | Occurs in GetPurchase()")

        return CurrentPurchase

        
