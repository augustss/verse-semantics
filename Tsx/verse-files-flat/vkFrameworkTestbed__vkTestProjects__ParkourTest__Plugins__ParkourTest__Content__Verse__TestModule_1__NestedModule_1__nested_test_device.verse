
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_new_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
nested_test_device := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_new_device}

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
