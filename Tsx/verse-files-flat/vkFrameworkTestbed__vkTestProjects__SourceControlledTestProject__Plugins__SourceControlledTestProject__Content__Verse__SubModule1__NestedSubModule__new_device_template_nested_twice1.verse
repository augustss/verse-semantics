
using { /Fortnite.com/Devices/Classes }
using { /Verse.org/Interop }
using { /Verse.org/VerseEngine/Diagnostics }
 
# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
new_device_template_nested_twice1 := class<concrete>(user_device):
    
    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Log("Verse device started!")

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
