using { /Fortnite.com/AI }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Characters }
 
# Getting started: https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/Verse/onboarding-guide-to-programming-with-verse-in-unreal-editor-for-fortnite

# A Verse-authored NPC Behavior that can be used within an NPC Character Definition or an NPC Spawner device's NPC Behavior Script Override.
customer_npc_behavior_basic<public> := class(npc_behavior):    
    
    # This function runs when the NPC is spawned in the world and ready to follow a behavior.
    OnBegin<override>()<suspends>:void=
        # TODO: Replace this with your code
        Print("Nexus OnBegin - customer_npc_behavior_basic")

    # This function runs when the NPC is despawned or eliminated from the world.
    OnEnd<override>():void=
        # TODO: Replace this with your code
        Print("Nexus OnEnd - customer_npc_behavior_basic")

    Navigate(Character : fort_character): void = 
        	# Get the Navigatable Interface, this allows you to tell it to move.
            if(Navigatable := Character.GetNavigatable[]):

