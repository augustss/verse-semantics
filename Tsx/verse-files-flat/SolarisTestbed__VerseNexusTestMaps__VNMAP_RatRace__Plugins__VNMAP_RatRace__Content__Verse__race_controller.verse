
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Simulation/Tags }

CheckpointTag := class(tag){}

# A Verse-authored creative device that can be placed in a level
race_controller := class(creative_device):
    @editable
    RaceManager:race_manager_device = race_manager_device{}

    

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        # TODO: Replace this with your code
        Print("Hello, world!")
        Print("2 + 2 = {2 + 2}")