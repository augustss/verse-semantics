using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }

set_shield_to_character := class(creative_device):

    OnBegin<override>()<suspends>:void=
        Print ("Start OnBegin() function for set_shield_to_character device and get _Player")

        if (_Player := GetPlayspace().GetPlayers()[0]):
           Print ("Get FortCharacter")
           if (FortCharacter := _Player.GetFortCharacter[]):
                Print ("Set 50.0 points to shield")
                FortCharacter.SetShield(50.0)