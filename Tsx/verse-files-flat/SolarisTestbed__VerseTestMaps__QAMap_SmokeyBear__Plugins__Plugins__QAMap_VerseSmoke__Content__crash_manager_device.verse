
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Colors }

# This is an automated test script for covering the Self.GetPlayspace() on a deivce that references its default class type | SOL-5836
crash_manager_device := class(creative_device):

    OtherCrashDevice : other_crash_manager_device = other_crash_manager_device{}

    OnBegin<override>()<suspends>:void=
        Print("[SUCCESS] crash_manager_device | Called OnBegin function without crashing!", ?Color:=NamedColors.LightGreen)
        CrashMe()
        Print("[SUCCESS] crash_manager_device | Called CrashMe function without crashing!", ?Color:=NamedColors.LightGreen)
        OtherCrashDevice.CrashFunction()
        Print("[SUCCESS] crash_manager_device | Called other_crash_device | CrashFunction without crashing!", ?Color:=NamedColors.LightGreen)


(Blah:crash_manager_device).CrashMe():void=
    crash_manager_device{}.GetPlayspace().GetTeamCollection().GetTeams()

other_crash_manager_device := class(creative_device):

    CrashFunction()<suspends>:void=
        if(Player:=Self.GetPlayspace().GetPlayers()[0]):
        Print("[SUCCESS] other_crash_device | Called function without crashing!", ?Color:=NamedColors.LightGreen)
