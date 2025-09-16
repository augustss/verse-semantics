using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SceneGraph }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/Diagnostics }

# A Verse-authored component that can be added to entities
follow_player_component<public> := class<direct>(component):


  
    # Runs when the component should start simulating in a running game.
    OnSimulate<override>()<suspends>:void =
        EntityToSearch := 
            if: 
                SimulationEntity := Entity.GetSimulationEntity[]
            then:
                SimulationEntity
            else:
                Entity

<#         FollowPlayer()

    FollowPlayer():void = 
        loop:
            if (PlayerList := GetPlayerSpace().GetPlayerSpace.GetPlayers(), FortniteCharacter := PlayerList[0].GetFortCharacter[]):
                PlayerPosition : vector3 = FortniteCharacter.GetTransform().Translation
                Entity.MoveTo(PlayerPosition, rotation{}, 1.0)

            else:
                break
            Print("Following player") #>