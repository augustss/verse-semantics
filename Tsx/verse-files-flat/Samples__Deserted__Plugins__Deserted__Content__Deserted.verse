# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Simulation/Tags }

DominationSpawnTags<public> := module:
    Tag_SpawnGroup<public>:=            class(tag){}
    Tag_SpawnGroup_Initial<public>:=    class(Tag_SpawnGroup){}
    Tag_SpawnGroup_GroupA<public>:=     class(Tag_SpawnGroup){}
    Tag_SpawnGroup_GroupB<public>:=     class(Tag_SpawnGroup){}
    Tag_SpawnGroup_GroupC<public>:=     class(Tag_SpawnGroup){}

    Tag_TeamOwnership<public>:=         class(tag){}
    Tag_TeamOwnership_Team1<public>:=   class(Tag_TeamOwnership){}
    Tag_TeamOwnership_Team2<public>:=   class(Tag_TeamOwnership){}