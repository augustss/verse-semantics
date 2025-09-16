# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Assets }


solaris_test_lib<native> := class:

    
    AssetGetStaticmesh<native_callable>()<transacts><decides>:mesh=
        return MakeAsset(mesh, "/Engine/EditorMeshes/EditorCube.EditorCube")


    AssetGetSkeletalmesh<native_callable>()<transacts><decides>:skeletal_mesh=
        return MakeAsset(skeletal_mesh, "/Engine/EditorMeshes/SkeletalMesh/DefaultSkeletalMesh.DefaultSkeletalMesh")


    AssetGetMaterial<native_callable>()<transacts><decides>:material=
        return MakeAsset(material, "/Engine/EngineDebugMaterials/BlackUnlitMaterial.BlackUnlitMaterial")


    AssetGetSoundcue<native_callable>()<transacts><decides>:sound=
        return MakeAsset(sound, "/Engine/EditorSounds/GamePreview/StartPlayInEditor_Cue.StartPlayInEditor_Cue")


    AssetGetAnimationBlueprint<native_callable>()<transacts><decides>:animation_blueprint=
        return MakeAsset(animation_blueprint, "/Engine/Tutorial/SubEditors/TutorialAssets/TutorialAnimationBlueprint.TutorialAnimationBlueprint")
