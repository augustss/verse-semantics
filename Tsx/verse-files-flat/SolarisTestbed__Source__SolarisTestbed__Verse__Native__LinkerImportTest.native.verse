# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Native }

@import_as("/Script/Engine.AActor")
imported_actor<native> := class:

@import_as("/Script/SolarisTestbed.ASolImportTestActor")
imported_testbed_actor<native> := class(imported_actor):

ImportedTestbedActorLib := module:
    
    FindAllTestbedActors<native><public>()<transacts>:[]imported_testbed_actor


sol_script_actor<native> := class(imported_testbed_actor):
    Id:int

    
    GetSubObjs<native>()<transacts>:[]imported_testbed_object

    Print():void=
    {
        IdStrPrefix := "{Id},"

        var Index:int = 0
        for (SubObj:GetSubObjs())
        {
            SolarisTestbedLogPrint("{IdStrPrefix}{Index}")
            if (AsScriptSubObj := sol_script_actor_sub_obj[SubObj])
            {
                AsScriptSubObj.Print()
            }
            set Index += 1
        }
    }


@import_as("/Script/SolarisTestbed.USolImportTestObject")
imported_testbed_object<native> := class:


sol_script_actor_sub_obj<native> := class(imported_testbed_object):
    StrVal:string
    var Parent:imported_actor

    
    SetParent<native_callable>(InParent:imported_actor):void=
    {
        set Parent = InParent
    }

    Print():void=
    {
        var ParentId:int = -1
        if (AsTestbedActor := imported_testbed_actor[Parent])
        {
            if (AsScriptActor := sol_script_actor[AsTestbedActor])
            {
                set ParentId = AsScriptActor.Id
            }
        }

        SolarisTestbedLogPrint("{ParentId}: {StrVal}")
    }
