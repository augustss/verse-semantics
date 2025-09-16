using { /Verse.org/Native }
using { /Verse.org/Simulation }

TestModule<public> := module:
    @import_as("/Script/VVMTest.AVVMTestActor")
    vvm_test_actor_native_base<native> := class<abstract><epic_internal>():

    vvm_test_actor<native> := class(vvm_test_actor_native_base):
        @editable
        Greeting<public>:string

        @editable
        Target<public>:named

        HelloWorld<native_callable><public>():string = "{Greeting}, {Target.GetName()}!"

    named<public> := interface:
        GetName<public>():string