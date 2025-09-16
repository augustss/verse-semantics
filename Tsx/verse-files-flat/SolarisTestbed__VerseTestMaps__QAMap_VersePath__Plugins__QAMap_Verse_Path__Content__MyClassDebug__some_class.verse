using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation }

using {MyClassDebug.MyClassModule}
using {MyClassDebug.MyModule}
using {MyClassDebug.base_module.sub_module.mini_module}

# Creating functions and constants while defining the path
# (/localhost/QAMap_Verse_Path/MyClassDebug:)MyInt : int = 1
# (/localhost/QAMap_Verse_Path/MyClassDebug:)SomeFunc(SomeInt : int):void= "[SUCCESS] {SomeInt}"

# Defintiion of our Module Imports
# MyClassModuleRef := import(/localhost/QAMap_Verse_Path/MyClassDebug/MyClassModule)
# mini_moduleRef := import(/localhost/QAMap_Verse_Path/MyClassDebug/base_module/sub_module/mini_module)



MyDevice := class(creative_device):

    var MyLogic : logic = true
    var MyVector : vector2 = vector2{X:= 0.0, Y:= 0.0}
    var MyString : []char = "MyString"

    # Instances of our my_class for easy reference
    MyClassRef : MyClassModule.my_class = MyClassModule.my_class {}
    MyClassRef2 : base_module.sub_module.mini_module.my_class = base_module.sub_module.mini_module.my_class {}



    OnBegin<override>()<suspends>:void=
        <# These are examples on how to reference code using custom paths and modules. Dependencies between projects aren't available yet.#>
        {
            <# Module referenced in the same verse file #>
            (MyModule:)FooFunc(MyString) <# Should reference line 56 MyModule #>
            MyClassDebug.MyModule.FooFunc(MyString) <# Should reference line 56 MyModule #>
            # (/localhost/QAMap_Verse_Path/MyClassDebug:)MyModule.FooFunc(MyString) <# Should reference line 56 MyModule #>
            # SomeFunc(MyInt)

            <# Module referenced in the same folder but different verse file with different name and ambiguous function name #>
            MyClassDebug.MyClassModule.my_class{}.FooFunc() <# Should reference line 11 MyClassModule #>
             MyClassRef.FooFunc() <# Should reference line 11 MyClassModule #>
            # (/localhost/QAMap_Verse_Path/MyClassDebug:)MyClassModule.my_class{}.FooFunc() <# Should reference line 11 MyClassModule #>
            # (/localhost/QAMap_Verse_Path/MyClassDebug:)MyClassModule.my_class{}.LogicToString(MyLogic) <# Should reference line 1 MyClassModule #>
            # (/localhost/QAMap_Verse_Path/MyClassDebug:)MyClassModule.my_class{}.Vector2ToString(MyVector) <# Should reference line 11 MyClassModule #>
            # MyClassModuleRef.my_class{}.FooFunc() <# Should reference line 11 MyClassModule #>
            # MyClassRef.(/localhost/MyProject_1/MyClassModule/my_class:)FooFunc() <# This syntax will work in 30.00 #>
            # MyClassRef.(/localhost/MyProject_1/MyClassModule:)MyClassModule.my_class{}.FooFunc() <# This syntax will work in 30.00 #>

            <# Nested sub modules with ambiguous references #>
            # (/localhost/QAMap_Verse_Path/MyClassDebug:)base_module.sub_module.mini_module.my_class{}.FooFunc() <# Should reference line 5 mini_module #>
            MyClassRef2.FooFunc() <# Should reference line 5 mini_module #>
            # mini_moduleRef.my_class{}.FooFunc() <# Should reference line 5 mini_module #>
        }

MyModule <public> := module:
    FooFunc<public>(Text : string):void=
        return Print("[SUCCESS] FooFunc() | MyModule | {Text}")


