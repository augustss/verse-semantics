using { LspMath }

cube_style<public> := enum:
    Cube
    Cone
    Sphere

test_class<public> := class:

    # Default access
    DataDefault : float = 1.0

    DataPublic<public> : float = 2.0

    DataInternal<internal> : float = 3.0

    # Default access
    MethodDefault()<transacts> : float =
        return (11.0)

    MethodPublic<public>()<transacts> : float =
        return (22.0)

    MethodInternal<internal>()<transacts> : float =
        return (33.0)

math_test_class<public> := class(test_class):

    var MutableVector<public>:?vector3

    ConstVector<private>:vector3 = vector3{X:=1.0, Y:=2.0, Z:=3.0}

    var TestEnum<public >:cube_style

    DoStuff<public>() : void =
        set MutableVector = option{Vector3.CreateZero()}
        set MutableVector = option{Self.ConstVector}
        set TestEnum = cube_style.Sphere
        set MutableVector = option{vector3{X:=DataDefault, Y:=DataPublic, Z:=DataInternal}}
        set MutableVector = option{vector3{X:=MethodDefault(), Y:=MethodPublic(), Z:=MethodInternal()}}
       