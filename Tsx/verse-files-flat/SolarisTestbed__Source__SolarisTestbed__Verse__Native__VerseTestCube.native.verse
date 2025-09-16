solaris_test_cube<native> := class:
    
    GetRotationSpeed<native>()<transacts>:float

    # Native coroutine - waits until hit event and returns other object
    
    WaitHit<native>()<suspends>:void

    NonNativePolyFillStyleTest(Speed:float):void=
    {
        # @TODO: we need a `self` expression
        if (Me := SolarisTestCubeLib.GetTestCube[])
        {
            SolarisTestbedLogPrint("Speed set to: {Speed}")
            SolarisTestCubeLib.SetCubeRotationSpeed(Me, Speed)
        }
    }

    
    TestNativeHookInstance<native_callable>(Speed:float):logic=
    {
        NonNativePolyFillStyleTest(Speed)
        return(true)
    }

SolarisTestCube := module:
    
    TestNativeHook<native_callable><public>(Speed:float):logic=
    {
        if (Cube := SolarisTestCubeLib.GetTestCube[])
        {
            return(Cube.TestNativeHookInstance(Speed))
        }
        return (false)
    }

SolarisTestCubeLib := module:
    
    GetTestCube<native><public>()<transacts><decides>:solaris_test_cube

    
    SetCubeRotationSpeed<native><public>(Cube:solaris_test_cube, DegPerSec:float) : void

    
    SetCubeRotationAxis<native><public>(Cube:solaris_test_cube, AxisX:float, AxisY:float, AxisZ:float) : void

    
    SetCubeStyle<native><public>(Cube:solaris_test_cube, Style:cube_style) : void


cube_style<native> := enum:
    Cube
    Cone
    Sphere
