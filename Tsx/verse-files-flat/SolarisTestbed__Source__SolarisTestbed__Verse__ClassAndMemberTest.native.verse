class_2 := class<computes>(class_1):
    instance_data2 : class_1

    InstanceMethod2() : string=
    {
        return ("two")
    }

    
    InstanceMethod1<override>() : string=
    {
        return ("one two")
    }

Class2 := module:
    
    ClassData2<public> : int= 0

    
    ClassMethod2<public>() : int=
    {
        return (2)
    }

class_1 := class<computes>:
    InstanceData1 : string

    InstanceMethod1() : string=
    {
        return ("one")
    }

Class1 := module:
    
    ClassData1<public> : class_1= class_1{InstanceData1 := ""}

    
    ClassMethod1<public>() : int=
    {
        return (1)
    }
