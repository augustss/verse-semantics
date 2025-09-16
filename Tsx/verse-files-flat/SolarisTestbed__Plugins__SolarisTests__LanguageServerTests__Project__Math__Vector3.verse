# Copyright Epic Games, Inc. All Rights Reserved.


vector3<public> := class<concrete><final>:

    
    IsZero<public>()<decides>:void = (X = 0.0 and Y = 0.0 and Z = 0.0)

    
    SizeSquared<public>():float =
    {
        return (X*X + Y*Y + Z*Z)
    }

    
    X<public>:float = 0.0

    
    Y<public>:float = 0.0

    
    Z<public>:float = 0.0


Vector3<public> := module:
    
    CreateZero<public>()<transacts>:vector3 =
        vector3{X:=0.0, Y:=0.0, Z:=0.0}

prefix'-'<public>(Operand:vector3):vector3=
    return vector3{X := -Operand.X, Y := -Operand.Y, Z := -Operand.Z}

operator'+'<public>(Left:vector3, Right:vector3):vector3=
    return vector3{X := Left.X + Right.X, Y := Left.Y + Right.Y, Z := Left.Z + Right.Z}

operator'*'<public>(Left:vector3, Right:vector3):vector3=
    return vector3{X := Left.X * Right.X, Y := Left.Y * Right.Y, Z := Left.Z * Right.Z}

operator'*'<public>(Left:vector3, Right:float):vector3=
    return vector3{X := Left.X * Right, Y := Left.Y * Right, Z := Left.Z * Right}

operator'*'<public>(Left:float, Right:vector3):vector3=
    return vector3{X := Left * Right.X, Y := Left * Right.Y, Z := Left * Right.Z}
