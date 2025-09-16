# Verse Script used to achieve things that we can't do yet

using { /UnrealEngine.com/Temporary/SpatialMath }

########
# Util #
########

direction:= enum:
    Up, Down, Left, Right

vector:= enum:
    X, Y, Z

min_max_vector3 :=struct: 
    Minimum:vector3, Maximum:vector3

#########
# Logic #
#########

LogicToString(Logic:logic):string=


    ReturnedLogic:=if(Logic = true) then "false" else "true"


Not(Logic:logic)<transacts>:logic=

    ReturnedLogic:=if(Logic = true) then false else true

##########
# Vector #
##########

VectorToString(Vector2:vector2):string=

    return "[X = {Vector2.X}] [Y = {Vector2.Y}]"

VectorToString(Vector3:vector3):string=

    return "[X = {Vector3.X}] [Y = {Vector3.Y}] [Z = {Vector3.Z}]"


AddFloatToVector(VectorDirection:vector, Vector3:vector3, Float:float):vector3=

    case(VectorDirection):
        vector.X => 
            vector3{X:=Float,Y:=0.0,Z:=0.0} + Vector3
        vector.Y => 
            vector3{X:=0.0,Y:=Float,Z:=0.0} + Vector3
        vector.Z =>
            vector3{X:=0.0,Y:=0.0,Z:=Float} + Vector3

AddFloatToVector(VectorDirection:vector, Vector2:vector2, Float:float):vector2=

    case(VectorDirection):
        vector.X => 
            vector2{X:=Float,Y:=0.0} + Vector2
        vector.Y => 
            vector2{X:=0.0,Y:=Float} + Vector2
        vector.Z => 
            Print("AddFloatToVector used with Vector2 does not support adding a Z float")
            Vector2 

GetVectorFromRotation(Rotation:rotation):vector3=

    vector3{X:=Rotation.GetAxis().X,Y:=Rotation.GetAxis().Y,Z:=Rotation.GetAxis().Z}

MultiplyVector(Vector3:vector3, Float:float):vector3=

    vector3{X:=Vector3.X * Float, Y:=Vector3.Y * Float, Z:=Vector3.Z * Float}

############
# Rotation #
############

AddFloatToRotation(VectorDirection:vector, Rotation:rotation, Float:float):rotation=

    case(VectorDirection):
        vector.X => 
            Rotation.ApplyRoll(Float)
        vector.Y => 
            Rotation.ApplyPitch(Float)
        vector.Z =>
            Rotation.ApplyYaw(Float)
