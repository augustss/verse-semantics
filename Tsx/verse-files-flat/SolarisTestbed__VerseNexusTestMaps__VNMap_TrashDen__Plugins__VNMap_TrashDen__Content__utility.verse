using { /UnrealEngine.com/Temporary/SpatialMath }
# Utils verse file to achieve QOL and other convenient things

PrintStringMethod(String:string):void=

    Print ("{String}")

LogicToString(Logic:logic):string=

    ReturnedLogic:=
        if(Logic = true)

        then "false"

        else "true"

VectorToString(Vector3:vector3):string=

    return "[X = {Vector3.X}] [Y= {Vector3.Y}] [Z = {Vector3.Z}]"

