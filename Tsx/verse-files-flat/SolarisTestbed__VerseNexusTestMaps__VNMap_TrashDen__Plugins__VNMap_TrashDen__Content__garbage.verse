# big_struct := struct:
#     A:int = 2147483647
#     B:int = 2147483647
#     C:int = 2147483647
#     D:int = 2147483647
#     E:int = 2147483647
#     F:int = 2147483647
#     G:int = 2147483647
#     H:int = 2147483647
#     I:int = 2147483647
#     J:int = 2147483647
#     K:int = 2147483647
#     L:int = 2147483647
#     M:int = 2147483647
#     N:int = 2147483647
#     O:int = 2147483647
#     D:int = 2147483647
#     P:int = 2147483647
#     Q:int = 2147483647
#     R:int = 2147483647
#     S:int = 2147483647
#     T:int = 2147483647
#     U:int = 2147483647
#     V:int = 2147483647
#     W:int = 2147483647
#     X:int = 2147483647
#     Y:int = 2147483647
#     Z:int = 2147483647
# bigger_struct := struct:
#     Aa:struct = big_struct
#     Ba:struct = big_struct
#     Ca:struct = big_struct
#     Da:struct = big_struct
#     Ea:struct = big_struct
#     Fa:struct = big_struct
#     Ga:struct = big_struct
#     Ha:struct = big_struct
#     Ia:struct = big_struct
#     Ja:struct = big_struct
#     Ka:struct = big_struct
#     La:struct = big_struct
#     Ma:struct = big_struct
#     Na:struct = big_struct
#     Oa:struct = big_struct
#     Da:struct = big_struct
#     Pa:struct = big_struct
#     Qa:struct = big_struct
#     Ra:struct = big_struct
#     Sa:struct = big_struct
#     Ta:struct = big_struct
#     Ua:struct = big_struct
#     Va:struct = big_struct
#     Wa:struct = big_struct
#     Xa:struct = big_struct
#     Ya:struct = big_struct
#     Za:struct = big_struct
# gc_force_device := class(creative_device)
#     var GiantStructArray:[]big_struct = array{}
#     var NumOfBigStructs:int = 100000
#     var StructElementsPerFrame:int = 100

#     OnBegin<override>()<suspends>:void=

#         Print ("GCTest: OnBegin")
#         Print ("For loop started with {NumOfBigStructs} elements")
#         loop:
#             for (Index:=0..StructElementsPerFrame):
#                 set GiantStructArray += array{BiggestStruct:=big_struct{}}
            
#             if (NumOfBigStructs < GiantStructArray.Length)
#             { break }
#             Sleep (0.0)
    
#     OnEnd<override>():void=
#         Print("GCTest: OnEnd")