# Copyright Epic Games, Inc. All Rights Reserved.

E := enum:
       A
       B

X:E
Y:E

(all{X<Y}, all{X<=Y}, all{X>Y}, all{X>=Y})
