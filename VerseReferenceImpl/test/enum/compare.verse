# Copyright Epic Games, Inc. All Rights Reserved.

E := enum{B; A; C}

X:E
Y:E

# Enum values are considered unordered, the line below should not produce any
# output, expect stuck or wrong.

(all{X<Y}, all{X<=Y}, all{X>Y}, all{X>=Y})
