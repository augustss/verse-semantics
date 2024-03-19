# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
    s0<public> := struct:
        X0<internal>:int = 42

H0(S:m0.s0):int = S.X0

H0(m0.s0{})
