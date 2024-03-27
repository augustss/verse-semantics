# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
    s0<public> := struct:
        X0<internal>:int = 42

    H0<public>(S:s0):int = S.X0

    m1<public> := module:
      H1<public>(S:s0):int = S.X0

S0 := m0.s0{}
m0.H0(S0), m0.m1.H1(S0)
