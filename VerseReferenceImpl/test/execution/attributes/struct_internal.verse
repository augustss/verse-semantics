# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
    s0 := struct:
        X0<internal>:int = 42

    H0(S:s0):int = S.X0

    m1 := module:
      H1(S:s0):int = S.X0

S0 := m0.s0{}
m0.H0(S0), m0.m1.H1(S0)
