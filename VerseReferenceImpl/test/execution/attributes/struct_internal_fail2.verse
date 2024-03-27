# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
    m1<public> := module:
      s0<public> := struct:
          X0<internal>:int = 42

    H0<public>(S:m1.s0):int = S.X0

m0.H0(m0.m1.s0{})
