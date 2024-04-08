# Copyright Epic Games, Inc. All Rights Reserved.
domain:
  t:type
  verify:
    forall k
    if:
      domain:
        X := t[k]
    then:
      succeeds:
        X = 1|2
        t[1]
  t[2]
