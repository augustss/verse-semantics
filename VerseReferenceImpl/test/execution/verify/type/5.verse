# Copyright Epic Games, Inc. All Rights Reserved.
domain:
  t:type
  exists i
  t[i]
  verify:
    forall k
    if:
      domain:
        X := t[k]
    then:
      succeeds:
        X = 1
        t[2]
  i = 1
