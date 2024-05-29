# Copyright Epic Games, Inc. All Rights Reserved.
succeeds:
  domain:
    t:type
    verify:
      forall j
      domain:
        X := t[j]
      succeeds:
        t[1]
  assume:
    domain:
      X := t[2]
    forall k
    assume:
      t[k]
