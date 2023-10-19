# Copyright Epic Games, Inc. All Rights Reserved.
exists If
exists F
If = function (exists P). function (exists X). function (exists Y):
  all{P = true; X()} | all{P = false; Y()}
If(true)(function (exists _). false)[function (exists _). true]
