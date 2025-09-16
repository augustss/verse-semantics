
# using { /UnrealEngine.com/Temporary/SpatialMath }
# using { /Verse.org/Assets }
# using { /Verse.org/Verse }
# using { /Verse.org/SceneGraph }

# forest := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self }
#         Tree1:tree_leaf=tree_leaf{}
#         if (Transform := Tree1.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=0.0, Z:=250.0}  }
#         Tree2:tree_leaf=tree_leaf{}
#         if (Transform := Tree2.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=0.0, Z:=0.0}  }
#         Tree3:tree_leaf=tree_leaf{}
#         if (Transform := Tree3.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=0.0, Z:=0.0}  }
#         Tree4:tree_leaf=tree_leaf{}
#         if (Transform := Tree4.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=0.0, Z:=0.0}  }
#         Tree5:tree_leaf=tree_leaf{}
#         if (Transform := Tree5.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=0.0, Z:=0.0}  }
#         Tree6:tree_leaf=tree_leaf{}
#         if (Transform := Tree6.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=0.0, Z:=0.0}  }
#         Tree7:tree_leaf=tree_leaf{}
#         if (Transform := Tree7.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=0.0, Z:=0.0}  }
#         Tree8:tree_leaf=tree_leaf{}
#         if (Transform := Tree8.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=0.0, Z:=0.0}  }
#         Tree9:tree_leaf=tree_leaf{}
#         if (Transform := Tree9.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=0.0, Z:=0.0}  }
#         Tree10:tree_leaf=tree_leaf{}
#         if (Transform := Tree10.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=0.0, Z:=0.0}  }

#         Tree11:tree_leaf=tree_leaf{}
#         if (Transform := Tree11.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=500.0, Z:=0.0}  }
#         Tree12:tree_leaf=tree_leaf{}
#         if (Transform := Tree12.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=500.0, Z:=0.0}  }
#         Tree13:tree_leaf=tree_leaf{}
#         if (Transform := Tree13.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=500.0, Z:=0.0}  }
#         Tree14:tree_leaf=tree_leaf{}
#         if (Transform := Tree14.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=500.0, Z:=0.0}  }
#         Tree15:tree_leaf=tree_leaf{}
#         if (Transform := Tree15.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=500.0, Z:=0.0}  }
#         Tree16:tree_leaf=tree_leaf{}
#         if (Transform := Tree16.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=500.0, Z:=0.0}  }
#         Tree17:tree_leaf=tree_leaf{}
#         if (Transform := Tree17.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=500.0, Z:=0.0}  }
#         Tree18:tree_leaf=tree_leaf{}
#         if (Transform := Tree18.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=500.0, Z:=0.0}  }
#         Tree19:tree_leaf=tree_leaf{}
#         if (Transform := Tree19.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=500.0, Z:=0.0}  }
#         Tree20:tree_leaf=tree_leaf{}
#         if (Transform := Tree20.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=500.0, Z:=0.0}  }

#         Tree21:tree_leaf=tree_leaf{}
#         if (Transform := Tree21.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=1000.0, Z:=0.0}  }
#         Tree22:tree_leaf=tree_leaf{}
#         if (Transform := Tree22.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=1000.0, Z:=0.0}  }
#         Tree23:tree_leaf=tree_leaf{}
#         if (Transform := Tree23.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=1000.0, Z:=0.0}  }
#         Tree24:tree_leaf=tree_leaf{}
#         if (Transform := Tree24.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=1000.0, Z:=0.0}  }
#         Tree25:tree_leaf=tree_leaf{}
#         if (Transform := Tree25.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=1000.0, Z:=0.0}  }
#         Tree26:tree_leaf=tree_leaf{}
#         if (Transform := Tree26.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=1000.0, Z:=0.0}  }
#         Tree27:tree_leaf=tree_leaf{}
#         if (Transform := Tree27.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=1000.0, Z:=0.0}  }
#         Tree28:tree_leaf=tree_leaf{}
#         if (Transform := Tree28.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=1000.0, Z:=0.0}  }
#         Tree29:tree_leaf=tree_leaf{}
#         if (Transform := Tree29.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=1000.0, Z:=0.0}  }
#         Tree30:tree_leaf=tree_leaf{}
#         if (Transform := Tree30.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=1000.0, Z:=0.0}  }

#         Tree31:tree_leaf=tree_leaf{}
#         if (Transform := Tree31.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=1500.0, Z:=0.0}  }
#         Tree32:tree_leaf=tree_leaf{}
#         if (Transform := Tree32.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=1500.0, Z:=0.0}  }
#         Tree33:tree_leaf=tree_leaf{}
#         if (Transform := Tree33.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=1500.0, Z:=0.0}  }
#         Tree34:tree_leaf=tree_leaf{}
#         if (Transform := Tree34.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=1500.0, Z:=0.0}  }
#         Tree35:tree_leaf=tree_leaf{}
#         if (Transform := Tree35.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=1500.0, Z:=0.0}  }
#         Tree36:tree_leaf=tree_leaf{}
#         if (Transform := Tree36.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=1500.0, Z:=0.0}  }
#         Tree37:tree_leaf=tree_leaf{}
#         if (Transform := Tree37.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=1500.0, Z:=0.0}  }
#         Tree38:tree_leaf=tree_leaf{}
#         if (Transform := Tree38.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=1500.0, Z:=0.0}  }
#         Tree39:tree_leaf=tree_leaf{}
#         if (Transform := Tree39.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=1500.0, Z:=0.0}  }
#         Tree40:tree_leaf=tree_leaf{}
#         if (Transform := Tree40.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=1500.0, Z:=0.0}  }

#         Tree41:tree_leaf=tree_leaf{}
#         if (Transform := Tree41.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=2000.0, Z:=0.0}  }
#         Tree42:tree_leaf=tree_leaf{}
#         if (Transform := Tree42.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=2000.0, Z:=0.0}  }
#         Tree43:tree_leaf=tree_leaf{}
#         if (Transform := Tree43.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=2000.0, Z:=0.0}  }
#         Tree44:tree_leaf=tree_leaf{}
#         if (Transform := Tree44.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=2000.0, Z:=0.0}  }
#         Tree45:tree_leaf=tree_leaf{}
#         if (Transform := Tree45.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=2000.0, Z:=0.0}  }
#         Tree46:tree_leaf=tree_leaf{}
#         if (Transform := Tree46.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=2000.0, Z:=0.0}  }
#         Tree47:tree_leaf=tree_leaf{}
#         if (Transform := Tree47.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=2000.0, Z:=0.0}  }
#         Tree48:tree_leaf=tree_leaf{}
#         if (Transform := Tree48.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=2000.0, Z:=0.0}  }
#         Tree49:tree_leaf=tree_leaf{}
#         if (Transform := Tree49.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=2000.0, Z:=0.0}  }
#         Tree50:tree_leaf=tree_leaf{}
#         if (Transform := Tree50.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=2000.0, Z:=0.0}  }

#         Tree51:tree_leaf=tree_leaf{}
#         if (Transform := Tree51.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=2500.0, Z:=0.0}  }
#         Tree52:tree_leaf=tree_leaf{}
#         if (Transform := Tree52.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=2500.0, Z:=0.0}  }
#         Tree53:tree_leaf=tree_leaf{}
#         if (Transform := Tree53.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=2500.0, Z:=0.0}  }
#         Tree54:tree_leaf=tree_leaf{}
#         if (Transform := Tree54.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=2500.0, Z:=0.0}  }
#         Tree55:tree_leaf=tree_leaf{}
#         if (Transform := Tree55.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=2500.0, Z:=0.0}  }
#         Tree56:tree_leaf=tree_leaf{}
#         if (Transform := Tree56.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=2500.0, Z:=0.0}  }
#         Tree57:tree_leaf=tree_leaf{}
#         if (Transform := Tree57.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=2500.0, Z:=0.0}  }
#         Tree58:tree_leaf=tree_leaf{}
#         if (Transform := Tree58.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=2500.0, Z:=0.0}  }
#         Tree59:tree_leaf=tree_leaf{}
#         if (Transform := Tree59.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=2500.0, Z:=0.0}  }
#         Tree60:tree_leaf=tree_leaf{}
#         if (Transform := Tree60.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=2500.0, Z:=0.0}  }
        
#         Tree61:tree_leaf=tree_leaf{}
#         if (Transform := Tree61.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=3000.0, Z:=0.0}  }
#         Tree62:tree_leaf=tree_leaf{}
#         if (Transform := Tree62.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=3000.0, Z:=0.0}  }
#         Tree63:tree_leaf=tree_leaf{}
#         if (Transform := Tree63.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=3000.0, Z:=0.0}  }
#         Tree64:tree_leaf=tree_leaf{}
#         if (Transform := Tree64.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=3000.0, Z:=0.0}  }
#         Tree65:tree_leaf=tree_leaf{}
#         if (Transform := Tree65.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=3000.0, Z:=0.0}  }
#         Tree66:tree_leaf=tree_leaf{}
#         if (Transform := Tree66.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=3000.0, Z:=0.0}  }
#         Tree67:tree_leaf=tree_leaf{}
#         if (Transform := Tree67.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=3000.0, Z:=0.0}  }
#         Tree68:tree_leaf=tree_leaf{}
#         if (Transform := Tree68.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=3000.0, Z:=0.0}  }
#         Tree69:tree_leaf=tree_leaf{}
#         if (Transform := Tree69.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=3000.0, Z:=0.0}  }
#         Tree70:tree_leaf=tree_leaf{}
#         if (Transform := Tree70.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=3000.0, Z:=0.0}  }

#         Tree71:tree_leaf=tree_leaf{}
#         if (Transform := Tree71.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=500.0, Y:=3500.0, Z:=0.0}  }
#         Tree72:tree_leaf=tree_leaf{}
#         if (Transform := Tree72.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1000.0, Y:=3500.0, Z:=0.0}  }
#         Tree73:tree_leaf=tree_leaf{}
#         if (Transform := Tree73.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=1500.0, Y:=3500.0, Z:=0.0}  }
#         Tree74:tree_leaf=tree_leaf{}
#         if (Transform := Tree74.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2000.0, Y:=3500.0, Z:=0.0}  }
#         Tree75:tree_leaf=tree_leaf{}
#         if (Transform := Tree75.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=2500.0, Y:=3500.0, Z:=0.0}  }
#         Tree76:tree_leaf=tree_leaf{}
#         if (Transform := Tree76.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3000.0, Y:=3500.0, Z:=0.0}  }
#         Tree77:tree_leaf=tree_leaf{}
#         if (Transform := Tree77.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=3500.0, Y:=3500.0, Z:=0.0}  }
#         Tree78:tree_leaf=tree_leaf{}
#         if (Transform := Tree78.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4000.0, Y:=3500.0, Z:=0.0}  }
#         Tree79:tree_leaf=tree_leaf{}
#         if (Transform := Tree79.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=4500.0, Y:=3500.0, Z:=0.0}  }
#         Tree80:tree_leaf=tree_leaf{}
#         if (Transform := Tree80.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=5000.0, Y:=3500.0, Z:=0.0}  }

#         AttachComponents(array{ TransformComp })
#         AttachEntities of array:
#             entity(Tree1)
#             entity(Tree2)
#             entity(Tree3)
#             entity(Tree4)
#             entity(Tree5)
#             entity(Tree6)
#             entity(Tree7)
#             entity(Tree8)
#             entity(Tree9)
#             entity(Tree10)
#             entity(Tree11)
#             entity(Tree12)
#             entity(Tree13)
#             entity(Tree14)
#             entity(Tree15)
#             entity(Tree16)
#             entity(Tree17)
#             entity(Tree18)
#             entity(Tree19)
#             entity(Tree20)
#             entity(Tree21)
#             entity(Tree22)
#             entity(Tree23)
#             entity(Tree24)
#             entity(Tree25)
#             entity(Tree26)
#             entity(Tree27)
#             entity(Tree28)
#             entity(Tree29)
#             entity(Tree30)
#             entity(Tree31)
#             entity(Tree32)
#             entity(Tree33)
#             entity(Tree34)
#             entity(Tree35)
#             entity(Tree36)
#             entity(Tree37)
#             entity(Tree38)
#             entity(Tree39)
#             entity(Tree40)
#             entity(Tree41)
#             entity(Tree42)
#             entity(Tree43)
#             entity(Tree44)
#             entity(Tree45)
#             entity(Tree46)
#             entity(Tree47)
#             entity(Tree48)
#             entity(Tree49)
#             entity(Tree50)
#             entity(Tree51)
#             entity(Tree52)
#             entity(Tree53)
#             entity(Tree54)
#             entity(Tree55)
#             entity(Tree56)
#             entity(Tree57)
#             entity(Tree58)
#             entity(Tree59)
#             entity(Tree60)
#             entity(Tree61)
#             entity(Tree62)
#             entity(Tree63)
#             entity(Tree64)
#             entity(Tree65)
#             entity(Tree66)
#             entity(Tree67)
#             entity(Tree68)
#             entity(Tree69)
#             entity(Tree70)
#             entity(Tree71)
#             entity(Tree72)
#             entity(Tree73)
#             entity(Tree74)
#             entity(Tree75)
#             entity(Tree76)
#             entity(Tree77)
#             entity(Tree78)
#             entity(Tree79)
#             entity(Tree80)
#             # Tree81
#             # Tree82
#             # Tree83
#             # Tree84
#             # Tree85
#             # Tree86
#             # Tree87
#             # Tree88
#             # Tree89
#             # Tree90
#             # Tree91
#             # Tree92
#             # Tree93
#             # Tree94
#             # Tree95
#             # Tree96
#             # Tree97
#             # Tree98
#             # Tree99
#             # Tree100
