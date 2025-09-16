
# using { /UnrealEngine.com/Temporary/SpatialMath }
# using { /Verse.org/Verse }
# using { /Verse.org/Assets }
# using { /Verse.org/SceneGraph }

# CylinderMesh<public>:mesh = MakeAsset(mesh, "/Engine/BasicShapes/Cylinder.Cylinder")
# SphereMesh<public>:mesh = MakeAsset(mesh, "/Engine/BasicShapes/Sphere.Sphere")

# tree_trunk := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self, InitialRelativeTransform:=transform{ Scale:=vector3{X:=0.25, Y:=0.25, Z:=3.0} } }
#         AttachComp:attachment_component=attachment_component{ Entity := Self }
#         StaticMeshComp:static_mesh_component= static_mesh_component:
#             Entity := Self
#             Mesh := CylinderMesh
#             Material := option{ MakeAsset(material_asset, "/Engine/BasicShapes/BasicShapeMaterial.BasicShapeMaterial") }
#         PhysicsComp:physics_component = physics_component{ Entity := Self }

#         AttachComponents(array:
#             component(TransformComp)
#             component(StaticMeshComp)
#             component(PhysicsComp)
#             component(AttachComp)
#         )
        
# tree_leaf := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self, InitialRelativeTransform:=transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1} } }
#         AttachComp:attachment_component=attachment_component{ Entity := Self }
#         StaticMeshComp:static_mesh_component= static_mesh_component:
#             Entity:=Self
#             Mesh:=SphereMesh
#             Material:=option{ MakeAsset(material_asset, "/Engine/BasicShapes/BasicShapeMaterial.BasicShapeMaterial") }

#         AttachComponents(array:
#             component(TransformComp)
#             component(StaticMeshComp)
#             component(AttachComp)
#         )

# tree_branch := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self, InitialRelativeTransform:=transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=1.0} } }
#         AttachComp:attachment_component=attachment_component{ Entity := Self }
#         StaticMeshComp:static_mesh_component= static_mesh_component:
#             Entity := Self
#             Mesh:=CylinderMesh
#             Material:=option{ MakeAsset(material_asset, "/Engine/BasicShapes/BasicShapeMaterial.BasicShapeMaterial") }
#         PhysicsComp:physics_component=physics_component{ Entity := Self }

#         AttachComponents(array:
#             component(TransformComp)
#             component(StaticMeshComp)
#             component(PhysicsComp)
#             component(AttachComp)
#         )
        
# tree_branch_group := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{Entity := Self} 
#         AttachComp:attachment_component=attachment_component{Entity := Self}
#         BranchTruck:tree_branch=tree_branch{}
#         Leaf1:tree_leaf=tree_leaf{}
#         if (Transform := Leaf1.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=0.0, Z:=50.0}}
#         Leaf2:tree_leaf=tree_leaf{}
#         if (Transform := Leaf2.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=50.0, Z:=75.0}}
#         Leaf3:tree_leaf=tree_leaf{}
#         if (Transform := Leaf3.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=0.0, Y:=50.0, Z:=25.0}}
#         Leaf4:tree_leaf=tree_leaf{}
#         if (Transform := Leaf4.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=50.0, Z:=100.0}}
#         Leaf5:tree_leaf=tree_leaf{}
#         if (Transform := Leaf5.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=0.0, Z:=50.0}}
#         Leaf6:tree_leaf=tree_leaf{}
#         if (Transform := Leaf6.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=-50.0, Z:=75.0}}
#         Leaf7:tree_leaf=tree_leaf{}
#         if (Transform := Leaf7.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=0.0, Y:=-50.0, Z:=25.0}}
#         Leaf8:tree_leaf=tree_leaf{}
#         if (Transform := Leaf8.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=-50.0, Z:=100.0}}
#         Leaf9:tree_leaf=tree_leaf{}
#         if (Transform := Leaf9.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=25.0, Y:=75.0, Z:=50.0}}
#         Leaf10:tree_leaf=tree_leaf{}
#         if (Transform := Leaf10.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=25.0, Y:=25.0, Z:=75.0}}
#         Leaf11:tree_leaf=tree_leaf{}
#         if (Transform := Leaf11.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=75.0, Y:=25.0, Z:=25.0}}
#         Leaf12:tree_leaf=tree_leaf{}
#         if (Transform := Leaf12.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=75.0, Y:=75.0, Z:=100.0}}
#         Leaf13:tree_leaf=tree_leaf{}
#         if (Transform := Leaf13.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-25.0, Y:=75.0, Z:=50.0}}
#         Leaf14:tree_leaf=tree_leaf{}
#         if (Transform := Leaf14.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-25.0, Y:=-25.0, Z:=75.0}}
#         Leaf15:tree_leaf=tree_leaf{}
#         if (Transform := Leaf15.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=75.0, Y:=-25.0, Z:=25.0}}
#         Leaf16:tree_leaf=tree_leaf{}
#         if (Transform := Leaf16.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=75.0, Y:=75.0, Z:=100.0}}
#         Leaf17:tree_leaf=tree_leaf{}
#         if (Transform := Leaf17.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=25.0, Y:=-75.0, Z:=50.0}}
#         Leaf18:tree_leaf=tree_leaf{}
#         if (Transform := Leaf18.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=25.0, Y:=25.0, Z:=75.0}}
#         Leaf19:tree_leaf=tree_leaf{}
#         if (Transform := Leaf19.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-75.0, Y:=25.0, Z:=25.0}}
#         Leaf20:tree_leaf=tree_leaf{}
#         if (Transform := Leaf20.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-75.0, Y:=-75.0, Z:=100.0}}
#         Leaf21:tree_leaf=tree_leaf{}
#         if (Transform := Leaf21.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-25.0, Y:=-75.0, Z:=50.0}}
#         Leaf22:tree_leaf=tree_leaf{}
#         if (Transform := Leaf22.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-25.0, Y:=-25.0, Z:=75.0}}
#         Leaf23:tree_leaf=tree_leaf{}
#         if (Transform := Leaf23.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-75.0, Y:=-25.0, Z:=25.0}}
#         Leaf24:tree_leaf=tree_leaf{}
#         if (Transform := Leaf24.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-75.0, Y:=-75.0, Z:=100.0}}
#         Leaf25:tree_leaf=tree_leaf{}
#         if (Transform := Leaf25.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=0.0, Z:=150.0}}
#         Leaf26:tree_leaf=tree_leaf{}
#         if (Transform := Leaf26.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=50.0, Z:=175.0}}
#         Leaf27:tree_leaf=tree_leaf{}
#         if (Transform := Leaf27.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=0.0, Y:=50.0, Z:=125.0}}
#         Leaf28:tree_leaf=tree_leaf{}
#         if (Transform := Leaf28.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=50.0, Y:=50.0, Z:=200.0}}
#         Leaf29:tree_leaf=tree_leaf{}
#         if (Transform := Leaf29.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=0.0, Z:=150.0}}
#         Leaf30:tree_leaf=tree_leaf{}
#         if (Transform := Leaf30.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=-50.0, Z:=175.0}}
#         Leaf31:tree_leaf=tree_leaf{}
#         if (Transform := Leaf31.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=0.0, Y:=-50.0, Z:=125.0}}
#         Leaf32:tree_leaf=tree_leaf{}
#         if (Transform := Leaf32.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.1, Y:=0.1, Z:=0.1}, Translation:=vector3{X:=-50.0, Y:=-50.0, Z:=200.0}}

#         AttachComponents of array:
#             component(TransformComp)
#             component(AttachComp)

#         AttachEntities of array:
#             entity(BranchTruck)
#             entity(Leaf1)
#             entity(Leaf2)
#             entity(Leaf3)
#             entity(Leaf4)
#             entity(Leaf5)
#             entity(Leaf6)
#             entity(Leaf7)
#             entity(Leaf8)
#             entity(Leaf9)
#             entity(Leaf10)
#             entity(Leaf11)
#             entity(Leaf12)
#             entity(Leaf13)
#             entity(Leaf14)
#             entity(Leaf15)
#             entity(Leaf16)
#             entity(Leaf17)
#             entity(Leaf18)
#             entity(Leaf19)
#             entity(Leaf20)
#             entity(Leaf21)
#             entity(Leaf22)
#             entity(Leaf23)
#             entity(Leaf24)
#             entity(Leaf25)
#             entity(Leaf26)
#             entity(Leaf27)
#             entity(Leaf28)
#             entity(Leaf29)
#             entity(Leaf30)
#             entity(Leaf31)
#             entity(Leaf32)

# tree := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self }
#         AttachComp:attachment_component=attachment_component{ Entity := Self }
#         ReplicationComp:replication_component=replication_component{ Entity := Self }
#         MoverComp:mover_component=mover_component{ Entity := Self }
#         Trunk:tree_trunk=tree_trunk{}
#         if (Transform := Trunk.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Scale:=vector3{X:=0.25, Y:=0.25, Z:=3.0}, Translation:=vector3{X:=0.0, Y:=0.0, Z:=150.0}}

#         # TODO: MakeRotationFromYawPitchRoll was <converges> at one point, but changed to <varies> due to its dependence on trigonometric functions. Enable building by using IdentityRotation, instead.
#         Branch1:tree_branch_group=tree_branch_group{}
#         if (Transform := Branch1.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=40.0, Y:=0.0, Z:=300.0}, Rotation:=IdentityRotation()}
#         Branch2:tree_branch_group=tree_branch_group{}
#         if (Transform := Branch2.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=0.0, Y:=40.0, Z:=300.0}, Rotation:=IdentityRotation()}
#         Branch3:tree_branch_group=tree_branch_group{}
#         if (Transform := Branch3.GetComponent[transform_component]):
#             set Transform.InitialRelativeTransform = transform{ Translation:=vector3{X:=-40.0, Y:=-40.0, Z:=300.0}, Rotation:=IdentityRotation()}

#         AttachComponents of array:
#             component(TransformComp)
#             component(AttachComp)
#             component(ReplicationComp)
#             component(MoverComp)

#         AttachEntities of array:
#             entity(Trunk)
#             entity(Branch1)
#             entity(Branch2)
#             entity(Branch3)
