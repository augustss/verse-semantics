# using { /Verse.org/Assets }
# using { /Verse.org/SceneGraph }
# using { /UnrealEngine.com/Temporary/SpatialMath }

# BoopSound<epic_internal>:sound = MakeAsset(sound, "/CoreEntityPrototype/Sounds/TestBoop.TestBoop") 
# BoopSphereMesh<public>:mesh = MakeAsset(mesh, "/Engine/BasicShapes/Sphere.Sphere")

# boop := class(entity):
#     block:
#         TransformComp:transform_component=transform_component{ Entity := Self }
#         SoundComp:sound_component= sound_component:
#             Entity := Self
#             Sound:=BoopSound
#         StaticMeshComp:static_mesh_component= static_mesh_component:
#             Entity := Self
#             Mesh:=BoopSphereMesh
#             Material:=option{ MakeAsset(material_asset, "/Engine/BasicShapes/BasicShapeMaterial.BasicShapeMaterial") }
#         RepeatingSoundComp:repeating_sound_component=repeating_sound_component:
#             Entity := Self
#             RepeatTime:= 1.0

#         AttachComponents of array:
#             component(TransformComp)
#             component(SoundComp)
#             component(StaticMeshComp)
#             component(RepeatingSoundComp)