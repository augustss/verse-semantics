# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/VerseCLR }

##########################################################
# TEMP: Covenient utility to print out the entity structure
##########################################################
# KSTODO: Make this an actual object FormatDebugStringr instead of
# a glorified indenter. Move serailize logic to use object
# APIs like PrintField, StartArray, etc.
class_debug_formatter<public> := class:
    var Result<internal>:string = ""
    var Ind:int = 0
    var ArrayItem:logic = false

    Indent<public>()<transacts>:void = { set Ind += 1 }
    Deindent<public>()<transacts>:void = { set Ind = Max(Ind-1, 0) }

    IndStr<private>()<transacts>:string =
        var R:string = ""
        for (I := 0..Ind-1) { set R += "    " }
        return R

    StartArrayItem<public>()<transacts>:void =
        set ArrayItem = true

    Append<public>(Value:string)<transacts>:void =
        if (ArrayItem?):
            set Result += IndStr() + "- " + Value
            set ArrayItem = false
        else:
            set Result += IndStr() + Value

    AppendLine<public>(Value:string)<transacts>:void =
        Append(Value)
        set Result += "\n"

ToString<public>(S:class_debug_formatter)<transacts>:string = S.Result

##########################################################
# Object Model Core Classes
##########################################################
# KSTODO: This needs to be able to serialize recursively down
# the entity chain
entity_saved_state<public> := class<internal>:
    var OwnedEntities<internal>:dependent_array
    var Components<internal>:dependent_array

entity<public> := class<unique>:
    ##########################################################
    # Public Members
    ##########################################################
    var Name<public>:string
    var<private> ParentEntity<public>:?entity = false
    var OwnedEntities<public>:dependent_array
    var Components<public>:dependent_array

    ##########################################################
    # Default State
    # 
    # NOTE: These functions are used to stub how we might
    # class defaults being overridden through merging (vs. replacement).
    # In the final API we would more likely introduce something more
    # elegant like attributes on the types that allow for properties 
    # to be "merged when overridden"
    # 
    # Example: BaseClass = [A,B]
    #          DerivedClass = [A,-B,C] 
    # - Need to store removal of B and add of C on DerivedClass
    # - If BaseClass later changes to have another value, that change
    #   would need to merge with DerivedClass.
    ##########################################################
    GetDefaultEntities<protected>()<transacts>:dependent_array = MakeDependentArray(0)
    GetDefaultComponents<protected>()<transacts>:dependent_array = MakeDependentArray(0)

    ##########################################################
    # Lifetime Functions
    ##########################################################
    Init()<transacts>:void =
        set OwnedEntities = GetDefaultEntities()
        for (E : OwnedEntities.GetArray(); E2 := entity_crdt_val[E]) { E2.V.Init() }
        set Components = GetDefaultComponents()

    Init(SavedState:entity_saved_state)<transacts>:void =
        DefaultEntities := GetDefaultEntities()
        SavedStateEntities := SavedState.OwnedEntities.Clone()
        SavedStateEntities.MergeDisconnectedChangesFromBaseArray(DefaultEntities)
        set OwnedEntities = SavedStateEntities
        for (E : OwnedEntities.GetArray(); E2 := entity_crdt_val[E]) { E2.V.Init() }

        DefaultComponents := GetDefaultComponents()
        SavedStateComponents := SavedState.Components.Clone()
        SavedStateComponents.MergeDisconnectedChangesFromBaseArray(DefaultComponents)
        set Components = SavedStateComponents

    ##########################################################
    # Gameplay Functions
    ##########################################################
    AddComponent<public>(C:component)<decides><transacts>:void =
        Components.Append[MakeComponentCrdt(C)]

    RemoveComponent<public>(C:component)<decides><transacts>:void =
        I := FindComponentIndex(C, Components)
        I >= 0
        Components.Remove[I]

    AddEntity<public>(E:entity)<decides><transacts>:void =
        set E.ParentEntity = option { Self }
        OwnedEntities.Append[MakeEntityCrdt(E)]

    RemoveEntity<public>(E:entity)<decides><transacts>:void =
        I := FindEntityIndex(E, OwnedEntities)
        I >= 0
        OwnedEntities.Remove[I]

    ##########################################################
    # Test Utilities
    ##########################################################
    FormatDebugString<public>(Builder:class_debug_formatter)<transacts>:void =
        Builder.AppendLine("entity:")
        Builder.Indent()
        Builder.AppendLine("Name: '{Name}'")

        if (Components.Length() > 0):
            Builder.AppendLine("Components:")
            Builder.Indent()
            for (I : Components.GetArray(); C := component_crdt_val[I].V):
                Builder.StartArrayItem()
                C.FormatDebugString(Builder)
                Builder.AppendLine("")
            Builder.Deindent() # components
        else:
            Builder.AppendLine("Components: []")

        if (OwnedEntities.Length() > 0):
            Builder.AppendLine("Entities:")
            Builder.Indent()
            for (I : OwnedEntities.GetArray(); ChildEntity := entity_crdt_val[I].V):
                Builder.StartArrayItem()
                ChildEntity.FormatDebugString(Builder)
            Builder.Deindent() # Entities
        else:
            Builder.AppendLine("Entities: []")

        Builder.Deindent() # entity

ToString<public>(E:entity)<transacts>:string =
    Builder := class_debug_formatter{}
    E.FormatDebugString(Builder)
    return Builder.Result

MakeEntity<public><constructor>(Name:string)<transacts> := entity:
    Name := Name
    OwnedEntities := MakeDependentArray(0)
    Components := MakeDependentArray(0)

entity_crdt_val<public> := class(crdt_value_interface):
    V<public>:entity
    Stringify<override>()<transacts>:string = "{V}"

MakeEntityCrdt<public><constructor>(Entity:entity)<transacts> := entity_crdt_val:
    V := Entity

component<public> := class<unique>:
    Name<public>:string

    FormatDebugString<public>(Builder:class_debug_formatter)<transacts>:void =
        Builder.Append("component: \{ Name: '{Name}' \}")

ToString<public>(C:component)<transacts>:string =
    Builder := class_debug_formatter{}
    C.FormatDebugString(Builder)
    return "{Builder}"

MakeComponent<public><constructor>(Name:string)<transacts> := component:
    Name := Name

component_crdt_val := class(crdt_value_interface):
    V:component
    Stringify<override>()<transacts>:string = "{V}"
MakeComponentCrdt<public><constructor>(Component:component)<transacts> := component_crdt_val:
    V := Component

FindComponentIndex<public>(C:component, Array:dependent_array)<transacts>:int =
    for (I := 0..Array.Length()-1):
        if (Item := component_crdt_val[Array.Get[I]]; Item.V = C):
            return I
    return -1

FindEntityIndex<public>(E:entity, Array:dependent_array)<transacts>:int =
    for (I := 0..Array.Length()-1):
        if (Item := entity_crdt_val[Array.Get[I]]; Item.V = E):
            return I
    return -1

##########################################################
# PREFABS
# 
# KSTODO:
# Stable keying mechanism to account for cases where
# new classes are injected into the class hierarchy 
# over time.
##########################################################

tree_trunk<public> := class(entity):
    TransformComp<public>:component = component{ Name := "Transform" }
    PhysicsComp<public>:component = component{ Name := "Physics" }
    StaticMeshComp<public>:component = component{ Name := "Mesh" }

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()
        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(TransformComp)]
            DefaultComponents.Append[MakeComponentCrdt(PhysicsComp)]
            DefaultComponents.Append[MakeComponentCrdt(StaticMeshComp)]
        return DefaultComponents

MakeTreeTrunk<public><constructor>(Name:string)<transacts> := tree_trunk:
    MakeEntity<constructor>(Name)

tree_leaf<public> := class(entity):
    TransformComp<public>:component = component{ Name := "Transform" }
    PhysicsComp<public>:component = component{ Name := "Physics" }
    StaticMeshComp<public>:component = component{ Name := "Mesh" }

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()
        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(TransformComp)]
            DefaultComponents.Append[MakeComponentCrdt(PhysicsComp)]
            DefaultComponents.Append[MakeComponentCrdt(StaticMeshComp)]
        return DefaultComponents

MakeTreeLeaf<public><constructor>(Name:string)<transacts> := tree_leaf:
    MakeEntity<constructor>(Name)

tree_branch<public> := class(entity):
    TransformComp<public>:component = component{ Name := "Transform" }
    PhysicsComp<public>:component = component{ Name := "Physics" }
    StaticMeshComp<public>:component = component{ Name := "Mesh" }

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()
        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(TransformComp)]
            DefaultComponents.Append[MakeComponentCrdt(PhysicsComp)]
            DefaultComponents.Append[MakeComponentCrdt(StaticMeshComp)]
        return DefaultComponents

MakeTreeBranch<public><constructor>(Name:string)<transacts> := tree_branch:
    MakeEntity<constructor>(Name)

tree_branch_group<public> := class(entity):
    TransformComp<public>:component = component{ Name := "Transform" }
    Branch1<public>:tree_branch
    Leaf1<public>:tree_leaf
    Leaf2<public>:tree_leaf

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()
        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(TransformComp)]
        return DefaultComponents

    GetDefaultEntities<override>()<transacts>:dependent_array =
        BaseDefaultEntities := (super:)GetDefaultEntities()
        DefaultEntities := BaseDefaultEntities.GetDependentArray()
        if:
            DefaultEntities.Append[MakeEntityCrdt(Branch1)]
            # KSTODO: Would we want to make the leaves children of the Branch?
            DefaultEntities.Append[MakeEntityCrdt(Leaf1)]
            DefaultEntities.Append[MakeEntityCrdt(Leaf2)]
        return DefaultEntities

MakeTreeBranchGroup<public><constructor>(Name:string)<transacts> := tree_branch_group:
    MakeEntity<constructor>(Name)
    Branch1 := MakeTreeBranch("Branch")
    Leaf1 := MakeTreeLeaf("Leaf1")
    Leaf2 := MakeTreeLeaf("Leaf2")

tree<public> := class(entity):
    TransformComp<public>:component = component{ Name := "Transform" }
    PhysicsComp<public>:component = component{ Name := "Physics" }
    StaticMeshComp<public>:component = component{ Name := "Mesh" }

    Branch1<public>:tree_branch_group
    Branch2<public>:tree_branch_group

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()
        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(TransformComp)]
            DefaultComponents.Append[MakeComponentCrdt(PhysicsComp)]
            DefaultComponents.Append[MakeComponentCrdt(StaticMeshComp)]
        return DefaultComponents

    GetDefaultEntities<override>()<transacts>:dependent_array =
        BaseDefaultEntities := (super:)GetDefaultEntities()
        DefaultEntities := BaseDefaultEntities.GetDependentArray()
        if:
            DefaultEntities.Append[MakeEntityCrdt(Branch1)]
            DefaultEntities.Append[MakeEntityCrdt(Branch2)]
        return DefaultEntities

MakeTree<public><constructor>(Name:string)<transacts> := tree:
    MakeEntity<constructor>(Name)
    Branch1 := MakeTreeBranchGroup("Branch1")
    Branch2 := MakeTreeBranchGroup("Branch2")

tree2<public> := class(tree):
    LightComponent:component = component{ Name := "Light" }

    GetDefaultComponents<override>()<transacts>:dependent_array =
        BaseDefaultComponent := (super:)GetDefaultComponents()

        DefaultComponents := BaseDefaultComponent.GetDependentArray()
        if:
            DefaultComponents.Append[MakeComponentCrdt(LightComponent)]
            DefaultComponents.Remove[FindComponentIndex(PhysicsComp, DefaultComponents)]

        return DefaultComponents

MakeTree2<public><constructor>(Name:string)<transacts> := tree2:
    MakeTree<constructor>(Name)

##########################################################
# TESTS
##########################################################

TestTree():logic =
    block:
        T1 := MakeTree("T1")
        T1.Init()

        Builder := class_debug_formatter{}
        T1.FormatDebugString(Builder)
        Print("\n{Builder}")

    block:
        T2 := MakeTree2("T2")
        T2.Init()

        Builder := class_debug_formatter{}
        T2.FormatDebugString(Builder)
        Print("\n{Builder}")

    return true

##########################################################
# TEST SUITE
##########################################################

DependentArrayInClassesTestSuite():[]test_case =
    array:
        test_case{ Name := "TestTree", Func := TestTree }
