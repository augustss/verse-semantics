# Additional imports that you may find useful

using { /Verse.org }
using { /Verse.org/Assets }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }

PhysicsApiTags<public> := module:

    @experimental
    query_object_tag<epic_internal> := class(tag) {}

    @experimental
    detectable_object_tag<epic_internal> := class(tag) {}
