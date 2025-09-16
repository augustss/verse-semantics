UnrealEngine := import(/UnrealEngine.com)
Fortnite := import(/Fortnite.com)
Verse := import(/Verse.org)
# LeeLovesChrist := import (/leeloveschrist_3@fortnite.com/QAMap_Verse_Path)

# local_internal := scoped {/brandinobrando@fortnite.com/QAMap_Verse_Path}
my_internal := scoped{MyClassDebug}
# persistence_enum_ref := enum (/leeloveschrist_3@fortnite.com/QAMap_Verse_Path/PersistenceModule:)FooFunc():void= Print("")


(MyClassDebug:)MyWorldDebug<my_internal> := module:
    (MyClassDebug.base_module.sub_module.mini_module.my_class:)MyClassStuff<my_internal> := module: