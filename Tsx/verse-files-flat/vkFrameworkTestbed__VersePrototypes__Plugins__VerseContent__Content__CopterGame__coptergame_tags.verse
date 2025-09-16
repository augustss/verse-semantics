# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Assets }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }

coptergame_tag<epic_internal> := class(tag){}
coptergame_player_tag<epic_internal> := class(coptergame_tag){}

coptergame_part_tag<epic_internal> := class(coptergame_tag){}
coptergame_part_body_tag<epic_internal> := class(coptergame_part_tag){}
coptergame_part_mainrotor_tag<epic_internal> := class(coptergame_part_tag){}
coptergame_part_tailrotor_tag<epic_internal> := class(coptergame_part_tag){}

coptergame_camera_tag := class<concrete>(coptergame_tag){}
coptergame_world_camera1_tag<epic_internal> := class<concrete>(coptergame_camera_tag){}
coptergame_world_camera2_tag<epic_internal> := class<concrete>(coptergame_camera_tag){}
coptergame_world_camera3_tag<epic_internal> := class<concrete>(coptergame_camera_tag){}
coptergame_world_camera4_tag<epic_internal> := class<concrete>(coptergame_camera_tag){}
coptergame_world_camera5_tag<epic_internal> := class<concrete>(coptergame_camera_tag){}

@experimental
verse_camera_mode_coptergame_tag<epic_internal> := class(verse_camera_mode_tag){}
@experimental
verse_camera_mode_coptergame_start_tag<epic_internal> := class(verse_camera_mode_coptergame_tag){}
@experimental
verse_camera_mode_coptergame_main_tag<epic_internal> := class(verse_camera_mode_coptergame_tag){}
@experimental
verse_camera_mode_coptergame_underchopper_tag<epic_internal> := class(verse_camera_mode_coptergame_tag){}