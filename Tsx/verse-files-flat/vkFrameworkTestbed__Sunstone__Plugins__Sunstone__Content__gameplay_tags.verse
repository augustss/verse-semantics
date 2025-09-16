using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }

player_profile_manager_tag := class<public>(tag){}
global_events_manager_tag := class<public>(tag){}

# Element Tags
tower_element_tag := class(tag){}
no_tower_element := class(tower_element_tag){}
arcane_tower_element := class(tower_element_tag){}
fire_tower_element := class(tower_element_tag){}
ice_tower_element := class(tower_element_tag){}
electric_tower_element := class(tower_element_tag){}

# Tower level Tags
tower_level_tag := class(tag){}
tower_level_none := class(tower_level_tag){}
tower_level_one := class(tower_level_tag){}
tower_level_two := class(tower_level_tag){}
tower_level_three := class(tower_level_tag){}

shield_tag := class(tag){}
damage_tag := class(tag){}

# VFX Tags
vfx_tag := class(tag){}
target_impact_vfx_tag := class(vfx_tag){}
targeting_vfx_tag := class(vfx_tag){}
muzzle_flash_vfx_tag := class(vfx_tag){}
explosion_vfx_tag := class(vfx_tag){}
explosion_2_vfx_tag := class(vfx_tag){}
explosion_3_vfx_tag := class(vfx_tag){}
level_up_vfx_tag := class(vfx_tag){}
level_up_2_vfx_tag := class(vfx_tag){}
beam_vfx_tag := class(vfx_tag){}
