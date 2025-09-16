using { /Fortnite.com/Devices }
using { Controllers }
using { Entities }


EventsBus<public> := class(creative_device):

    RaceStartEvent<public>:event() = event(){}

    LocalRainsInitEvent<public>:event([]vfx_spawner_device) = event([]vfx_spawner_device){}

    LightingsInitEvent<public>:event([]LightingConfig) = event([]LightingConfig){}

    LeavesInitEvent<public>:event([]LeavesConfig) = event([]LeavesConfig){}

    PlayersInitEvent<public>():event([]PlayerWrapper) = event([]PlayerWrapper){}

    DTOs<public>:DTOBus = DTOBus{}



DTOBus<public> := class:
    var Players<public>:?[]PlayerWrapper = false

