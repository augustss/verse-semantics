
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

##############################################################################################
# This VerseQA map is for demoing the Verse SceneGraph RoundManager and GameEvents feature.
# Verse SceneGraph RoundManager and GameEvents introduces a way to manage 
# the lifetime of experiences in conjunction with SceneGraph.
# It consists of the fort_round_manager interface and fort_game_events_component.
# QA Owner: Holly.Lucas
# Dev Owner: Fredrik.Lindh
##############################################################################################

########################################################
# To showcase that SceneGraph can operate in the same
# lifetime as a creative device's OnBegin function.
########################################################
QA_roundevents_device := class(creative_device):

    RoundMessage<localizes>:message = "fort_round_manager: Red Shroom appears when the round starts"

    @editable
    RoundBillboard:billboard_device := billboard_device{}

    OnBegin<override>()<suspends>:void=
        RoundBillboard.SetText(RoundMessage)