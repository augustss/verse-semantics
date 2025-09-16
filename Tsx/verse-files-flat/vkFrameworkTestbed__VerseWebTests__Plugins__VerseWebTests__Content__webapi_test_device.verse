# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/VerseInternalTesting/VerseFramework}
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com }

# The WebAPI tests require the following set in Engine.ini:
# [VerseWebAPI.invaliddomain.VerseWebTests.TestWebAPI.test_client_id]
# BaseURL="https://www.example.com"
# +Resources=news
#
# [VerseWebAPI.invaliddomain.VerseWebTests.TestWebAPI.test_client_id.news]
# Verb=GET
# Path="/news?date=now"

TestWebAPI := module:
    test_client_id<internal> := class<final><computes>(WebAPI.client_id){}
    TestWebClient<internal>:WebAPI.client = WebAPI.MakeClient(test_client_id{})

    unconfigured_client_id<internal> := class<final><computes>(WebAPI.client_id){}
    UnconfiguredTestWebClient<internal>:WebAPI.client = WebAPI.MakeClient(unconfigured_client_id{})

    webapi_test_device<epic_internal> := class(test_device):
        ##########################################################
        # TEST CLASS SETUP
        ##########################################################
        SuiteName<override>:string = "WebAPITestSuite"

        # All tests need to be included in this array in order to be run.
        # The test category will allow for filtering tests. Currently, filters are not enforced,
        # but the `test_runner_device` specifically will run "editor" and "no_editor" tests.
        TestCases<override>():[]test_case = 
            array:
                test_case{ Name := "TestWebAPI_MakeRequest", Func := TestWebAPI_MakeRequest, Category := array{"editor"} }
                test_case{ Name := "TestWebAPI_MakeRequest_InvalidResource", Func := TestWebAPI_MakeRequest_InvalidResource, Category := array{"editor"} }
                test_case{ Name := "TestWebAPI_MakeUnconfiguredRequest", Func := TestWebAPI_MakeUnconfiguredRequest, Category := array{"editor"} }

        ##########################################################
        # TESTS
        ##########################################################
        TestWebAPI_MakeRequest()<suspends>:void =
            Response := TestWebClient.Get("news")
            TestRunner.CheckTrue(logic{WebAPI.body_response[Response]}, "Make Request")

        TestWebAPI_MakeRequest_InvalidResource()<suspends>:void =
            Response := TestWebClient.Get("invalid_resource")
            TestRunner.CheckTrue(logic{not WebAPI.body_response[Response]}, "Make Request to Invalid Resource")

        TestWebAPI_MakeUnconfiguredRequest()<suspends>:void =
            Response := UnconfiguredTestWebClient.Get("news")
            TestRunner.CheckTrue(logic{not WebAPI.body_response[Response]}, "Make Unconfigured Request")                    
