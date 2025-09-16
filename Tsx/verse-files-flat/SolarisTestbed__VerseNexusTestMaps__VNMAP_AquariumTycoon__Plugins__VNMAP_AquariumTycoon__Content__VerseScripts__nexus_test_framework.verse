using { /Fortnite.com/VerseInternalTesting/NexusFramework }
nexus_test_runner_alias := class<epic_internal>(nexus_test_runner){}

# using { /Fortnite.com/Devices }
# using { /Verse.org/Simulation }
# using { /Verse.org/Colors }
# using { /UnrealEngine.com/Temporary/Diagnostics }

# result_data<epic_internal> := enum:

#     Skipped,
#     Passed,
#     Failed

# nexus_test_runner := class<epic_internal>(creative_device):
	
#     # Test suites have a map that contains nexus_test's
#     # We will use strings to make it as user friendly as possible
#     var NexusTestSuites:[string]nexus_test_suite = map{}

#     # A method to register all tests that assign to NexusTestSuites
#     var NexusTestDevices:[]nexus_test_device = array{}

#     AddDeviceToTestRunner(NexusTestDevice:nexus_test_device):void=

#         set NexusTestDevices += array{NexusTestDevice}

#     RegisterTestSuites()<suspends>:void=

#         # Reset Test Suites
#         set NexusTestSuites = map{}

#         for(NexusTestDevice:NexusTestDevices):

#             if:
#                 set NexusTestSuites[NexusTestDevice.GetTestSuiteName()] = NexusTestDevice.TestSuite
#             then:
#                 NexusTestDevice.TestSuite.RegisterTests()

#                 Print("{NexusTestDevice.GetTestSuiteName()} was added to NexusTestSuites map")
    
#     AddTestResults(TestSuiteName:string,TestName:string, NexusResultData:nexus_result_data)<transacts>:void=
        
#         if:
#             NexusTestSuite:=NexusTestSuites[TestSuiteName]
#         then:
#             if:
#                 NexusTestSuite.NexusTestsMap[TestName]
#             then:
#                 if:
#                     NexusTestToAddTo:=NexusTestSuite.NexusTestsMap[TestName]
#                 then:
#                     NexusTestToAddTo.AddResult(NexusResultData)
#                     if(NexusResultData.ResultType=result_data.Failed):
#             else:
#                 Print("Adding {TestName} to LooseTests in {TestSuiteName}")
#                 # If a test doesn't exist, create a loose test
#                 if:
#                     LooseTest:= NexusTestSuite.LooseTests[TestName] 
#                 then:
#                     LooseTest.AddResult(NexusResultData)
#                     if(NexusResultData.ResultType=result_data.Failed):
#                         # Print("NexusError - {NexusResultData.ContextString}")
#                 else:
                    
#                     if:
#                         NexusTestToAddTo:= set NexusTestSuite.LooseTests[TestName] = nexus_test{Results:=array{NexusResultData}}
#                         if(NexusResultData.ResultType=result_data.Failed):
#         else:

#             Err("{TestSuiteName} not found in the map in AddTestResults")

#     RemoveTestResult(TestSuiteName:string,TestName:string, ContextString:string)<transacts>:void=

#         if:
#             NexusTestSuite:=NexusTestSuites[TestSuiteName]
#         then:
#             if:
#                 NexusTestSuite.NexusTestsMap[TestName]
#             then:
#                 if:
#                     var NexusTestToRemoveFrom :nexus_test=NexusTestSuite.NexusTestsMap[TestName]
#                 then:
#                     var NewNexusTest :nexus_test= MakeNexusTest(TestName)
#                     var HasNameBeenRemoved : logic = false

#                     for(Result:NexusTestToRemoveFrom.Results):
                
#                             if(ContextString = Result.ContextString and not HasNameBeenRemoved?):
#                                 Print("Result {Result.ContextString} has been removed")
#                                 set HasNameBeenRemoved = true
#                             else:
#                                 set NewNexusTest.Results += array{Result}

#                     set NexusTestToRemoveFrom = NewNexusTest
#                     logic{set NexusTestSuite.NexusTestsMap[TestName] = NewNexusTest}

#             else:
#                 if:
#                     var LooseTestToRemoveFrom :nexus_test=NexusTestSuite.LooseTests[TestName]
#                 then:
#                     var NewNexusTest :nexus_test= MakeNexusTest(TestName)
#                     var HasNameBeenRemoved : logic = false

#                     for(Result:LooseTestToRemoveFrom.Results):
                
#                             if(ContextString = Result.ContextString and not HasNameBeenRemoved?):
#                                 Print("Result {Result.ContextString} has been removed")
#                                 set HasNameBeenRemoved = true
#                             else:
#                                 set NewNexusTest.Results += array{Result}
                        
#                     set LooseTestToRemoveFrom = NewNexusTest
#                         logic{set NexusTestSuite.LooseTests[TestName] = NewNexusTest}

#     OnBegin<override>()<suspends>:void=

#         Sleep(0.5)
#         RegisterTestSuites()

#     OnEnd<override>():void=

#         DisplayResults()

#     DisplayResults()<transacts>:void=

#         var ResultsString : string = "\n==========================\n====   Test Results   ===="

#         var Passed : int = 0
#         var Failed : int = 0
#         var Total : int = 0
        
#         for(TestSuiteName->TestSuite:NexusTestSuites):

#             if(DetectedCrash:=TestSuite.CrashTest?):
                
#                 set ResultsString += "NexusCrash - {DetectedCrash.Name}"
#                 AddTestResults(TestSuiteName, DetectedCrash.Name,NexusResults(DetectedCrash.Name,result_data.Failed))
#             var TestSuiteResultsString : string = "\n==========================\n{TestSuiteName}:"

    
#             HumanTestData := ParseDataForHumans(TestSuite.NexusTestsMap)

#             set TestSuiteResultsString+= HumanTestData(0)
#             set Passed += HumanTestData(1)
#             set Failed += HumanTestData(2)
#             set Total += HumanTestData(3)


#             HumanLooseTestData := ParseDataForHumans(TestSuite.LooseTests)

#             if(HumanLooseTestData(0).Length<>0):
#                 set TestSuiteResultsString += "\n==== Loose Tests Detected ====\n"

#             set TestSuiteResultsString+= HumanLooseTestData(0)
#             set Passed += HumanLooseTestData(1)
#             set Failed += HumanLooseTestData(2)
#             set Total += HumanLooseTestData(3)


#             set ResultsString += TestSuiteResultsString
            
            
#         set ResultsString += "\n============\n"
        
#         PassedPercentage:float= (Passed*1.0)/(Total*1.0)
#         FailedPercentage:float= (Failed*1.0)/(Total*1.0)

#         set ResultsString += "\n{PassedPercentage*100.0}% Passed {FailedPercentage*100.0}% Failed"

#         Print("{ResultsString}")


# nexus_test := class<epic_internal>:
	
# 	Name: string = "Test"
# 	var Results:[]nexus_result_data=array{}

#     AddResult(NewResult:nexus_result_data)<transacts>:void=

#         set Results += array{NewResult} 
    
#     GetResultsFromType(ResultType:result_data)<transacts>:int=

#         var Ret:int=0
#         for(ResultData:Results):

#             case(ResultData.ResultType):

#                 result_data.Skipped => if(ResultType = result_data.Skipped) then set Ret += 1 
#                 result_data.Passed =>  if(ResultType = result_data.Passed) then set Ret += 1 
#                 result_data.Failed =>  if(ResultType = result_data.Failed) then set Ret += 1 

#         return Ret
    
#     GetResultsMetaData(ResultType:result_data)<transacts>:string=

#         var Ret : string = ""

#         var _ResultsMap : [string]int = map{}
        
#         # Add results to _ResultsMap
#         for(Result:Results):

#             if:
#                 ResultType = Result.ResultType
#                 not _ResultsMap[Result.ContextString]
#                 set _ResultsMap[Result.ContextString] = 1
#             else:
#                 if(set _ResultsMap[Result.ContextString] += 1){}
        
#         # Add _ResultsMap elements to Ret string
#         for(ResultMessage->ResultQuantity:_ResultsMap):

#             set Ret += "\n\t\t\t{ResultQuantity}x - {ResultMessage}"

#         return Ret

        
# nexus_result_data := class<epic_internal>:

# 	ContextString<public> : string = "Test ended"
# 	ResultType : result_data = result_data.Skipped


# NexusResults<constructor><epic_internal>(NewContextString:string, NewResultData:result_data)<transacts> := nexus_result_data:

#     ContextString:=NewContextString
#     ResultType:=NewResultData


# MakeNexusTest<constructor><epic_internal>(NewTestName:string)<transacts> := nexus_test:

#     Name := NewTestName


# nexus_test_suite := class<epic_internal><concrete>:

#     @editable
#     Name : string = "your_device_name"

#     var NexusTestsMap:[string]nexus_test = map{}

#     var LooseTests:[string]nexus_test = map{}

#     var CrashTest :?nexus_test = false

#     GetNexusTests()<transacts>:[]nexus_test=

#         var NexusTests : []nexus_test = array{}

#         for(TestName->NexusTest:NexusTestsMap):

#             set NexusTests += array{NexusTest}

#         return NexusTests

#     RegisterTests(TestsToRegister:[]nexus_test):void=

#         for(Test:TestsToRegister):
#             if(set NexusTestsMap[Test.Name] = Test):


# nexus_test_device := class<epic_internal>(creative_device):

#     @editable 
#     TestSuite:nexus_test_suite = nexus_test_suite{}

#     @editable
#     var TestRunner<public>:nexus_test_runner = nexus_test_runner{}

#     Tests<public>():[]nexus_test= array{}
     
#     GetTest(TestIndex:int)<transacts>:nexus_test=

#         var NexusTestToReturn:nexus_test=nexus_test{}

#         if(set NexusTestToReturn = TestSuite.GetNexusTests()[TestIndex]):
#         else:
#             Print("NexusUserError - {TestSuite.Name} does not contain a test with an index of {TestIndex}")

#         return NexusTestToReturn


#     GetTest(TestName:string)<transacts>:nexus_test=

#         var NexusTestToReturn:nexus_test=nexus_test{}

#         # Check if it exists in Nexus Tests Map
#         if(set NexusTestToReturn = TestSuite.NexusTestsMap[TestName]):
#         else:
#             #Check if it exists in LooseTests
#             if(set NexusTestToReturn = TestSuite.LooseTests[TestName]):
#             else:
#                 return MakeNexusTest("-")

#         return NexusTestToReturn

#     # ? Should we change AddResults to be Check just like in C++
#     AddResults(TestName:string, ResultsData:nexus_result_data)<transacts>:void=

#         NexusResultData :=NexusResults(ResultsData.ContextString,ResultsData.ResultType)
        
#         TestRunner.AddTestResults(TestSuite.Name, TestName,NexusResultData)

#         if(ResultsData.ResultType = result_data.Failed):
#             Print("NexusError - {TestName} - {NexusResultData.ContextString}")

#     # ? Should we change AddResults to be Check just like in C++
#     AddResults(TestName:string, Passed:logic, ContextString:string)<transacts>:void=

#         if(Passed?):
#            TestRunner.AddTestResults(TestSuite.Name, TestName, NexusResults(ContextString,result_data.Passed))
#         else:
#             TestRunner.AddTestResults(TestSuite.Name, TestName,NexusResults(ContextString,result_data.Failed))
#             Print("NexusError - {TestName} - {ContextString}")


#     # ? Should we change AddResults to be Check just like in C++
#     AddResults(TestIndex:int, Passed:logic, ContextString:string)<transacts>:void=

#         if(Passed?):
#             TestRunner.AddTestResults(TestSuite.Name, GetTest(TestIndex).Name, NexusResults(ContextString,result_data.Passed))
#         else:
#             TestRunner.AddTestResults(TestSuite.Name, GetTest(TestIndex).Name, NexusResults(ContextString,result_data.Failed))
#             Print("NexusError - {GetTest(TestIndex).Name} - {ContextString}")


#     RemoveResults(TestName:string, ContextString:string)<transacts>:void=

#         TestRunner.RemoveTestResult(TestSuite.Name,TestName, ContextString)


#     SetupTest(TestName:string, IsCrashTest:logic)<transacts>:void=

#         var NexusTest:nexus_test= GetTest(TestName)
        
#         #If the test doesn't exist, add a loose test, set NexusTest to the loose test
#         #TODO: Replace "Test is pending..." with a member default variable 
#         if(NexusTest.Name = "-"):
#             TestRunner.AddTestResults(TestSuite.Name, TestName, NexusResults("Test is pending...",result_data.Failed))
#             set NexusTest = GetTest(TestName)
#             Print("LogWes: Unnamed Test Detected")
#         else:
#             NexusTest.AddResult(NexusResults("Test is pending...", result_data.Failed))
            
#         if(IsCrashTest?):
#             TestRunner.DisplayResults()
#             set TestSuite.CrashTest = option:
#                 GetTest(TestName)

#     # TODO: Add Timeout (not need for v1)
#     # TimeoutTest(TimeoutTime:float)<suspends>:void=
#     #     Sleep(TimeoutTime)

#     CompleteTest(TestName:string, ContextString:string, IsCrashTest:logic)<transacts>:void=

#         AddResults(TestName, true, ContextString)
#         RemoveResults(TestName, "Test is pending...")

#         if(IsCrashTest?, TestName=TestSuite.CrashTest?.Name):
#             set TestSuite.CrashTest = false

#     OnBegin<override>()<suspends>:void=
#         TestRunner.AddDeviceToTestRunner(Self)
#         Print("Nexus OnBegin - {TestSuite.Name} OnBegin")
#         Sleep(1.0)
#         TestSuite.RegisterTests(Tests())

#     OnEnd<override>():void=
#         Print("Nexus OnEnd - {TestSuite.Name} OnEnd")

# # * Helpers * #

# (NexusTestDevice:nexus_test_device).GetTestSuiteName<epic_internal>()<transacts> : string =  NexusTestDevice.TestSuite.Name

# RemoveKeyFromMap(ExampleMap:[string]nexus_test, ElementToRemove:string):[string]nexus_test=
#     var NewMap:[string]nexus_test = map{}
#     # Concatenate Keys from ExampleMap into NewMap, excluding ElementToRemove
#     for (Key -> Value : ExampleMap, Key <> ElementToRemove):
#         set NewMap = ConcatenateMaps(NewMap, map{Key => Value})
#     return NewMap

# RemoveResult(NexusTest:nexus_test, TestName:string, ContextString:string):nexus_test=

#     var NewNexusTest : nexus_test = MakeNexusTest(TestName)
#     var HasNameBeenRemoved : logic = false

#     for(Result:NexusTest.Results):

#         if(HasNameBeenRemoved?):
#             if(ContextString = Result.ContextString and not HasNameBeenRemoved?):
#                 set HasNameBeenRemoved = true
#             else:
#                 set NewNexusTest.Results += array{Result}

#     Print("Passed {NewNexusTest.GetResultsMetaData(result_data.Passed)}")
#     Print("Failed {NewNexusTest.GetResultsMetaData(result_data.Failed)}")

#     return NewNexusTest

# # Parse Helpers
# ParseDataForHumans(Map:[string]nexus_test)<transacts>:tuple(string, int, int, int)=

#     var TestSuiteResultsString:string = ""
#     var PassFailTotal: tuple(int, int, int)= (0,0,0)

#     for(NexusTestName->NexusTest:Map):
                
#         TestsPassedLength : int = NexusTest.GetResultsFromType(result_data.Passed)
#         TestsFailedLength : int = NexusTest.GetResultsFromType(result_data.Failed)
#         TotalTestsLength : int = TestsPassedLength + TestsFailedLength
        
#         set TestSuiteResultsString += "\n\t{NexusTestName}-"

#         if(TotalTestsLength <= 0):
#             set TestSuiteResultsString += "\n\t\t[0/0] - Skipped"
#         else:
#             set TestSuiteResultsString += ("\n\t\t[{TestsPassedLength}/{TotalTestsLength}] - Passed")
#             set TestSuiteResultsString += ("{NexusTest.GetResultsMetaData(result_data.Passed)}")

#             set TestSuiteResultsString += ("\n\t\t[{TestsFailedLength}/{TotalTestsLength}] - Failed")
#             set TestSuiteResultsString += ("{NexusTest.GetResultsMetaData(result_data.Failed)}")
        
#         set PassFailTotal = (
#         PassFailTotal(0)+TestsPassedLength,
#         PassFailTotal(1)+TestsFailedLength,
#         PassFailTotal(2)+TotalTestsLength
#         )

    
        
#     return (TestSuiteResultsString,PassFailTotal(0), PassFailTotal(1), PassFailTotal(2))

# #TODO: Add elapsed time to test results

# # * Example Device * #
# # All classes that use the framework will need to inherit from nexus_test_device
# example_nexus_device := class(nexus_test_device):

#     # To add tests, override this function, feel free to copy and paste this line
#     Tests<override>():[]nexus_test= array:
#         MakeNexusTest("Example Test")
#         MakeNexusTest("Crash Test")

#     # OnBegin is required with (super:)OnBegin() being necessary for the first line in it
#     OnBegin<override>()<suspends>:void=
#         (super:)OnBegin() # * All devices will need this line

#         # * To add tests to a result you follow the signature TestName:string, IsPassed:logic, ContextString:string
#         AddResults("Example Test", true, "Example Test passed in OnBegin")

#         # * You can add results using the index of Tests:[]nexus_test
#         AddResults(0, true, "Example Test passed in OnBegin")

#         # * Now let's fail a test!
#         AddResults("Example Test", false, "Example Test failed intentionally")

#         # * If you use AddResults and the test doesn't exist in Tests<override>():[]nexus_test, it will enter the LooseTests bucket
#         AddResults("Loose Test Example",true,"Loose Test Context String in OnBegin()")

#         # * You can set up/Complete tests, the logic var indicates whether or not it's a crash setup
#         SetupTest("Sample Setup / Completed test", false)
#         CompleteTest("Sample Setup / Completed test", "Test completed in OnBegin", false)

#     # OnEnd is required with (super:)OnEnd() being necessary for the first line in it
#     OnEnd<override>():void=
#         (super:)OnEnd()
