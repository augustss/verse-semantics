
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Verse }
using { /Fortnite.com/Characters }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.



# A Verse-authored creative device that can be placed in a level
money_handler_device := class(nexus_test_device):

    @editable
    TransactionHandler : transaction_handler_device = transaction_handler_device{}
    @editable
    DeviceTimer : timer_device = timer_device{}
    @editable
    TankBuilder : tank_builder_device = tank_builder_device{}
    @editable
    HUDDevice : hud_device = hud_device{}

    var CoinMap : [fort_character]int = map{}
    var IncomeMap : [fort_character]int = map{}
    var DebtMap : [fort_character]int = map{}

    AddMoney : logic = true

    #Timer
    var SecondTimer : int = 0

    var PurchasePrice : int = 10

    #HUD elements
    @editable
    TimeMoneyHUD : hud_message_device = hud_message_device{}

    var MessageString : string = ""
    
    var TimerString : string = ""
    var MoneyString : string = ""
    var DebtString : string = ""
    var TankString : string = ""
    var AnimalString : string = ""

    @editable
    Spawners : []player_spawner_device = array{}

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Set Debt")
    #     MakeNexusTest("Increase Income")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - money_handler_device")
        for(playerspawn : Spawners):
            playerspawn.SpawnedEvent.Subscribe(OnPlayerSpawn)

            (super:)OnBegin() # * All devices will need this line

            # SetupTest("Set Debt", false)
            # SetupTest("Increase Income", false)
            # CompleteTest("Set Debt", "Set Debt Test completed in OnBegin()", false)
            # CompleteTest("Increase Income", "Increase Income completed in OnBegin()", false)

    OnEnd<override>():void =
        Print("Nexus OnEnd - money_handler_device")
        (super:)OnEnd() # * All devices will need this line

    OnPlayerSpawn(InAgent : agent): void =
        if(InPlayer := InAgent.GetFortCharacter[]):
            if(set DebtMap[InPlayer] = 0):
            if(set CoinMap[InPlayer] = 0):
                if (set IncomeMap[InPlayer] = 1):
                    #Print("Player successfully set up with money_handler_device")
                    spawn{ RunLoops(InPlayer) }
        spawn{RunTimer()}


    RunLoops(InCharacter : fort_character)<suspends>:void =
        sync:
            PassiveIncome(InCharacter)
            HUDLoop(InCharacter)
            SetPurchaseLoop(InCharacter)
            SetTankStringLoop()
            SetAnimalStringLoop()
            DebtLoop(InCharacter)


    HUDLoop(InCharacter : fort_character)<suspends>:void =
        loop:
            set MessageString = "{MoneyString} \n {DebtString} \n {TimerString} \n {TankString} \n {AnimalString}"
            TimeMoneyHUD.SetText(StringToMessage(MessageString))
            Sleep(0.0)


    RunTimer()<suspends>:void =
        loop:
            set TimerString = "Seconds: {SecondTimer}"
            Sleep(1.0)
            set SecondTimer = SecondTimer + 1

    SetTankStringLoop()<suspends>: void =
        loop:
            set TankString = "Tanks: {TankBuilder.GetTankCount()}"
            Sleep(1.0)

    SetAnimalStringLoop()<suspends>: void =
        loop:
            set AnimalString = "Animals: {HUDDevice.CountAnimals()}"
            Sleep(1.0)

    PassiveIncome(InCharacter : fort_character)<suspends>:void =    
        loop:
            if(set CoinMap[InCharacter] += IncomeMap[InCharacter]):
                # if(CoinMap[InCharacter] < 0):
                #     if(SetDebt(InCharacter, CoinMap[InCharacter])):  
                #     if(set CoinMap[InCharacter] = 0): 
                #         #Print("Profit set to 0")   
                #     else:
                #         Print("Couldn't set coin map to 0")
                if(NewMoneyString : string = "Coral Coins: {CoinMap[InCharacter]}"):
                    set MoneyString = NewMoneyString
                    Sleep(0.1)
                # Uncommenting the line below causes a Verse Error
                #IncreaseIncome(2, InCharacter)
            if(AddMoney = false):
                break

    SetDebt(InCharacter : fort_character, NewDebt : int)<transacts>: void =
        if(set DebtMap[InCharacter] += NewDebt):
            #AddPass("Set Debt", "Debt successfully set in SetDebt()")
        else:
            #AddFail("Set Debt", "Debt could not be set in SetDebt()")
        
        if(NewDebtString : string = "Debt : {DebtMap[InCharacter]} Coins"):
            set DebtString = NewDebtString
        else:
            Print("Couldn't set Debt String")
            
    IncreaseIncome(AddedCoins : int, InCharacter : fort_character):void =
        if(set IncomeMap[InCharacter] += AddedCoins):
            AddPass("Increase Income", "Income Increased in IncreaseIncome()")
        else:
            AddFail("Increase Income", "Income could not be Increased in IncreaseIncome()")

    DebtLoop(InCharacter : fort_character)<suspends>: void =
        loop:
            #Print("Debt Loop")
            PayDebt(InCharacter)
            Sleep(1.0)


            ################################################ YOU LEFT OFF HERE!
            # Moved the logic for setting debt into the looped function for paying debt

    PayDebt(InCharacter : fort_character)<transacts> : void = 
        # if(DebtMap[InCharacter] <= 0):
        #     Print("Debt Paid")
        #     return

        if(CoinMap[InCharacter] < 0):
            if(SetDebt(InCharacter, CoinMap[InCharacter])):  
            if(set CoinMap[InCharacter] = 0): 
                #Print("Profit set to 0")   
            else:
                Print("Couldn't set coin map to 0")
        else:
            if(DebtAmount : int = DebtMap[InCharacter]):
                #Print("Debt = {DebtAmount}")
            if(DebtMap[InCharacter] <= CoinMap[InCharacter]):
                if(set CoinMap[InCharacter] = CoinMap[InCharacter] - DebtMap[InCharacter]):
                    SetDebt(InCharacter, 0)
            else:
                Print("Can't afford to pay debt")


    

    #Agent Handler Tests
    SetPurchaseLoop(InCharacter : fort_character)<suspends>:void =
        loop:
            Sleep(0.5)
            SetUpPurchase(InCharacter, PurchasePrice)
            set PurchasePrice += 5


    SetUpPurchase(InCharacter : fort_character, Price : int): void =
        NewPurchase := MakePurchase(InCharacter, Price)
        TransactionHandler.SetPurchase(NewPurchase)
        

    #There's currently no way to check the amount of time passed on a timer that counts up.

    # TimeTests()<suspends>:void =
    #     Sleep(10.0)
    #     if(SecondTimer = DeviceTimer.GetActiveDuration()):
    #         Print("PASS - Timer keeps track of seconds as expected")
    #     else:
    #         Print("FAIL - Timer value is {SecondTimer} instead of {DeviceTimer.GetActiveDuration()}")
    #     Sleep(10.0)
    #     if(SecondTimer = DeviceTimer.GetActiveDuration()):
    #         Print("PASS - Timer keeps track of seconds as expected")
    #     else:
    #         Print("FAIL - Timer value is {SecondTimer} instead of {DeviceTimer.GetActiveDuration()}")
        


