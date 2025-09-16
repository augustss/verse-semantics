using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

purchasing := class(tag){}

purchase := class<unique>():
    Buyer : fort_character
    Price : int

MakePurchase<constructor>(NewBuyer : fort_character, ItemPrice : int) := purchase:
    Buyer := NewBuyer
    Price := ItemPrice


purchase_device:= class(nexus_test_device):

    @editable
    MyTanks : tank_builder_device = tank_builder_device{}
    @editable
    MyMoney : money_handler_device = money_handler_device{}
    @editable
    TransactionHandler : transaction_handler_device = transaction_handler_device{}
 
    var TankModels : []tank = array{}

    var TankPrice : int = 5

    #Buttons
    @editable
    DefaultTankButton : button_device = button_device{}
    @editable
    MedTankButton : button_device = button_device{}
    @editable
    LargeTankButton : button_device = button_device{}

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Purchase Animal")
    #     MakeNexusTest("Purchase Food")
    #     MakeNexusTest("Purchase Tank")

    OnBegin<override>()<suspends>:void=        
        Print("Nexus OnBegin - purchase_device")

        (super:)OnBegin() # * All devices will need this line

        # SetupTest("Purchase Animal", false)
        # SetupTest("Purchase Food", false)
        # SetupTest("Purchase Tank", false)

        # CompleteTest("Purchase Animal", "Purchase Animal Test completed in OnBegin()", false)
        # CompleteTest("Purchase Food", "Purchase Food Test completed in OnBegin()", false)
        # CompleteTest("Purchase Tank", "Purchase Tank Test completed in OnBegin()", false)

        PopulateTankModels()
        PassivePurchaseLoop()

    
    OnEnd<override>():void =
        Print("Nexus OnEnd - purchase_device")
        (super:)OnEnd() # * All devices will need this line
    
    PassivePurchase() : void = 
        if(MyPurchase : purchase = TransactionHandler.GetPurchase()?):
            TankRange : int = GetRandomInt(0, TankModels.Length - 1)
            if(NewTank := TankModels[TankRange]):
                PurchaseTank(NewTank, MyPurchase)
        else:
            Print("NexusError - Failed to set purchase")

    PassivePurchaseLoop()<suspends> : void =
        Sleep(0.1)
        loop:
            Sleep(0.5)
            PassivePurchase()

    PopulateTankModels():void = 
        DefaultTank := tank{}
        AddToTankModelArray(DefaultTank)
        MedTank := tank{Length := 100.0, Width := 100.0, Height:= 100.0, Name:= "Medium Tank"}
        AddToTankModelArray(MedTank)
        LargeTank := tank{Length := 200.0, Width := 250.0, Height := 200.0, Name := "Large Tank"}
        AddToTankModelArray(LargeTank)
        MaxTank := tank{Length := 500.0, Width := 500.0, Height := 500.0, Name := "Maximum Tank"}
        AddToTankModelArray(MaxTank)

        #Subscribe Buttons
        DefaultTankButton.InteractedWithEvent.SubscribeAgent(OnTankButtonInteract, DefaultTank)
        MedTankButton.InteractedWithEvent.SubscribeAgent(OnTankButtonInteract, MedTank)
        LargeTankButton.InteractedWithEvent.SubscribeAgent(OnTankButtonInteract, LargeTank)

    AddToTankModelArray(InTank: tank):void =
        set TankModels += array{InTank}

    OnTankButtonInteract(Agent :agent, TankSize : tank): void =
        if(ButtonBuyer := Agent.GetFortCharacter[]):
            ButtonPurchase := MakePurchase(ButtonBuyer, TankPrice)
            PurchaseTank(TankSize, ButtonPurchase)

    PurchaseAnimal(InPurchase : purchase): void = 
        if(set MyMoney.CoinMap[InPurchase.Buyer] -= InPurchase.Price):
            AddPass("Purchase Animal", "Purchased Animal Successfully")
            MyMoney.IncreaseIncome(3, InPurchase.Buyer)
        else:
            Print("Failed to purchase animal")
            AddFail("Purchase Animal", "Failed to Purchase Animal Successfully")

    PurchaseFood(FoodPurchase : purchase, Animal : marine_animal) : void =
        if(set MyMoney.CoinMap[FoodPurchase.Buyer] -= FoodPurchase.Price):
            AddPass("Purchase Food", "Purchased Food Successfully")
        else:
            #Print("Failed to purchase food")
            AddFail("Purchase Food", "Failed to Purchase Food Successfully")
            
    PurchaseTank(InTank : tank, InPurchase : purchase):void=
        MyTanks.BuildTank(InTank, MyTanks.ChosenWater)
        if(set MyMoney.CoinMap[InPurchase.Buyer] -= InPurchase.Price):
            #Print("Spent {InPurchase.Price} to purchase a tank!")
            set TankPrice += 5
            MyMoney.IncreaseIncome(3, InPurchase.Buyer)
            AddPass("Purchase Tank", "Purchased Tank Successfully")
        else:
            Print("Failed to purchase tank")
            AddFail("Purchase Tank", "Failed to Purchase Tank Successfully")
 
        

#Wrapper
(Listenable : listenable(agent)).SubscribeAgent(OutputFunc : tuple(agent, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_agent(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
    
wrapper_agent(t : type) := class():
    ExtraData : t;
    OutputFunc : tuple(agent, t) -> void
    InputFunc(Agent : agent):void = OutputFunc(Agent, ExtraData)
    
(Listenable : listenable(tuple())).SubscribeEmpty(OutputFunc : t -> void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_empty(t) {ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)
    
wrapper_empty(t : type) := class():
    ExtraData : t;
    OutputFunc : t -> void
    InputFunc():void = OutputFunc(ExtraData)