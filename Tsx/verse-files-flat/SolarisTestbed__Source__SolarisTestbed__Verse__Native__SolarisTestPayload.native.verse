
solaris_test_payload<native> := class:
    
    SubObj<native>:solaris_test_payload_sub_obj
    
    IntegerA<native>:int
    
    RealA<native>:float
    
    var StringA<native>:string


solaris_test_payload_sub_obj<native> := class:
    
    BoolA<native>:logic

SolarisTestPayloadSubObj := module:    
    
    GetOuterPayload<native><public>(SubObj:solaris_test_payload_sub_obj)<transacts><decides>:solaris_test_payload
