# Copyright Epic Games, Inc. All Rights Reserved.

#----------------------------------------------------------------------------------------
# Used to test access levels in different modules

test_class<public> := class:

    # Default access
    DataDefault : int = 1

    
    DataPublic<public> : int = 2

    
    DataInternal<internal> : int = 3

    # Default access
    MethodDefault() : int=
        return (11)

    
    MethodPublic<public>() : int=
        return (22)

    
    MethodInternal<internal>() : int=
        return (33)

    # Default access
    MethodClassDefault() : int=
        return (44)

    
    MethodClassPublic<public>() : int=
        return (55)

    
    MethodClassInternal<internal>() : int=
        return (66)


TestClass<public> := module:
    DataClassDefault : int = 4

    
    DataClassPublic<public> : int = 5

    
    DataClassInternal<internal> : int = 6
