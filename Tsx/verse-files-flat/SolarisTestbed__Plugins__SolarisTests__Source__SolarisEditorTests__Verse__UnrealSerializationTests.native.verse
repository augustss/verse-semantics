using { /Verse.org/Simulation }

enum_test<native> := enum{A, B, C}

subobject_test_inner1<native> := class<concrete>:
    @editable
    A<native>:int = 5

subobject_test_inner2<native> := class<concrete>:
    @editable
    B<native>:int = 5

subobject_test_outer1<native> := class:
    @editable
    X<native>: subobject_test_inner1 = subobject_test_inner1{ A:= 7 }
    @editable
    Y<native>: subobject_test_inner1 = subobject_test_inner1{ A:= 8 }
    @editable
    Z<native>: subobject_test_inner1 = subobject_test_inner1{ A:= 9 }

subobject_test_outer2<native> := class:
    @editable
    Z<native>: subobject_test_inner1 = subobject_test_inner1{ A:= 9 }
    @editable
    Y<native>: subobject_test_inner2 = subobject_test_inner2{ B:= 8 }
    @editable
    X<native>: subobject_test_inner1 = subobject_test_inner1{ A:= 7 }

struct_test<native> := struct:
    @editable
    A<native>:int = 10
    @editable
    B<native>:string = "11"

int_serialization_test_class<native> := class{X<native>:int = 0}
float_serialization_test_class<native> := class{X<native>:float = 0.0}
string_serialization_test_class<native> := class{X<native>:string = ""}
optional_int_serialization_test_class<native> := class{X<native>:?int = option{0}}
sub_object_serialization_test_class<native> := class{X<native>:int_serialization_test_class = int_serialization_test_class{X:=0}}
optional_sub_object_serialization_test_class<native> := class{X<native>:?int_serialization_test_class = option{int_serialization_test_class{X:=0}}}
int_pointer_serialization_test_class<native> := class{var X<native>:int = 0}
any_serialization_test_class<native> := class{var X<native>:any = false}
any_with_sub_object_serialization_test_class<native> := class(any_serialization_test_class){Subobject<native>:function_serialization_test_class = function_serialization_test_class{}}

function_serialization_test_class<native> := class:
    var X<native>:int = 0
    F<native_callable>():int=return X
    GetF<native_callable>():type{_():int}=F

# Both native_callable and async are not supported together, yet.
async_function_serialization_test_class<native> := class:
    var X<native>:int = 0
    F<native_callable>()<suspends>:int=return X

parametric_class<native>(t:type):=class:
    Value<native>:t

parametric_subobject_serialization_test_class<native> := class:
    Test<native>:parametric_class(int)=parametric_class(int){Value:=10}

MakeParametricSubobjectSerializationTestClass<native_callable>():parametric_subobject_serialization_test_class=parametric_subobject_serialization_test_class{}

my_struct<native> := struct<concrete>:
    @editable MyInt<native>:int = 66667777
    @editable MyInt2<native>:int = 22223333

my_other_struct<native> := struct<concrete>:
    @editable MyInt<native>:int = 77776666
    @editable MyInt2<native>:int = 33332222

my_class<native> := class<concrete>:
    @editable MyClassInt<native>:int = 99998888
    @editable MyStruct<native>:my_struct = my_struct{}
    @editable MyStructs<native>:[]my_struct = array{my_struct{}}  # array with one element