## Use FrontEnd/Makefile to update compiled prelude.
## Small for verification.
## The lowered{} macro stops insertion lowering from BigCore to Core,
## i.e., stop assume{} being inserted.
array{
false                                  := array{};
any(_x:any$)<succeeds>                 := _x;
int(_x:any$)<decides>                  := { isInt$[_x]; _x };
nat(_x:any$)<decides>                  := { isInt$[_x]; intGE$[_x,0]; _x };
operator'+'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; assume { _z := intAdd$[_x,_y]; isInt$[_z]; _z } } };
operator'-'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; assume { _z := intSub$[_x,_y]; isInt$[_z]; _z } } };
operator'*'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; assume { _z := intMul$[_x,_y]; isInt$[_z]; _z } } };
operator'/'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intNE$[_y, 0]; assume { _z := intDiv$[_x,_y]; isInt$[_z]; _z } } };
operator'<'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intLT$[_x, _y]; assume { isInt$[_x]; _x } } };
operator'<=' := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intLE$[_x, _y]; assume { isInt$[_x]; _x } } };
operator'>'  := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intGT$[_x, _y]; assume { isInt$[_x]; _x } } };
operator'>=' := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intGE$[_x, _y]; assume { isInt$[_x]; _x } } };
operator'<>' := lowered{ lambda(xy){ (_x,_y):=xy; isInt$[_x]; isInt$[_y]; intNE$[_x, _y]; assume { isInt$[_x]; _x } } };
}
