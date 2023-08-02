## Use FrontEnd/Makefile to update compiled prelude.
## Small prelude for verification; arithmetic functions are concrete.
## The lowered{} macro stops insertion lowering from BigCore to Core,
## i.e., stop assume{} being inserted.
array{
false        := array{};
any          := lowered{ lambda(_x){                                                        assume { _x }}};
int          := lowered{ lambda(_x){                isInt$[_x];                             assume { _x }}};
nat          := lowered{ lambda(_x){                isInt$[_x];             intGE$[_x,0];   assume { _x }}};
operator'+'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z := intAdd$[_x,_y]; isInt$[_z]; _z }}};
operator'-'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z := intSub$[_x,_y]; isInt$[_z]; _z }}};
operator'*'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z := intMul$[_x,_y]; isInt$[_z]; _z }}};
operator'/'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intNE$[_y, 0];  assume { _z := intDiv$[_x,_y]; isInt$[_z]; _z }}};
operator'<'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intLT$[_x, _y]; assume { isInt$[_x]; _x }}};
operator'<=' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intLE$[_x, _y]; assume { isInt$[_x]; _x }}};
operator'>'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intGT$[_x, _y]; assume { isInt$[_x]; _x }}};
operator'>=' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intGE$[_x, _y]; assume { isInt$[_x]; _x }}};
operator'<>' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intNE$[_x, _y]; assume { isInt$[_x]; _x }}};
}
