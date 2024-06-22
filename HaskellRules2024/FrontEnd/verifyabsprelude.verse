## Use FrontEnd/Makefile to update compiled prelude.
## Small prelude for verification; arithmetic functions are abstract.
## The lowered{} macro stops insertion lowering from BigCore to Core,
## i.e., stop assume{} being inserted.
array{
false                                  := array{};
any(_x:any$)<succeeds>                 := _x;
int(_x:any$)<decides>                  := { isInt$[_x]; _x };
char(_x:any$)<decides>                 := { isChr$[_x]; _x };
nat(_x:any$)<decides>                  := { isInt$[_x]; intGE$[_x,0]; _x };
operator'+'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z : any$; isInt$[_z]; _z } } };
operator'-'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z : any$; isInt$[_z]; _z } } };
operator'*'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y];                 assume { _z : any$; isInt$[_z]; _z } } };
operator'/'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intNE$[_y, 0];  assume { _z : any$; isInt$[_z]; _z } } };
operator'<'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intLT$[_x, _y]; assume { _z : any$; isInt$[_z]; _z } } };
operator'<=' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intLE$[_x, _y]; assume { _z : any$; isInt$[_z]; _z } } };
operator'>'  := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intGT$[_x, _y]; assume { _z : any$; isInt$[_z]; _z } } };
operator'>=' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intGE$[_x, _y]; assume { _z : any$; isInt$[_z]; _z } } };
operator'<>' := lowered{ lambda(_xy){ (_x,_y):=_xy; isInt$[_x]; isInt$[_y]; intNE$[_x, _y]; assume { _z : any$; isInt$[_z]; _z } } };
}
