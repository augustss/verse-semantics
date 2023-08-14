## Use FrontEnd/Makefile to update compiled prelude.
## Medium prelude
array{

operator'+' (_x:rational, _y:rational)<open><succeeds>  := { ratAdd$[_x, _y] };
operator'-' (_x:rational, _y:rational)<open><succeeds>  := { ratSub$[_x, _y] };
operator'*' (_x:rational, _y:rational)<open><succeeds>  := { ratMul$[_x, _y] };
operator'/' (_x:rational, _y:rational)<open><decides>   := { ratNE$[_y, 0]; ratDiv$[_x, _y] };
prefix'-'   (_x:rational)             <open><succeeds>  := { ratNeg$[_x] };
prefix'+'   (_x:rational)             <open><succeeds>  := { ratPlus$[_x] };
operator'<' (_x:rational, _y:rational)<open><decides>   := { ratLT$[_x,_y] };
operator'<='(_x:rational, _y:rational)<open><decides>   := { ratLE$[_x,_y] };
operator'>' (_x:rational, _y:rational)<open><decides>   := { ratGT$[_x,_y] };
operator'>='(_x:rational, _y:rational)<open><decides>   := { ratGE$[_x,_y] };
operator'<>'(_x:rational, _y:rational)<open><decides>   := { ratNE$[_x,_y] };

false                                             := array{};
any                                               := any$;
int         (_x:any$)                 <decides>   := { isInt$[_x]; _x };
rational    (_x:any$)                 <decides>   := { isRat$[_x]; _x };
f32         (_x:any$)                 <decides>   := { isF32$[_x]; _x };
f64         (_x:any$)                 <decides>   := { isF64$[_x]; _x };
string      (_x:any$)                 <decides>   := { isStr$[_x]; _x };
char        (_x:any$)                 <decides>   := { isChr$[_x]; _x };
nat         (_x:any$)                 <decides>   := { isInt$[_x]; ratGE$[_x,0]; _x };
comparable  (_x:any$)                 <decides>   := { isInt$[_x] | isRat$[_x] | isF32$[_x] | isF64$[_x] | isStr$[_x] | isChr$[_x]; _x };
void        (_x:any$)                             := array{};
Length      (_x:any$)                 <decides>   := { isArr$[_x]; arrLen$[_x] };
Err         (_x:any$)                             := { err$[_x] };

#operator'->'(_s:any$, _t:any$)(_g:any$)<decides> := function(_x:any$){isFcn$[_g]; _t[_g[_s[_x]]]};
postfix'^'(_p:any$)<reads><decides>  := read$[_p];
operator'.='(_p:any$,_x:any$)<writes><decides> := write$[_p, _x];
new := function(_t:any$)(_x:any$)<alloc><decides>{ alloc$[_t[_x]] };
}
