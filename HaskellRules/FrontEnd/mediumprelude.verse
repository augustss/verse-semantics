## Use FrontEnd/Makefile to update compiled prelude.
## Medium prelude
array{

operator'+' (_x:rational, _y:rational)<succeeds>  := { ratAdd$[_x, _y] };
operator'-' (_x:rational, _y:rational)<succeeds>  := { ratSub$[_x, _y] };
operator'*' (_x:rational, _y:rational)<succeeds>  := { ratMul$[_x, _y] };
operator'/' (_x:rational, _y:rational)<decides>   := { ratNE$[_y, 0]; ratDiv$[_x, _y] };
prefix'-'   (_x:rational)             <succeeds>  := { ratNeg$[_x] };
prefix'+'   (_x:rational)             <succeeds>  := { ratPlus$[_x] };
operator'<' (_x:rational, _y:rational)<decides>   := { ratLT$[_x,_y] };
operator'<='(_x:rational, _y:rational)<decides>   := { ratLE$[_x,_y] };
operator'>' (_x:rational, _y:rational)<decides>   := { ratGT$[_x,_y] };
operator'>='(_x:rational, _y:rational)<decides>   := { ratGE$[_x,_y] };
operator'<>'(_x:rational, _y:rational)<decides>   := { ratNE$[_x,_y] };

false                                             := array{};
any                                               := any$;
int         (_x:any$)                 <decides>   := { isInt$[_x]; _x };
rational    (_x:any$)                 <decides>   := { isRat$[_x]; _x };
string      (_x:any$)                 <decides>   := { isStr$[_x]; _x };
char        (_x:any$)                 <decides>   := { isChr$[_x]; _x };
nat         (_x:any$)                 <decides>   := { isInt$[_x]; ratGE$[_x,0]; _x };

#operator'->'(_s:any$, _t:any$)(_g:any$)<decides> := function(_x:any$){isFcn$[_g]; _t[_g[_s[_x]]]};
#postfix'^'(_p:any$)<reads><decides>  := read$[_p];
#operator'.='(_p:any$,_x:any$)<writes><decides> := write$[_p, _x];
#new := function(_t:any$)(_x:any$)<alloc><decides>{ alloc$[_t[_x]] };
}
