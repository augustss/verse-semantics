## Use FrontEnd/Makefile to update compiled prelude.
## Small prelude with no checking of arithmetic.
array{
operator'+'      := intAdd$;
operator'-'      := intSub$;
operator'*'      := intMul$;
operator'/'      := intDiv$;
prefix'-'        := intNeg$;
prefix'+'        := intPlus$;
operator'<'      := intLT$;
operator'<='     := intLE$;
operator'>'      := intGT$;
operator'>='     := intGE$;
operator'<>'     := intNE$;
any              := any$;
int(_x:any$)<decides><closed>         := { isInt$[_x]; _x };
rational(_x:any$)<decides><closed>    := { isRat$[_x]; _x };
string(_x:any$)<decides><closed>      := { isStr$[_x]; _x };
char(_x:any$)<decides><closed>        := { isChr$[_x]; _x };
nat(_x:any$)<decides><closed>         := { isInt$[_x]; intGE$[_x,0]; _x };
## operator'->'(_s:any$, _t:any$)(_g:any$)<decides> := function(_x:any$){isFcn$[_g]; _t[_g[_s[_x]]]};
false                       := array{};
postfix'^'(_p:any$)<reads><decides>  := read$[_p];
operator'.='(_p:any$,_x:any$)<writes><decides> := write$[_p, _x];
new := function(_t:any$)(_x:any$)<allocates><decides>{ alloc$[_t[_x]] };
Length(_x:any$):=arrLen$[_x];
}
