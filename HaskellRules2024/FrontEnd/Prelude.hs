module FrontEnd.Prelude(
    PreludeName,
    findPrelude,
    defaultPrelude
  ) where

import FrontEnd.Expr
import FrontEnd.Parse

type PreludeName = String  -- We have a bunch of named preludes

findPrelude :: PreludeName
            -> Either
                  String               -- Error message if non-existent
                  (PreludeName, SrcExpr)
findPrelude pn =
  case lookup pn preludes of
    Nothing -> Left $ "prelude " ++ pn ++ " not found"
    Just file ->
      case parseTry pFile pn file of
        Left msg -> Left $ "bad prelude " ++ msg
        Right e  -> Right (pn, e)

defaultPrelude :: PreludeName
defaultPrelude = "miniprelude"

preludes :: [(PreludeName, String)]
preludes = [noprelude, miniEvalPrelude, miniVerifyPrelude]

noprelude :: (PreludeName, String)
noprelude = ("noprelude", "\
\array{}\n\
\")

miniEvalPrelude :: (PreludeName, String)
-- This "miniprelude" uses already-lowered lambdas, rather than Source Verse,
-- to reduce clutter
miniEvalPrelude = ("miniprelude", "\
\array{\n\
\false      := array{};\n\
\true       := truth{array{}};\n\
\void       := lam y { ()           };\n\
\any        := lam y {            y };\n\
\int        := lam y { isInt$[y]; y };\n\
\rational   := lam y { isRat$[y]; y };\n\
\string     := lam y { isStr$[y]; y };\n\
\char       := lam y { isChar$[y]; y };\n\
\comparable := lam y { isComp$[y]; y };\n\
\nat        := lam y { isInt$[y]; intGE$[y,0]; y };\n\
\Length     := lam y { isArr$[y]; arrLen$[y] };\n\
\prefix'?'        := lam t { lam x { if (truth{y:any} = x) then { truth{t[y]} } else { x = () } } };\n\
\operator'..'     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; dotDot$[x,y] }};\n\
\prefix'[]'       := lam t { lam a   { exi j { j = isArr$[a]; arrMap$[t,j] } } };\n\
\operator'+'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intAdd$[x,y] }};\n\
\operator'-'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intSub$[x,y] }};\n\
\operator'*'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intMul$[x,y] }};\n\
\operator'/'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intNE$[y,0]; intDiv$[x,y] }};\n\
\operator'<'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intLT$[x,y]; x }};\n\
\operator'<='     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intLE$[x,y]; x }};\n\
\operator'>'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intGT$[x,y]; x }};\n\
\operator'>='     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intGE$[x,y]; x }};\n\
\operator'<>'     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intNE$[x,y]; x }};\n\
\prefix'-'        := lam x { isInt$[x]; intNeg$[x] };\n\
\prefix'+'        := lam x { isInt$[x]; x };\n\
\}\n\
\")

miniVerifyPrelude :: (PreludeName, String)
-- This "miniprelude" uses already-lowered lambdas, rather than Source Verse,
-- to reduce clutter
miniVerifyPrelude = ("miniverifyprelude", "\
\array{\n\
\false      := array{};\n\
\true       := truth{array{}};\n\
\void       := lam y { ()           };\n\
\any        := lam y {            y };\n\
\int        := lam y { isInt$[y]; y };\n\
\rational   := lam y { isRat$[y]; y };\n\
\string     := lam y { isStr$[y]; y };\n\
\char       := lam y { isChar$[y]; y };\n\
\comparable := lam y { isComp$[y]; y };\n\
\nat        := lam y { isInt$[y]; intGE$[y,0]; y };\n\
\Length     := lam y { isArr$[y]; y >> some{int} };\n\
\prefix'?'        := lam t { lam x { if (truth{y:any} = x) then { truth{t[y]} } else { x = () } } };\n\
\prefix'[]'       := lam t { lam a   { exi j { j = isArr$[a]; arrMap$[t,j] } } };\n\
\operator'..'     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; dotDot$[x,y] }};\n\
\operator'+'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; (x,y) >> some{ lam z { z = intAdd$[x,y]; isInt$[z]; z } }}};\n\
\operator'-'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; (x,y) >> some{ lam z { z = intSub$[x,y]; isInt$[z]; z } }}};\n\
\operator'*'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; (x,y) >> some{ lam z { z = intMul$[x,y]; isInt$[z]; z } }}};\n\
\operator'/'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intNE$[y, 0]; (x,y) >> some{ lam z { z = intDiv$[x,y]; isInt$[z]; z } }}};\n\
\operator'<'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intLT$[x,y]; x }};\n\
\operator'<='     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intLE$[x,y]; x }};\n\
\operator'>'      := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intGT$[x,y]; x }};\n\
\operator'>='     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intGE$[x,y]; x }};\n\
\operator'<>'     := lam p { exi x y { (x,y) = p; isInt$[x]; isInt$[y]; intNE$[x,y]; x }};\n\
\prefix'-'        := lam x { isInt$[x]; x >> some{ lam z { z = intNeg$[x]; isInt$[z]; z } }};\n\
\prefix'+'        := lam x { isInt$[x]; x >> some{ lam z { z = x; isInt$[z]; z } }};\n\
\}\n\
\")
