{-# OPTIONS_GHC -Wall #-}
module Desugar where
import Prelude hiding ((<>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Supply
import Control.Monad.Wrong
import Data.ByteString qualified as ByteString
import qualified Data.Text as Text
import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Parse2
import qualified Language.Verse.Rewrite.Exp as R
import qualified Language.Verse.Rewrite2 as R
import Epic.Print

parseFile :: FilePath
          -> IO (L (R.Exp L Ident))
parseFile verseFile = do
  file <- ByteString.readFile verseFile
  case parse2 verseFile file of
    Left err -> error $ show err
    Right e  -> pure $ runM $ R.rewrite e

newtype M a = M { unM :: Label -> (Label, a) }
instance Functor M where
  fmap f ma = M $ \ l -> case unM ma l of (l', a) -> (l', f a)
instance Applicative M where
  pure a = M $ \ l -> (l, a)
  (<*>) = ap
instance Monad M where
  ma >>= k = M $ \ l -> case unM ma l of (l', a) -> unM (k a) l'
instance MonadWrong Error M where
  wrong e = error $ show e
instance MonadSupply Label M where
  supply = M $ \ l -> let !l' = l + 1 in (l', l)
runM :: M a -> a
runM (M a) = snd (a 0)

---------------------------------------------

isName :: String -> L (R.Exp L Ident) -> Bool
isName s (L _ (R.Name (Name t))) = s == Text.unpack t
isName _ _ = False

---------------------------------------------

data Atom = AInt Integer | AFloat Double | AChar Char
  deriving (Eq, Ord, Show)
data Vertex = VIdent Id | VAtom Atom | VTuple Int Id | VCall Vertex Vertex
  deriving (Eq, Ord, Show)
data TimCore
  = Seq [TimCore]
  | Vertex :=: Vertex
  | Scope ScopeId TimCore
  | TimCore :|: TimCore
  | Exists [Id]
  | Verify String ScopeId TimCore
  deriving (Eq, Ord, Show)

instance Pretty Atom where
  pPrintPrec l p (AInt   i) = pPrintPrec l p i
  pPrintPrec l p (AFloat i) = pPrintPrec l p i
  pPrintPrec l p (AChar  i) = pPrintPrec l p i
instance Pretty Vertex where
  pPrintPrec l _ (VIdent i) = ppId l i
  pPrintPrec l p (VAtom a)  = pPrintPrec l p a
  pPrintPrec l p (VTuple n i) = maybeParens (p>0) $ text "tuple" <> parens (pPrintPrec l 0 n) <+> ppId l i
  pPrintPrec l _ (VCall v1 v2) = pPrintPrec l 1 v1 <> parens (pPrintPrec l 0 v2)
instance Pretty TimCore where
  pPrintPrec l p (Seq xs) = maybeParens (p > 0) $ sep $ punctuate (text ";") $ map (pPrintPrec l 0) xs
  pPrintPrec l _ (v1 :=: v2) = pPrintPrec l 1 v1 <+> text "=" <+> pPrintPrec l 1 v2
  pPrintPrec l _ (e1 :|: e2) = pPrintPrec l 1 e1 <+> text "|" <+> pPrintPrec l 1 e2
  pPrintPrec l _ (Scope c e) = text "scope" <> parens (ppId l c) <> braces (pPrintPrec l 0 e)
  pPrintPrec l _ (Exists is) = text "exists" <+> (hcat $ punctuate (text ",") $ map (ppId l) is)
  pPrintPrec l _ (Verify fx c e) = text "verify" <> parens (text fx <> text "," <+> ppId l c) <> braces (pPrintPrec l 0 e)

ppId :: PrettyLevel -> String -> Doc
ppId _ s = text s

type Id = String
type ScopeId = Id
type DS a = State Int a

newIdents :: Int -> String -> DS [Id]
newIdents n prefix = do
  s <- get
  put $! s + n
  pure [ prefix ++ show i | i <- [s .. s+n-1] ]

newIdent :: String -> DS Id
newIdent prefix = do
  s <- get
  put $! s + 1
  pure $ prefix ++ show s

(+>) :: TimCore -> TimCore -> TimCore
(+>) e1 e2 = eseq [e1, e2]

eseq :: [TimCore] -> TimCore
eseq es = Seq $ concatMap f es
  where f (Seq xs) = xs
        f x = [x]

newScope :: DS TimCore -> DS TimCore
newScope dsa = do
  c <- newIdent "c"
  a <- dsa
  pure $ Scope c a

desugar :: L (R.Exp L Ident) -> TimCore
desugar e = flip evalState 1 $ do
  i <- newIdent "i"
  j <- newIdent "j"
  lds e (VIdent i) (VIdent j)

lds :: L (R.Exp L Ident) -> Vertex -> Vertex -> DS TimCore
lds (L _ e) = ds e

ds :: R.Exp L Ident -> Vertex -> Vertex -> DS TimCore
ds e u v =
  case e of
    --           (AtomSyntax)  syntax(opv,u,v){Num|Char|Path} ---> u=atom; v=atom
    R.Int x     ->  pure $ Seq [ u :=: atom, v :=: atom ] where atom = VAtom $ AInt   x
    R.Float x   ->  pure $ Seq [ u :=: atom, v :=: atom ] where atom = VAtom $ AFloat x
    R.Char32 x  ->  pure $ Seq [ u :=: atom, v :=: atom ] where atom = VAtom $ AChar  x
    --          (UnifySyntax)  syntax(opv,u,v){s0=s1}  ---> syntax(opv1,u,v){s0}; syntax(opv2,u,v){s1}
    e1 R.:=: e2 ->  (+>) <$> lds e1 u v <*> lds e2 u v
    --         (ChoiceSyntax)  syntax(opv,u,v){s0|s1}  ---> scope c {syntax(opv1,u,v){s0}} | scope d {syntax(opv2,u,v){s1}}
    e1 R.:|: e2 ->  (:|:) <$> (newScope $ lds e1 u v) <*> (newScope $ lds e2 u v)
    --          (RangeSyntax)  syntax(opv,u,v){s0..s1} ---> exists i j x y; syntax(opv1,i,x){s0}; syntax(opv2,j,y){s1}; TODO
    -- XXX
    --       (SequenceSyntax)  syntax(opv,u,v){s_0; ... s_n} --->
    --                             exists i_0 ... i_n-1;
    --                             exists x_0 ... x_n-1;
    --                             syntax(opv_0,i_0,x_0){s_0}; ... ; syntax(opv_n-1,i_n-1,x_n-1){s_n-1};
    --                             syntax(opv_n,u,v){s_n}
    --                         n<>1
    R.List es   ->  do
      let n = length es
      is <- newIdents (n-1) "i"
      xs <- newIdents (n-1) "x"
      ss <- sequence $ zipWith3 lds es (map VIdent is) (map VIdent xs)
      s  <- lds (last es) u v
      pure $ eseq $ [Exists is, Exists xs] ++ ss ++ [s]
    --          (TupleSyntax)  syntax(opv,u,v){s_0,...,s_n-1} ---> syntax(opv1,u,v){array{s_0,...,s_n-1}}
    -- No special tuple construct
    --           (ArrayMacro)  syntax(opv,u,v){array{s_0, ..., s_n-1}) --->
    --                             u=tuple(n) i;
    --                             v=tuple(n) x;
    --                             syntax(opv_0  ,(tuple(n) i)(0  ),(tuple(n) x)(0  )){s_0  };
    --                             ...
    --                             syntax(opv_n-1,(tuple(n) i)(n-1),(tuple(n) x)(n-1)){s_n-1}
    --       (ArrayListMacro)  syntax(opv,u,v){array{s_0; ...  s_n-1}) ---> syntax(opv1,u,v){array{s_0,...,s_n-1}}
    R.Tuple es -> do
      i <- newIdent "i"
      x <- newIdent "x"
      let n = length es
          iTuple = VTuple n i
          uEq    = u :=: iTuple
          xTuple = VTuple n x
          vEq    = v :=: xTuple
          f k s = lds s (VCall iTuple vk) (VCall xTuple vk) where vk = VAtom (AInt k)
      ss <- zipWithM f [0..] es
      pure $ eseq $ [uEq, vEq] ++ ss

    --       (DeoptionSyntax)  syntax(opv,u,v){s0?} --->
    --                             exists f h x;
    --                             u=z; v=z;
    --                             syntax(opv1,h,f){s0};
    --                             z=f(x)
    R.BracketInvoke q s0 | isName "postfix'?'" q -> do
      f <- newIdent "f"; h <- newIdent "h"; x <- newIdent "x"; z <- newIdent "z"
      let vf = VIdent f; vh = VIdent h; vx = VIdent x; vz = VIdent z
      e0 <- lds s0 vf vh
      pure $ eseq $ [Exists [f,h,x,z], u :=: vz, v :=: vz, e0, vz :=: VCall vf vx]

    --     (CallClosedSyntax)  syntax(opv,u,v){s0[s1]} --->
    --                             exists f h i x z;
    --                             u=z; v=z;
    --                             syntax(opv1,h,f){s0};
    --                             syntax(opv2,i,x){s1};
    --                             z=f(x)
    R.BracketInvoke s0 s1 -> do
      f <- newIdent "f"; h <- newIdent "h"; i <- newIdent "i"; x <- newIdent "x"; z <- newIdent "z"
      let vf = VIdent f; vh = VIdent h; vx = VIdent x; vz = VIdent z
      e0 <- lds s0 vf vh
      e1 <- lds s1 (VIdent i) vx
      pure $ eseq $ [Exists [f,h,i,x,z], u :=: vz, v :=: vz, e0, e1, vz :=: VCall vf vx]
      
    --       (CallOpenSyntax)  syntax(opv,u,v){s0(s1)} --->
    --                             exists f h i x;
    --                             u=z; v=z;
    --                             syntax(opv1,h,f){s0};
    --                             syntax(opv2,i,x){s1};
    --                             verify(succeeds+imperatives,P00) c {
    --                                 exists f1 x1 z;
    --                                 f1=f; x1=x; z=f1(x1)
    --                             }
    -- XXX does not follow the above exactly
    R.ParenInvoke s0 s1 -> do
      f <- newIdent "f"; h <- newIdent "h"; i <- newIdent "i"; x <- newIdent "x"; z <- newIdent "z"
      let vf = VIdent f; vh = VIdent h; vx = VIdent x; vz = VIdent z
      e0 <- lds s0 vf vh
      e1 <- lds s1 (VIdent i) vx
      c <- newIdent "c"
      pure $ eseq $ [Exists [f,h,i,x,z], u :=: vz, v :=: vz, e0, e1, Verify "succeeds+imperatives" c (vz :=: VCall vf vx)]
      
    R.Name n -> pure $ eseq [ u :=: i, v :=: i ] where i = VIdent (show n) -- XXX hack
    _           ->  error $ "ds: " ++ show e
{-



     (UnderscoreSyntax)  syntax(opv,u,v){_} ---> syntax(opv1,u,v){:any}

                         G |- ProgramScope[sc,ProgramSpan[resolved(opv){u=definable; v=definable}]]
          (IdentSyntax)  --------------------------------------------------------------------------
                         G |- ProgramScope[sc,ProgramSpan[syntax(opv,u,v){Ident}                 ]]
                           &  ProgramScope[sc,ScopeSpan[define Ident {definable}                 ]]

                         This rule resolves a reference to an identifier Ident anywhere underneath
                         a scope sc that is defined within the scope sc's ScopeSpan.
                         TODO: Explain deterministic mechanism for detecting and producing ambiguous identifier errors.

       (DotIdentSyntax)  syntax(opv,u,v){s0.Ident} --->
                             exists f h z;
                             syntax(opv1,h,f){s0};
                             cast(f,opv2,abstracts*reads*(accepts+N05)) {
                                 head normal form representing an instance of a nominal type containing a field with a Path that Ident matches unambiguously => {
                                     u=z; v=z
                                     z=f(Path);
                                 }
                             }
          (IdentDefine)  syntax(opv,u,v){Ident:=s2} ---> define Ident {v}; syntax(opv1,u,v){s2}

     (UnderscoreDefine)  syntax(opv,u,v){_:=s} ---> syntax(opv1,u,v){s}

      (IdentSpecDefine)  syntax(opv,u,v){Ident<s1>:=s2} --->
                             verify(succeeds,U00) c {
                                 exists i x;
                                 syntax(opv1,i,x){s1}
                             }
                             cast(x,opv2,accepts+X31) {
                                 natively defined 'var' specifier => {define Ident {v^}; syntax(opv3,u,v){TODO}}
                                 natively defined 'ref' specifier => {define Ident {v^}; syntax(opv3,u,v){TODO}}
                             }

                         TODO: Define exactly how desugaring works based on definition form, for example:
                         var x:t   -> x:new(t)
                         var x:t=v -> x:new(t)=v
                         var x:=v  -> x:=new(type{v})=v, but this is a rough approximation

           *(DotDefine)  syntax(opv,u,v){s0.Ident:=s1} ---> syntax(opv1,u,v){operator'.Ident'(s0):=s1}
       (CallOpenDefine)  syntax(opv,u,v){s0(s1)Specs:=s2) ---> syntax(opv1,u,v){s0:=function(s1)Specs{s2}}

    *(CallClosedDefine)  syntax(opv,u,v){s0[s1]Specs:=s2} ---> ...
      *(MultipleDefine)  syntax(opv,u,v){(s_0 & ... & s_n) Specs := s} ---> 
                             syntax(opv1,u,v){s_0 Specs:=expect<computes>{s}}; ... syntax(opv2,u,v){s_n Specs:=expect<computes>{s}}

                         NOTE: Later we'll wrap the result in prefix'..' macro for Scheme macro style unquote-splicing.

                         G |- ProgramOp[c,resolve(opv){
                                  u=tuple(n) t1;
                                  v=tuple(n) t2;
                                  syntax(opv1,u,v){s2};
                                  syntax(opv_0  ,(tuple(n) t1)(0  ),(tuple(n) t2)(0  )){s0   Specs:=_};
                                  ...
                                  syntax(opv_n-1,(tuple(n) t1)(n-1),(tuple(n) t2)(n-1)){sn-1 Specs:=_}
                              }]
         *(ArrayDefine)  ------------------------------------------------------------------------------------------------------------
                         G, effects{u=tuple(p) t0}@c, effects{p=n}@c |- ProgramOp[c,syntax(opv,u,v){array{s_0, ..., s_n-1}Specs:=s2}]
                         G, effects{v=tuple(p) t0}@c, effects{p=n}@c |- ProgramOp[c,syntax(opv,u,v){array{s_0, ..., s_n-1}Specs:=s2}]

     *(ArrayListDefine)  syntax(opv,u,v){array{s_0; ...; s_n-1}Specs:=s} ---> syntax(opv1,u,v){array{s_0,...,s_n-1}Specs:=s}

                         n<>1
         *(TupleDefine)  syntax(opv,u,v){(s_0, ..., s_n-1)Specs:=s2} ---> syntax(opv1,u,v){array{s_0,...,s_n-1}Specs:=s2}

      *(DeoptionDefine)  syntax(opv,u,v){s0? Specs:=s1)        ---> syntax(opv1,u,v){s0:=option Specs{s1}}
      *(OptionalDefine)  syntax(opv,u,v){?s0 Specs:=s1)        ---> ...

   *(PointerTypeDefine)  syntax(opv,u,v){s0^:s1)               ---> syntax(opv1,u,v){s0:new(s1)}
  *(PointerStageDefine)  syntax(opv,u,v){s0^:s1=s2)            ---> syntax(opv1,u,v){s0:new(s1)=s2}
      
       *(VarTypeDefine)  syntax(opv,u,v){var s0:s1)            ---> syntax(opv1,u,v){s0<var>:s1}
      *(VarStageDefine)  syntax(opv,u,v){var s0:s1=s2)         ---> syntax(opv1,u,v){s0<var>:s1=s2}
      
           *(RefDefine)  syntax(opv,u,x){s0 ref)               ---> TODO
       *(RefTypeDefine)  syntax(opv,u,v){ref s0:s1)            ---> syntax(opv1,u,v){s0<ref>:s1}
      *(RefStageDefine)  syntax(opv,u,v){ref s0:s1=s2)         ---> syntax(opv1,u,v){s0<ref>:s1=s2}

    *(AliasValueDefine)  syntax(opv,u,v){alias s0:=s1)         ---> syntax(opv1,u,v){s0<alias>:=s1 ref}
         *(AliasDefine)  syntax(opv,u,v){alias s0)             ---> syntax(opv1,u,v){s0<alias>}

                 *(Set)  syntax(opv,u,v){set s0=s3}         ---> ...
         *(DeoptionSet)  syntax(opv,u,v){set s0?=s3}        ---> ...
             *(ReadSet)  syntax(opv,u,v){set s0^=s3}        ---> ...
         *(CallOpenSet)  syntax(opv,u,v){set s0(s1)=s2}     ---> ...
     *(CallOpenSpecSet)  syntax(opv,u,v){set s0(s1)<s3>=s2} ---> ...
       *(CallClosedSet)  syntax(opv,u,v){set s0[s1]=s2}     ---> ...

             (InSyntax)  syntax(opv,u,v){:s0} ---> syntax(opv1,u,v){_->_:s0}

        (ArrowInDefine)  syntax(opv,u,v){(s1->s2)Specs0:UnwrapColons[s0 Specs1]} --->
                             # Here we define s1 as the input, and move s2 definition to the unwrapped output.
                             exists i f h;
                             syntax(opv2,i,u){s1:=_};
                             syntax(opv1,h,f){s0};
                             v=x; # When implying, directed edge x->v makes type abstraction output x flow into 'in' output v.
                             in(effects,none) c k x {
                                 exists g j y;
                                 k=u;    # When implying, directed edge u->k makes 'in' input u flow into type abstraction input k.
                                 g=f;
                                 y=g(k); # Plumb input through type abstraction to get output.
                                 define Ident1 {y}; # Where Ident1 fresh; needed because UnwrapColons expects syntax, not variable.
                                 syntax(opv3,j,x){s2 Specs0:=UnwrapColons[Ident1]}
                             }
                         TODO: Handle Specs1.

          (StageSyntax)  syntax(opv,u,v){:s0 Specs = s1) --->
                             VerifyFxSpecs(fx,effects,Specs) {
                                 exists f h;
                                 syntax(opv1,h,f){s0);
                                 v=x;
                                 stage(fx,fxv)
                                     c0 k x     {exists g; k=u; g=f; x=g(k)}
                                     value c1 z {exists j f2; syntax(opv2,j,z){s1}; f2=f; z=f2(j)}
                             }
                         TODO: Handle Specs.

     *(StageDefine)      TODO
        (FunctionMacro)  syntax(opv,u,v){function(s0)Specs{s1}} --->
                             VerifySpecs(fx,oc,lambda_defaults,Specs) {
                                cast(cast(u,opv1,X31) {
                                    fx where fx<=lambda_allows => {
                                        v=lambda(oc,fx,u)
                                            d0 i w       {syntax(opv2,i,w){s0}}
                                            range d1 j z {syntax(opv3,j,z){s1}}
                                    }
                                }
     (LambdaArrowMacro)  syntax(opv,u,v){operator'=>'(s0){s1}} ---> syntax(opv1,u,v){function(s0)<succeeds><transacts>{s1}}

            (TypeMacro)  syntax(opv,u,v){type Specs{s0}} ---> syntax(opv1,u,v){function(y:=s0)Specs<closed>{y}}

             (LetMacro)  syntax(opv,u,v){let(s0){s1}} --->
                             exists i x;
                             scope c {
                                 syntax(opv1,i,x){s0};
                                 scope d {
                                     syntax(opv2,u,v){s1}
                                 }
                             }

           (WhereMacro)  syntax(opv,u,v){s0 where s1} --->
                             exists i x;
                             syntax(opv1,u,v){s0};
                             syntax(opv2,i,x){s1}

      (IfThenElseMacro)  syntax(opv,u,v){if(s0){s1}else{op2}} --->
                             exists i;
                             i=();
                             iterate {
                                 split(i)
                                     c0         {exists j y; syntax(opv1,j,y){s0}}
                                     then c1 v1 {syntax(opv2,u,v){s1}; v1=()}
                                     else c2    {syntax(opv3,u,v){s2}}
                             }
          (IfThenMacro)  syntax(opv,u,v){if(s0){s1}}     ---> syntax(opv1,u,v){if(s0){s1}else{}}
              (IfMacro)  syntax(opv,u,v){if{s0}}         ---> syntax(opv1,u,v){if(y:=s0){y}else{}}
          (IfElseMacro)  syntax(opv,u,v){if{s0}else{s1}} ---> syntax(opv1,u,v){if(y:=s0){y}else{s1}}

         (FirstDoMacro)  syntax(opv,u,v){first(s0){s1}} ---> syntax(opv1,u,v){if(s0){s1}else{u=fail; v=fail}}
           (FirstMacro)  syntax(opv,u,v){first{s0}}     ---> syntax(opv2,u,v){first(y:=s0){y}}

             (AndMacro)  syntax(opv,u,v){s0 and s1} --->
                             exists j y;
                             scope c {syntax(opv1,j,y){s0}};
                             scope d {syntax(opv2,u,v){s1}}

              (OrMacro)  syntax(opv,u,v){s0 or  s1} ---> syntax(opv1,u,v){if{s0}else{s1}}
                         NOTE: Could be first{s0|s1}, but that nests strangely.

             (NotMacro)  syntax(opv,u,v){not s0} ---> syntax(opv1,u,v){if(s0){u=fail; v=fail}else{}}

            (LastMacro)  syntax(opv,u,v){last{s0}} --->
                             exists i;
                             i=();
                             iterate {
                                 split(i)
                                     c0         {exists j y; syntax(opv1,j,y){s0}}
                                     then c1 v1 {exists w; v1=tuple(1) t0; (tuple(1) t0)(0)=tuple(1) t1; (tuple(1) t1)(0)=y}
                                     else c2    {exists z; z=u0(0); u=z; v=z}
                             }

           (ForDoMacro)  syntax(opv,u,v){for(s0){s1}} --->
                             exists i w;
                             u=tuple(w) t0;
                             v=tuple(w) t1;
                             i=0;
                             iterate {
                                 split(i)
                                     c0 {
                                         exists j y;
                                         syntax(opv1,j,y){s0}
                                     }
                                     then c1 v1 {
                                         exists k z;
                                         k=(tuple(w) t0)(i);
                                         z=(tuple(w) t1)(i);
                                         syntax(opv1,k,z){s1};
                                         v1=(tuple(1) t2)
                                         (tuple(1) t2)(0)=i+1
                                     }
                                     else c2 {w=i}
                             }
             (ForMacro)  syntax(opv,u,v){for{s0}} ---> syntax(opv1,u,v){for(y:=s0){y}else{}}

          (ForAllMacro)  syntax(opv,u,v){forall(s0){s1}} ---> TODO

        (NotEqualMacro)  syntax(opv,u,v){s0<>s1} ---> syntax(opv1,u,v){s0 where for(y:=s1) {not v=y}}

          (VerifyMacro)  syntax(opv,u,v){verify Specs{s0}} --->
                             VerifyFxSpecs(fx,succeeds,Specs){
                                 u=i; v=x
                                 verify(fx,U00) d {exists i x; syntax(opv1,i,x){s0}};
                             }

          (AssertMacro)  syntax(opv,u,v){assert{s0}} ---> syntax(opv1,u,v){if(s0){} else Err("Assertion Failed")}

          (RejectMacro)  syntax(opv,u,v){reject{s0}} ---> scope c {exists j y; syntax(opv1,j,y){s0}; V02}

           *(CaseMacro)  syntax(opv,u,v){case(s)    {s_0; ... s_n}} ---> TODO
     *(LambdaCaseMacro)  syntax(opv,u,v){case       {s_0; ... s_n}} ---> TODO
 *(LambdaCaseSpecMacro)  syntax(opv,u,v){case<oc|fx>{s_0; ... s_n}} ---> TODO
          (AssumeMacro)  syntax(opv,u,v){assume Specs{s0}} --->
                             VerifyFxSpecs(fx,succeeds,Specs){
                                 assume(fx) d {exists i x; syntax(opv3,i,x){s0}};
                                 u=i; v=x
                             }

           (AllowMacro)  syntax(opv,u,v){allow Specs{s0}} ---> ...

          (ExpectMacro)  syntax(opv,u,v){expect Specs{s0}} ---> ...

            (TestMacro)  syntax(opv,u,v){test(err){s0}} ---> ...
-}

dsFile :: FilePath -> IO ()
dsFile fn = do
  e <- parseFile fn
  putStrLn $ prettyShow $ desugar e
