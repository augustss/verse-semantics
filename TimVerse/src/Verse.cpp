//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verse Verifier & Runtime Interpreter.

#define ANY_FUNCTION_FLOW 0
#define NEW_FUNCTION_FLOW 0
#define SELF_DOM          1
#define VECTOR_EQUALIZERS 1
#define TRIVIAL_STRENGTH  1
#pragma warning(disable: 4068 4100 4180)
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#define TIM 1
#include "VerseGrammar.h"
#include "Terse.h"
#define VERIFIER_HERE(Tag) [=]{return R00(locus{},string_fix_me(Tag));}

using namespace Verse;
using namespace Grammar;
namespace Verse {namespace Grammar {
	string ExposeToString(text S) { // Expose in right location for Koenig lookup.
		return ToString(span<char8>(S.Start,S.Stop));
	}
}}

// Debugging options.
bool ContextReportBetas   = false;
bool VerboseContextReport = false;
bool VerboseEval          = false;
bool VerboseSplitImply    = false;
bool VerboseEquate        = false;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Helper functions.

template<class... ps> requires requires(ps... PS) {(ToString(PS),...);}
function<string()> CaptureString(const ps&... PS) {
	return [=]{return ToString(PS...);};
}
template<class t> t CoerceResult(const result<t,error>& Result) {
	if(Result)
		return *Result;
	Err(Result.GetError());
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Forward declarations.

struct inferences;
struct equation_managed;
expose_mutable ExposeUnique(const equation_managed&); 
using equation = box<equation_managed>;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Syntax.

// Atoms.
struct syntax::atom: syntax {
	comparable AtomValue;
	atom(const locus& Locus0,const comparable& AtomValue0): syntax(Locus0), AtomValue(AtomValue0) {}
};
template<> struct expose<syntax::atom>: default_expose<syntax::atom> {
	path ExposeStaticSignature() {return "/Verse.org/syntax/atom"_VP;}
};

// Identifiers.
bool IsValidIdentifier(const string& S) {
	if(nat n=Length(S); n>0 && IsAlpha(S[0])) {
		for(nat i=1; i<n; i++) {
			auto c=S[i];
			if(IsAlnum(c))
				continue;
			if(c=='\'') {
				for(i++; i<n; i++)
					if(c=S[i]; IsIdentifierQuotable(c,i<n-1? S[i]: 0))
						continue;
					else
						return c=='\'' && i==n-1;
			}
			return false;
		}
		return true;
	}
	return false;
}
struct syntax::identifier: syntax {
	option<box<syntax>> Qualifier;
	string Name;
	bool Matches(const string& S) const {return !Qualifier && Name==S;}
	bool Matches(const char8*  S) const {return !Qualifier && Name==S;}
	identifier(const locus& Locus0,const option<box<syntax>>& Qualifier0,const string& Name0): syntax(Locus0), Qualifier(Qualifier0), Name(Name0) {
		VERSE_ENSURE(IsValidIdentifier(Name0));
	}
};
template<> struct expose<syntax::identifier>: default_expose<syntax::identifier> {
	path ExposeStaticSignature() {return "/Verse.org/syntax/identifier"_VP;}
};

// Calls.
struct syntax::call: syntax {
	mode CallMode;
	future<box<syntax>> FunctionSyntax,ParameterSyntax;
	call(const locus& Locus0,mode CallMode0,const future<box<syntax>>& FunctionSyntax0,const future<box<syntax>>& ParameterSyntax0):
		syntax(Locus0), CallMode(CallMode0), FunctionSyntax(FunctionSyntax0), ParameterSyntax(ParameterSyntax0) {
		VERSE_ENSURE(CallMode==mode::Open || CallMode==mode::Closed || CallMode==mode::With);
	}
};
template<> struct expose<syntax::call>: default_expose<syntax::call> {
	path ExposeStaticSignature() {return "/Verse.org/syntax/call"_VP;}
};

// Macros.
using clause=syntax::clause;
struct syntax::clause {
	locus                      ClauseLocus;
	array<future<box<syntax>>> Specifiers, Body;
	form                       Form = form::List;
};
struct syntax::invoke: syntax {
	future<box<syntax>> InvokeMacro;
	clause              Clause;
	option<clause>      DoClause, PostClause;
	invoke(const locus& Locus0,const future<box<syntax>>& InvokeMacro0,const clause& Clause0,const option<clause>& DoClause0=False,const option<clause>& PostClause0=False):
		syntax(Locus0), InvokeMacro(InvokeMacro0), Clause(Clause0), DoClause(DoClause0), PostClause(PostClause0) {}
};
template<> struct expose<syntax::invoke>: default_expose<syntax::invoke> {
	path ExposeStaticSignature() {return "/Verse.org/syntax/invoke"_VP;}
};

// Escapes.
struct syntax::escape: syntax {
	future<box<syntax>> Escaped;
	escape(const locus& Locus0,const future<box<syntax>>& Escaped0): syntax(Locus0), Escaped(Escaped0) {}
};
template<> struct expose<syntax::escape>: default_expose<syntax::escape> {
	path ExposeStaticSignature() {return "/Verse.org/syntax/escape"_VP;}
};

// Syntax construction helpers.
static box<syntax> MakeNativeSyntax(const string& S) {
	return box<syntax::identifier>(locus{},False,S); // Needs Qualifier.
}
static box<syntax> MakeNativeSyntax(const char* S) {
	return MakeNativeSyntax(string_fix_me(S));
}
static box<syntax> MakeStringSyntax(const string& S) {
	array<box<syntax>> CS=S.For([&](char8 C) {
		return box<syntax::atom>(locus{},C);
	});
	return box<syntax::invoke>(locus{},MakeNativeSyntax("array"),clause{locus{},False,CS});
}
static box<syntax> MakeDefineSyntax(const locus& Locus,const future<box<syntax>>& Left,const future<box<syntax>>& Right) {
	return box<syntax::invoke>(locus{},MakeNativeSyntax("operator':='"),
		clause{locus{},False,array{Left}},
		Truth(clause{locus{},False,array{Right}})
	);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Syntax generation.

struct generate_syntax {

	// Types we must expose to parser.
	using syntax_t   = box<syntax>;
	using syntaxes_t = array<box<syntax>>;
	using error_t    = Verse::error;
	using capture_t  = string;
	using block_t    = block<syntaxes_t,capture_t>;

	// Manipulation operations we must expose to parser.
	static void     SyntaxesAppend(syntaxes_t& as,const syntax_t& a) {as+=array{a};}
	static nat      SyntaxesLength(const syntaxes_t& as) {return Length(as);}
	static syntax_t SyntaxesElement(const syntaxes_t& as,nat i) {return as[i];}

	// Manipulation operations we must expose to abstract syntax generator.
	static void     CaptureAppend(capture_t& s,const capture_t& t) {s+=t;}
	static nat      CaptureLength(const capture_t& s) {return Length(s);}
	static char8    CaptureElement(const capture_t& s,nat i) {return s(i);}

	// Abstract syntax callbacks we must expose to parser.
	template<class... ps> error_t Err(const snippet& Snippet,const char* Code,const ps&... PS) const {
		return error{ToLocus(Snippet),string(Code),0,ToString(PS...)};
	}
	result<syntax_t,error_t> Native(const snippet& Snippet,text Name) const {
		return MakeNativeSyntax(ToString(Name));
	}
	result<syntax_t,error_t> Num(const snippet& Snippet,text Digits,text FractionalDigits,text ExponentSign,text Exponent) const {
		if(Length(FractionalDigits) || Length(Exponent) || ExponentSign)
			return box<syntax::atom>(ToLocus(Snippet),0);//!!to satisfy parsing tests, though unimplemented
		integer D=0;
		for(auto c=Digits.Start; c<Digits.Stop; c++)
			VERSE_ENSURE(*c>='0'&&*c<='9'),D=D*10+(*c-'0');
		rational R=D, d=1;
		for(auto c=FractionalDigits.Start; c<FractionalDigits.Stop; c++)
			VERSE_ENSURE(*c>='0'&&*c<='9'), d/=10, R+=(*c-'0')*d;
		return box<syntax::atom>(ToLocus(Snippet),R);
	}
	result<syntax_t,error_t> NumHex(const snippet& Snippet,text Digits) const {
		integer D=0;
		for(auto c=Digits.Start; c<Digits.Stop; c++)
			D=D*10+DigitValue(*c);
		return box<syntax::atom>(ToLocus(Snippet),D);
	}
	result<syntax_t,error_t> Char8(const snippet& Snippet,char8 ch) const {
		return box<syntax::atom>(ToLocus(Snippet),ch);
	}
	result<syntax_t,error_t> Char32(const snippet& Snippet,char32 ch,bool Code,bool Backslash) const {
		return box<syntax::atom>(ToLocus(Snippet),ch);
	}
	result<syntax_t,error_t> Path(const snippet& Snippet,text Value) const {
		return box<syntax::atom>(ToLocus(Snippet),path(ToString(Value)));
	}
	result<syntax_t,error_t> Invoke(const snippet& Snippet,const syntax_t& Macro,const block_t& Clause,const block_t* DoClause,const block_t* PostClause) const {
		return box<syntax::invoke>(ToLocus(Snippet),Macro,
			ToClause(Clause),
			DoClause?   Truth(ToClause(*DoClause)): False,
			PostClause? Truth(ToClause(*PostClause)): False
		);
	}
	result<syntax_t,error_t> Ident(const snippet& Snippet,text A,text B,text C) const {
		return box<syntax::identifier>(ToLocus(Snippet),False,ToString(A,B,C));
	}
	result<syntax_t,error_t> QualIdent(const snippet& Snippet,const block_t& QualifierBlock,text Name) const {
		GRAMMAR_LET(QualifierSyntax,HelpParenthesis(snippet{},QualifierBlock));
		return box<syntax::identifier>(ToLocus(Snippet),Truth(QualifierSyntax),ToString(Name));
	}
	result<syntax_t,error_t> Call(const snippet& Snippet,mode Mode,const syntax_t& FunctionSyntax,const block_t& ParameterBlock) const {
		VERSE_ENSURE(Mode!=mode::None);
		GRAMMAR_LET(ParameterSyntax,HelpParenthesis(snippet{},ParameterBlock));
		return box<syntax::call>(ToLocus(Snippet),Mode,FunctionSyntax,ParameterSyntax);
	}
	result<syntax_t,error_t> Escape(const snippet& Snippet,const syntax_t& Escaped) const {
		return box<syntax::escape>(ToLocus(Snippet),Escaped);
	}
	result<syntax_t,error_t> PrefixAttribute(const snippet& Snippet,const syntax_t& Left,const syntax_t& Right) const {
		return Right;//!!
	}
	result<syntax_t,error_t> PostfixAttribute(const snippet& Snippet,const syntax_t& Left,const syntax_t& Right) const {
		return Left;//!!
	}
	void Text(capture_t& Capture,const snippet& Snippet,place Place) const {
		Capture+=ToString(Snippet.Text);
	}

	// Internal.
	locus Locus;
	generate_syntax(const locus& Locus0): Locus(Locus0) {}
	locus ToLocus(const snippet& Snippet) const {
		return Snippet.Text || Snippet.StartLine || Snippet.StopLine || Snippet.StartColumn || Snippet.StopColumn?
			locus{Locus.Filename,Locus.StartLine+Snippet.StartLine,(Snippet.StartLine? 0: Locus.StartColumn)+Snippet.StartColumn,Locus.StartLine+Snippet.StopLine,(Snippet.StopLine? 0: Locus.StartColumn)+Snippet.StopColumn}:
			locus{};
	}
	clause ToClause(const block_t& Clause) const {
		return clause{ToLocus(Clause.BlockSnippet),Clause.Specifiers,Clause.Elements,Clause.Form};
	}
	result<syntax_t,error_t> HelpParenthesis(const snippet& Snippet,const block_t& CallBlock,bool Visible=false) const {
		if(Length(CallBlock.Elements)!=1) {
			GRAMMAR_LET(MacroSyntax,Native(snippet{},u8"array"));
			return Invoke(Snippet,MacroSyntax,CallBlock,nullptr,nullptr);
		}
		else return SyntaxesElement(CallBlock.Elements,0);
	}
};
VERSE_NO_INLINE result<box<syntax>,error> ParseVerseSyntax(const char8* S,const locus& Locus=locus{}) {
	auto GenerateSyntax=generate<generate_syntax>{Locus};
	return File(GenerateSyntax,Length(S),S);
}
VERSE_NO_INLINE result<box<syntax>,error> ParseVerseSyntax(const string& S,const locus& Locus=locus{}) {
	string_as_utf8 Storage(S);
	auto GenerateSyntax=generate<generate_syntax>{Locus};
	return File(GenerateSyntax,Storage.Length,Storage.UTF8);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Syntax printing.

bool IsNative(const pin<future<box<syntax>>>& Syntax,const char8* Match) {
	// Just checks identifier string match for now; fix when we expose the native environment nominally.
	if(auto Identifier=Syntax.Cast<syntax::identifier>(); Identifier && Identifier->Matches(Match))
		return true;
	return false;
}
option<array<future<box<syntax>>>> CastSyntaxArray(const pin<future<box<syntax>>>& Y) {
	if(auto Invoke=Y.Cast<syntax::invoke>())
		if(IsNative(Invoke->InvokeMacro,u8"array") && !Invoke->DoClause && !Invoke->PostClause)
			return Truth(Invoke->Clause.Body);
	return False;
}
token MakeToken(const string& Op) {
	for(auto i=nat8(token::FirstParse()); i<ArraySize(Tokens); i++)
		if(Tokens[i].Symbol==Op)
			return token(i);
	return token::None();
}
token TokenFromIdentifier(const string& S) {
	if(Length(S)>0 && S.End(0)!='\'')
		if(auto Token=MakeToken(S); Token && Token->PrefixMode!=mode::None)
			return Token;
	if(Length(S)>=10 && S.Slice(0,9)=="operator'"&&S.EndSlice(0,1)=="'")
		if(auto Token=MakeToken(S!="operator'[]'"? S.Slice(9).EndSlice(1): "["_VS); Token && Token->PostfixMode!=mode::None)
			return Token;
	if(Length(S)>=8 && S.Slice(0,7)=="prefix'"&&S.EndSlice(0,1)=="'")
		if(auto Token=MakeToken(S!="prefix'[]'"? S.Slice(7).EndSlice(1): "["_VS); Token && Token->PrefixMode!=mode::None)
			return Token;
	return token::None();
}
string ToStringSyntax(const encoding& Encoding,const pin<future<box<syntax>>>& a);
string ParenthesizeIf(bool B,const pin<string>& S) {
	return !B? string(S): "("_VS + S + ")"_VS;
}
string ToStringSpaced() {
	return string();
}
template<class s,class... ts> string ToStringSpaced(const s& S,const ts&... TS) {
	auto A=ToString(S);
	auto B=ToStringSpaced(TS...);
	return ToString(A,Length(A)>0 && Length(B)>0 && IsAlnum(A.End(0))&&IsAlnum(B[0])? " ": "",B);
}
string ToStringSyntaxBlock(const encoding& Encoding,const pin<array<future<box<syntax>>>>& as,form Form) {
	auto InnerPrec=Form==form::List? prec::Commas: prec::Expr;
	string rs=False;
	for(nat i=0,n=Length(as); i<n; i++) {
		if(Form==form::Commas && i!=0)
			rs += ","_VS;
		rs += ToStringSyntax(Encoding.Fresh(InnerPrec),as[i]);
		if(Form!=form::Commas)
			rs += i<n-1? "; "_VS: ";"_VS;
	}
	return rs;
}
string ToStringSyntaxClause(const encoding& Encoding,const char* Open,const clause& Clause,const char* Close) {
	string RS;
	for(auto Specifier:Clause.Specifiers)
		RS += "<"_VS + ToStringSyntax(Encoding.Fresh(prec::Choice),Specifier) + ">"_VS;
	return RS+ToString(Open,ToStringSyntaxBlock(Encoding.Fresh(prec::List),Clause.Body,Clause.Form),Close);
}
namespace Verse {
	// Because clause is in syntax which is in the Verse namespace.
	string ExposeToString(const clause& A) {
		return ToStringSyntaxClause(encoding(),"{",A,"}");
	}
}
string ToStringSyntaxOperator(const encoding& Encoding,mode Mode,const pin<future<box<syntax>>>& Macro,const option<future<box<syntax>>>& Parameter,const option<array<future<box<syntax>>>>& Parameters) {
	if(auto Id=Macro.Cast<syntax::identifier>(); Id && !Id->Qualifier) {
		if(auto Token=TokenFromIdentifier(Id->Name)) {
			if(Length(Id->Name)>9 && Id->Name.Slice(0,9)=="operator'" && Token->PostfixMode==Mode) {
				if(Token->PostfixAssoc==assoc::Postfix && Token->PostfixMode==Mode && Parameter) {
					bool Parens = ParenthesizePostfix(Encoding,Token->PostfixPrec);
					auto Left   = ToStringSyntax(Token->PostfixLeftEncoding(Encoding,Parens),Parameter.Coerce());
					return ParenthesizeIf(Parens,ToStringSpaced(Left,Token->Symbol)); // operator'^'[a] is a^.
				}
				else if(Token->PostfixAssoc!=assoc::None && Parameters && Length(Parameters.Coerce())==2) {
					bool Brackets = Id->Name=="operator'[]'";
					bool Parens   = ParenthesizePostfix(Encoding,Token->PostfixPrec);
					auto Left     = ToStringSyntax(Brackets? Encoding.Fresh(prec::List): Token->PostfixLeftEncoding(Encoding,Parens),Parameters.Coerce()[0]);
					auto Right    = ToStringSyntax(Token->PostfixRightEncoding(Encoding,Parens),Parameters.Coerce()[1]);
					return ParenthesizeIf(Parens,Brackets?
						ToString("[",Left,"]",Right):            // operator'[]'[a,b] is [a]b.
						ToStringSpaced(Left,Token->Symbol,Right) // operator'+'[a,b]  is a+b.
					); 
				}
			}
			if((Length(Id->Name)>0 && Id->Name.End(0)!='\'' || Length(Id->Name)>=7 && Id->Name.Slice(0,7)=="prefix'") &&
				Token->PrefixMode==Mode && Parameter) {
				bool IsColon = Id->Name=="prefix':'";
				bool IsVar   = Id->Name=="var" || Id->Name=="set" || Id->Name=="ref" || Id->Name=="alias";
				bool Parens  = ParenthesizePrefix(Encoding,IsColon&&Encoding.AllowIn || IsVar? prec::Choice: Token->PrefixPrec);
				return ParenthesizeIf(Parens,ToStringSpaced( // prefix'-'[a] as -a.
					Id->Name.End(0)!='\''? Id->Name: Id->Name.Slice(7).EndSlice(1),
					ToStringSyntax(Encoding.Fresh(IsColon? prec::Choice: Token->PrefixPrec,IsColon,IsColon||Encoding.FollowingIn&&!Parens),Parameter.Coerce()))); 
			}
		}
	}
	return False;
}
string ToStringSyntax(const encoding& Encoding,const pin<future<box<syntax>>>& S) {
	if(auto Atom=S.Cast<syntax::atom>())
		return ToCode(Atom->AtomValue);
	else if(auto Identifier=S.Cast<syntax::identifier>()) {
		if(!Identifier->Qualifier)
			return Identifier->Name;
		return ToString("(",ToStringSyntax(Encoding.Fresh(prec::List),Identifier->Qualifier.Coerce()),":)",Identifier->Name);
	}
	else if(auto Escape=S.Cast<syntax::escape>()) {
		auto Parens = ParenthesizePrefix(Encoding,prec::Def);
		return ParenthesizeIf(Parens, "&"_VS + ToStringSyntax(Encoding.Fresh(prec::Def),Escape->Escaped));
	}
	else if(auto Call=S.Cast<syntax::call>()) {
		if(string OS=ToStringSyntaxOperator(Encoding,Call->CallMode,Call->FunctionSyntax,Truth(Call->ParameterSyntax),CastSyntaxArray(Call->ParameterSyntax)))
			return OS;
		auto FunctionString=ToStringSyntax(Encoding.Fresh(prec::Call),Call->FunctionSyntax);
		switch(Call->CallMode) {
		case mode::Open:   return ToString(FunctionString,"(",ToStringSyntax(Encoding.Fresh(prec::List  ),Call->ParameterSyntax),")");
		case mode::Closed: return ToString(FunctionString,"[",ToStringSyntax(Encoding.Fresh(prec::List  ),Call->ParameterSyntax),"]");
		case mode::With:   return ParenthesizeIf(Encoding.FollowingIn,ToString(FunctionString,"<",ToStringSyntax(Encoding.Fresh(prec::Choice),Call->ParameterSyntax),">"));
		case mode::None:   Err();
		}
	}
	else if(auto Invoke=S.Cast<syntax::invoke>()) {
		if(auto Array=CastSyntaxArray(S)) {
			auto as=Array.Coerce();
			nat i=0,n=Length(as);
			string Result=False;
			if(i<n) {
				loop:
				if(auto ElementAtom=as[i].Cast<syntax::atom>()) if(auto Ch=ElementAtom->AtomValue.Cast<char8>()) {
					Result+=array{Ch.Coerce()};
					if(i++; i<n)
						goto loop;
					return ToCode(Result);
				}
			}
			auto ArrayPrec = Invoke->Clause.Form==form::List? prec::List: prec::Commas;
			return Length(as)!=1?
				ParenthesizeIf(Encoding.Prec>ArrayPrec,ToStringSyntaxBlock(Encoding,as,Invoke->Clause.Form)):
				ToString("array{",ToStringSyntax(Encoding,as[0]),"}");
		}
		if(!Invoke->Clause.Specifiers && !Invoke->DoClause && !Invoke->PostClause)
			if(string OS=ToStringSyntaxOperator(Encoding,mode::With,Invoke->InvokeMacro,Length(Invoke->Clause.Body)==1? Truth(Invoke->Clause.Body[0]): False,Truth(Invoke->Clause.Body)))
				return OS;
		if(Length(Invoke->Clause.Body)==1 && Invoke->DoClause && !Invoke->DoClause->Specifiers && !Invoke->PostClause)
			if(auto Id=Invoke->InvokeMacro.Cast<syntax::identifier>(); Id && !Id->Qualifier)
				if(auto Token=TokenFromIdentifier(Id->Name); Token && !Invoke->Clause.Specifiers && Token->PostfixMode==mode::With) {
					bool Parens = ParenthesizePostfix(Encoding,Token->PostfixPrec);
					bool Braces = Length(Invoke->DoClause->Body)!=1 || 
									Id->Name=="operator'over'" || Id->Name=="operator'when'" || Id->Name=="operator'where'" || Id->Name=="operator'while'";
					auto Right  = Braces?
						ToString("{",ToStringSyntaxBlock(Encoding.Fresh(prec::List),Invoke->DoClause->Body,Invoke->DoClause->Form),"}"):
						ToStringSyntax(Token->PostfixRightEncoding(Encoding,Parens),Invoke->DoClause->Body[0]);
					bool IsColonDef = Token==token(u8":=") && Length(Right)>=1 && Right[0]==':' && (Length(Right)==1 || Right[1]!='=');
					auto Left       = ToStringSyntax(Token->PostfixLeftEncoding(Encoding,Parens),Invoke->Clause.Body[0]);
					return ParenthesizeIf(Parens,IsColonDef?
						ToStringSpaced(Left,Right):
						ToStringSpaced(Left,Token->Symbol,Right));
				}
		auto Result = ToStringSyntax(Encoding.Fresh(prec::Call),Invoke->InvokeMacro);
		auto Parens = bool(Invoke->DoClause);
		Result+=ToStringSyntaxClause(Encoding,Parens?"(":"{",Invoke->Clause,Parens?")":"}");
		if(Invoke->DoClause)
			Result+=ToStringSyntaxClause(Encoding,"{",Invoke->DoClause.Coerce(),"}");
		if(!Invoke->PostClause)
			return Result;
		if(!Invoke->PostClause->Specifiers && Length(Invoke->PostClause->Body)==1)
			if(auto PostInvoke=Invoke->PostClause->Body[0].Cast<syntax::invoke>())
				if(PostInvoke && IsNative(PostInvoke->InvokeMacro,u8"catch"))
					return Result+ToStringSyntax(Encoding,PostInvoke.Coerce());
		return Result+ToStringSpaced(IsNative(Invoke->InvokeMacro,u8"if")? "else"_VS: "until"_VS,
			ToStringSyntaxClause(Encoding,"{",Invoke->PostClause.Coerce(),"}"));
	}
	VERSE_UNEXPECTED;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier errors.

// Priority 0:      Evaluator immediately-fatal errors and verifier asynchronous leaf rejection.
// Priority 1-4:    Verifier asynchronous non-leaf rejection (identifier resolution stall with no results,identifier resolution stall with one result,other).
// Priority 10..17: Evaluator suspension with disallowed effects (10-fails,11-imperatives,12-abstracts,13-iterates,14-resolves,15-decides,16-succeeds-where-disallowed,19-runtime).
// Priority 20..27: Same as above, but deprioritized because this error is dependent on a more specific one nested beneath it.
// Priority 40:     Success.

// Errors derived from verifier effects.
auto A00(const locus& Locus,const string& Internal) {return error{Locus,"A00"_VS,12,"Abstract expression not allowed here"_VS,Internal};}
auto D00(const locus& Locus,const string& Internal) {return error{Locus,"D00"_VS,15,"Decidable potential failure isn't allowed here"_VS,Internal};}
auto F00(const locus& Locus,const string& Internal) {return error{Locus,"F00"_VS,10,"Failure isn't allowed here"_VS,Internal};}
auto I00(const locus& Locus,const string& Internal) {return error{Locus,"I00"_VS,13,"Iteration isn't allowed here"_VS,Internal};}
auto S00(const locus& Locus,const string& Internal) {return error{Locus,"S00"_VS,40,"Success"_VS,Internal};}
auto U00(const locus& Locus,const string& Internal) {return error{Locus,"U00"_VS,14,"Expression resolves undecidably"_VS,Internal};}
auto X00(const locus& Locus,const string& Internal,fx Fx,fx AllowFx) {return error{Locus,"X00"_VS,11,ToString("Effects aren't allowed here: "_VS,Fx),Internal};}
auto S01(const locus& Locus,const string& Internal) {return error{Locus,"S01"_VS,16,"Success not allowed here"_VS,Internal};}

// Runtime errors.
auto R00(const locus& Locus,const string& Internal) {return error{Locus,"R00"_VS,19,"Runtime stalled"_VS,Internal};}
auto R01(const locus& Locus,const string& S)        {return error{Locus,"R01"_VS,0,ToString("Err called with: "_VS,S)};}
auto R02(const locus& Locus)                        {return error{Locus,"R02"_VS,0,"Test unexpectedly succeeded"_VS};}
auto R99(const locus& Locus)                        {return error{Locus,"R99"_VS,0,"Out of memory"_VS};}

// Bespoke errors.
auto C00(const locus& Locus,const string& Internal) {return error{Locus,"C00"_VS,0,"Call to non-function"_VS,Internal};}
auto D01(const locus& Locus,const string& Internal) {return error{Locus,"D01"_VS,1,"Unsupported definition left-hand side"_VS,Internal};}
auto D02(const locus& Locus,const string& Internal) {return error{Locus,"D02"_VS,1,"Tuple definition requires a tuple on the right hand side"_VS,Internal};}
auto M00(const locus& Locus,const string& Internal) {return error{Locus,"M00"_VS,1,"Macro invocation requires a macro or nominal type"_VS,Internal};}
auto M01(const locus& Locus,const string& Internal) {return error{Locus,"M01"_VS,2,"Macro invocation stalled without a macro or nominal type"_VS,Internal};}
auto M02(const locus& Locus,const char* What)       {return error{Locus,"M02"_VS,1,ToString("Bad parameters for macro \""_VS,What,"\""_VS),string_fix_me(What)};}
auto N00(const locus& Locus,const string& Ident)    {return error{Locus,"N00"_VS,1,ToString("Identifier \"",Ident,"\" not found"_VS)};}
auto N02(const locus& Locus,const string& Ident)    {return error{Locus,"N02"_VS,3,ToString("Identifier \"",Ident,"\" found but search stalled"_VS)};} // Unexpected because a proper error should be higher priority.
auto N03(const locus& Locus,const string& Ident)    {return error{Locus,"N03"_VS,1,ToString("Identifier \""_VS,Ident,"\" is ambiguous here"_VS)};}
auto N04(const locus& Locus,const string& Ident)    {return error{Locus,"N04"_VS,1,ToString("Duplicate identifier \""_VS,Ident,"\" declared here"_VS)};}
auto N05(const locus& Locus,const string& Ident,const string& Internal) {return error{Locus,"N05"_VS,1,ToString("Identifier \""_VS,Ident,"\" not found in this context"_VS),Internal};}
auto N10(const locus& Locus)                        {return error{Locus,"N10"_VS,1,ToString("Right hand side of destructuring tuple definition must be a tuple"_VS)};}
auto P00(const locus& Locus)                        {return error{Locus,"P00"_VS,1,"Parameter mismatch"_VS};}
auto T01(const locus& Locus)                        {return error{Locus,"T01"_VS,0,"Test unexpectedly succeeded"_VS};}
auto V02(const locus& Locus,const string& Text)     {return error{Locus,"V02"_VS,0,ToString("Rejects called with: "_VS,Text)};}
auto V02(const locus& Locus)                        {return error{Locus,"V02"_VS,0,ToString("Rejects called with unresolved expression"_VS)};}
auto V04(const locus& Locus)                        {return error{Locus,"V04"_VS,0,"Syntax resolves inconsistently across choices"_VS};}
auto V04(const locus& Locus,tuple<>)                {return error{Locus,"V04"_VS,0,"Register allocation resolves inconsistently across choices"_VS};}
auto V10(const locus& Locus)                        {return error{Locus,"V10"_VS,2,"Value doesn't belong to type"_VS};}
auto V99(const locus& Locus)                        {return error{Locus,"V99"_VS,2,"Unexpected"_VS};}
auto X30(const locus& Locus,const string& Internal) {return error{Locus,"X30"_VS,0,"Bad effect specifier"_VS,Internal};}
auto X31(const locus& Locus,const string& Internal) {return error{Locus,"X31"_VS,1,"This specifier is not allowed here"_VS,Internal};}
auto X32(const locus& Locus)                        {return error{Locus,"X32"_VS,1,"Contradictory effects specifiers"_VS};}
auto Here(const locus& Locus,const string& Internal) {
	return [=]{return R00(Locus,Internal);};
}
auto HereContext(const locus& Locus,const string& Internal) {
	return [=](const context&) {return R00(Locus,Internal);};
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Containers to later generalize and move to library.

template<class t> struct future_array: public managed {
	mutable nat     CurrentLength;
	var<map<nat,t>> Elements;
	future_array(tuple<>): CurrentLength(0) {}
	template<class... ps> requires requires(ps... PS) {(t(PS),...);} future_array(const ps&... PS): CurrentLength(0) {
		(Add(PS),...);
	}
	nat Add(const t& T) const {
		return Elements.Set(CurrentLength,T), CurrentLength++;
	}
	nat Add() const {
		return CurrentLength++;
	}
	void Init(nat n,const t& Value) const {
		VERSE_ENSURE(!Elements.Set(n,Value));
	}
};

template<class k,class v> struct future_map {
	var<map<k,future<option<v>>>> Map; // So we can enqueue queries whether they will later succeed or fail.
	var<nat>                      Introductions;
	future_map(): 
		Map(), //(Thread->Visibility()->Run([&]{return var<map<k,future<option<v>>>>();})), 
		Introductions(1U) {} //(Thread->Visibility()->Run([&]{return var<nat>(1U);})) {}
	future<option<v>> FutureGet(const k& Key) const {
		if(*Introductions==0)
			return Map.GetInit(Key,False);
		else
			return Map.GetInit(Key);
	}
	bool SetBatch(const k& Key,const v& Value) const {
		if(auto VO=Map.Get(Key); !VO)
			return Map.Set(Key,Truth(Value)), false;
		else if(!VO.Coerce().IsA())
			return VO->ResolveBatch(Truth(Value)), false;
		else
			return true;
	}
	void EliminateBatch() const {
		Introductions--;
		VERSE_ENSURE(*Introductions>=0);
		if(*Introductions==0)
			for(auto[_,Cursor]:Map)
				if(auto V=Cursor.ReadValue(); !V.IsA())
					V.ResolveBatch(option<v>(False));
	}
	void Introduce() const {
		VERSE_ENSURE(*Introductions!=0);
		Introductions++;
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier strength.

using           strength           = fx;
static const fx strength_succeeds  = succeeds+ambiguates;
static const fx strength_decides   = decides+ambiguates;
static const fx strength_resolves  = abstracts;
static const fx strength_unrelates = effects;
void EnsureStrengthValid(strength Strength) {
	VERSE_ENSURE(Strength==strength_succeeds || Strength==strength_decides || Strength==strength_resolves || Strength==strength_unrelates);
}
strength StrengthSuccessor(strength Strength) {
	EnsureStrengthValid(Strength);
	return Strength==strength_succeeds? strength_decides: Strength==strength_decides? strength_resolves: strength_unrelates;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier head shape.

// Extrema of head shapes.
struct false_shape {
	friend equality_ordering ExposeCompare(const false_shape& A,const false_shape& B) {
		return equality_ordering(true);
	}
	friend nat ExposeHash(const false_shape& A) {
		return 0;
	}
	friend string ExposeToString(const false_shape& A) {
		return "false_shape"_VS;
	}
};

// Signature shape.
struct signature_shape {
	path        Path;
	option<nat> ArrayLength;
	signature_shape(const path& Path0,const option<nat>& ArrayLength0=False):
		Path(Path0), ArrayLength(ArrayLength0) {}
	signature_shape(nat ArrayLength0):
		Path("/Verse.org/function"_VP), ArrayLength(Truth(ArrayLength0)) {}
	friend partial_ordering ExposeCompare(const signature_shape& A,const signature_shape& B) {
		if(A.Path!=B.Path)
			return partial_ordering(false,false);
		else
			return partial_ordering(B.ArrayLength<=A.ArrayLength,A.ArrayLength<=B.ArrayLength);
	}
	friend nat ExposeHash(const signature_shape& S) {
		return ExposeHash(S.Path)+ExposeHash(S.ArrayLength);
	}
	friend string ExposeToString(const signature_shape& S) {
		if(!S.ArrayLength)
			return ToString(S.Path);
		else
			return ToString("["_VS,S.ArrayLength? ToString(S.ArrayLength.Coerce()): False,"]any"_VS);
	}
};

// Head shapes, a closed world type representing false|atom(A)|signature(S)|comparable|any.
// This is and must be a full lattice including atoms so RepShape can intersect losslessly.
// If H, it's either atom, false_shape, or signature_shape; else it's either any or comparable.
struct shape {
	option<comparable> H;
	bool ShapeComparable;
	shape(): shape(False,false) {}
	static shape FromFalse() {return shape(Truth(false_shape{}),true);}
	static shape FromComparable(bool ShapeComparable0) {return shape(False,ShapeComparable0);}
	static shape FromSignature(const signature_shape& H0,bool ShapeComparable0) {return shape(Truth(H0),ShapeComparable0);}
	static shape FromAtom(const comparable& H0) {return shape(Truth(H0),true);}
	friend shape Intersection(const shape& A,const shape& B) {
		bool C=A.ShapeComparable||B.ShapeComparable;
		if(!A.H || A.H==B.H)
			return shape(B.H,C);
		else if(!B.H)
			return shape(A.H,C);
		auto SA=A.H->Cast<signature_shape>(), SB=B.H->Cast<signature_shape>();
		if(SA && SB && SA->Path==SB->Path && (SA->ArrayLength==SB->ArrayLength || !SA->ArrayLength || !SB->ArrayLength))
			return shape::FromSignature(signature_shape(SA->Path,SA->ArrayLength.ElseIf(SB->ArrayLength)),C);
		else if(SA && !SA->ArrayLength && !SB && Signature(B.H.Coerce())==SA->Path)
			return shape(B.H,C);
		else if(SB && !SB->ArrayLength && !SA && Signature(A.H.Coerce())==SB->Path)
			return shape(A.H,C);
		return shape(Truth(false_shape{}),true);
	}
	friend nat ExposeHash(const shape& A) {
		return Hash(A.H)^Hash(A.ShapeComparable);
	}
	friend string ExposeToString(const shape& A) {
		if(!A.H && !A.ShapeComparable)
			return "any?"_VS;
		else if(!A.H)
			return "comparable?"_VS;
		else if(A.H->Cast<false_shape>())
			return "false?"_VS;
		else if(A.H->Cast<signature_shape>())
			return ToString(A.H.Coerce());
		else
			return ToCode(A.H.Coerce());
	}
	friend partial_ordering ExposeCompare(const shape& A,const shape& B) {
		auto C=Intersection(A,B);
		return partial_ordering(A.H==C.H && A.ShapeComparable==C.ShapeComparable,B.H==C.H && B.ShapeComparable==C.ShapeComparable);
	}
	bool IsFail() const {
		return H && H->Cast<false_shape>();
	}
	bool IsAny() const {
		return !H && !ShapeComparable;
	}
	void Intersect(bool& Narrowed,const shape& Other) {
		auto C=Intersection(*this,Other);
		if(H!=C.H || ShapeComparable!=C.ShapeComparable)
			Narrowed=true, *this=C;
	}
private:
	shape(const option<comparable>& H0,bool ShapeComparable0): H(H0), ShapeComparable(ShapeComparable0) {}
};
template<class t> shape ShapeOf() {
	bool ShapeComparable = IsComparable<t>();
	if constexpr(IsEqual<t,falsity>)
		return shape::FromFalse();
	else if constexpr(HasStaticSignature<t>)
		return shape::FromSignature(signature_shape(expose<t>::ExposeStaticSignature()),ShapeComparable);
	else
		return shape::FromComparable(ShapeComparable);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier data flow graph vertices.

struct vertex_managed; expose_mutable ExposeUnique(const vertex_managed&);
struct call_vertex;    expose_mutable ExposeUnique(const call_vertex&);
struct caller_vertex;  expose_mutable ExposeUnique(const caller_vertex&);
struct array_vertex;   expose_mutable ExposeUnique(const array_vertex&);
struct macro_vertex;   expose_mutable ExposeUnique(const macro_vertex&);

// Data flow vertices. Equality means they're the same vertex (equal atomic values are always the same vertex).
struct vertex {
	vertex();
	explicit vertex(const tuple<>&);
	explicit vertex(const comparable& P): Value(P) {VERSE_ASSERT(!IsA<array<>>(Value));}
	template<class t> requires(IsDerived<t,vertex_managed>) vertex(const box<t>& P): Value(P) {} // Exists so comparable ctor can be explicit, to avoid unintentional conversions.
	pin<context> Context() const {
		return Value.Context();
	}
	shape GetShape() const;
	option<comparable> CastHead() const;
	option<comparable> CastAtom() const;
	template<class t> auto CastVertex() const {
		return Value.Cast<t>();
	}
	friend nat ExposeHash(const vertex& U) {
		return HashDynamic(U.Value);
	}
	void SetVertexName(const var<string>& S) const;
	friend string ExposeToString(const vertex& U) {
		if(auto A=U.CastAtom())
			return ToCode(A.Coerce());
		else
			return ToString(U.Value);
	}
	friend equality_ordering ExposeCompare(const vertex& U,const vertex& V) {
		return CompareEquality(U.Value,V.Value);
	}
	friend void ExposeExplicit(const vertex&); // Prevent implicit conversion to runtime future<>.
private:
	comparable Value;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier managed vertex data.

// Verifier vertex base class.
struct vertex_managed: managed {
	// Since our vertices are box<t extends vertex_managed> which inherits future<>, UnifyStep and FailTargetStep
	// verifier & runtime overloads favor future<> over vertex, so we must explicitly use vertex there.
	var<bag<vertex>>        VertexPredecessors;
	var<bag<vertex>>        VertexHeadFailSources;
	shape                   VertexShape;
	bool                    VertexHead;
	var<string>             VertexName;
	array<box<call_vertex>> VertexElements;
	vertex_managed(bool VertexHead0=false,const shape& VertexShape0=shape()):
		VertexShape(VertexShape0), VertexHead(VertexHead0) {}
	friend string ExposeToString(const vertex_managed& V);
};

// Variable vertex.
struct variable_vertex: vertex_managed {};

// Failure vertex.
struct fail_vertex: vertex_managed {
	fail_vertex(): vertex_managed(false,ShapeOf<falsity>()) {}
};

// Verifier call vertex, representing the result of a function call.
struct call_vertex: vertex_managed {
	vertex CallFunctionVertex, CallParameterVertex;
	option<box<caller_vertex>> CallerVertex; // For efficiently tracking non-<computes> betas.
	call_vertex(const vertex& CallFunctionVertex0,const vertex& CallParameterVertex0,const option<box<caller_vertex>>& CallerVertex0):
		CallFunctionVertex(CallFunctionVertex0), CallParameterVertex(CallParameterVertex0),
		CallerVertex(CallerVertex0) {}
	friend string ExposeToString(const call_vertex& R);
};

// The original call vertex initiated by an op, which spawns subsequent callee_beta_vertex for lambdas but not products.
struct caller_vertex: call_vertex {
	locus             CallOpLocus;
	fx                CallProductFx;
	var<bag<context>> CallBetaContexts;
	caller_vertex(const vertex& CallFunctionVertex0,const vertex& CallParameterVertex0,const locus& CallOpLocus0):
		call_vertex(CallFunctionVertex0,CallParameterVertex0,Truth(box(*this))), CallOpLocus(CallOpLocus0), CallProductFx(effects) {}
};

// A call vertex associated with a beta reduction.
struct callee_beta_vertex: call_vertex {
	box<caller_vertex> CallerVertex;
	callee_beta_vertex(const vertex& CallFunctionVertex0,const vertex& CallParameterVertex0,const box<caller_vertex>& CallOpVertex0,const context& CallBetaContext0,const box<caller_vertex>& CallerVertex0):
		call_vertex(CallFunctionVertex0,CallParameterVertex0,Truth(CallOpVertex0)), CallerVertex(CallerVertex0) {
		CallerVertex->CallBetaContexts.Set(CallBetaContext0);
	}
	friend string ExposeToString(const callee_beta_vertex& R);
};

// Forward declarations.
struct abstraction_vertex; expose_mutable ExposeUnique(const abstraction_vertex&);
struct function_vertex;    expose_mutable ExposeUnique(const function_vertex&);
struct lambda_vertex;      expose_mutable ExposeUnique(const lambda_vertex&);
template<> struct expose<abstraction_vertex>: default_expose<abstraction_vertex> {
	path ExposeStaticSignature() {return "/Verse.org/function"_VP;}
};

// Verifier abstraction (= function plus abstract effects) vertex.
struct abstraction_vertex: vertex_managed {
	fx AbstractionKeepFx, AbstractionAddFx;
	vertex AbstractionFunctionVertex;
	abstraction_vertex(const vertex& AbstractionFunctionVertex0,fx AbstractionKeepFx0,fx AbstractionAddFx0,const shape& VertexShape0=shape::FromSignature(signature_shape("/Verse.org/function"_VP),false)):
		vertex_managed(true,VertexShape0),
		AbstractionKeepFx(AbstractionKeepFx0), AbstractionAddFx(AbstractionAddFx0),
		AbstractionFunctionVertex(AbstractionFunctionVertex0) {}
	friend string ExposeToString(const abstraction_vertex& V);
};

// Verifier function vertex.
struct function_vertex: abstraction_vertex {
	function_vertex(fx AbstractionKeepFx0,fx AbstractionAddFx0,const shape& VertexShape0=shape::FromSignature(signature_shape("/Verse.org/function"_VP),false)):
		abstraction_vertex(box(*this),AbstractionKeepFx0,AbstractionAddFx0,VertexShape0) {}
	virtual current_step OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0)=0;
	friend string ExposeToString(const function_vertex& V);
};

// Make a lambda vertex with a handler.
using on_verifier_call_step=function<current_step(const box<lambda_vertex>&,const box<caller_vertex>& CallerVertex,const step& Step0)>;
struct lambda_vertex: function_vertex {
	on_verifier_call_step F;
	lambda_vertex(const on_verifier_call_step& F0): function_vertex(effects,no_effects), F(F0) {}
	current_step OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0) override {
		return F(box(*this),CallerVertex,Step0);
	}
};

// Verifier input function vertex.
// Exists to integrate op.function inhabitance decision with verifier data flow,
// including shutting up op.call to input that we're generating.
struct input_function_vertex: function_vertex {
	vertex InputVertex,OutputLambdaVertex;
	input_function_vertex(const vertex& InputVertex0,const vertex& OutputLambdaVertex0):
		function_vertex(effects,no_effects), InputVertex(InputVertex0), OutputLambdaVertex(OutputLambdaVertex0) {}
	current_step OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0) override;
};
template<> struct expose<input_function_vertex>: expose<function_vertex> {};

// Verifier array vertex.
struct array_vertex: function_vertex {
	array_vertex(const array<box<call_vertex>>& ArrayElements0):
		function_vertex(effects,no_effects,shape::FromSignature(signature_shape(Length(ArrayElements0)),false)) {
		VertexElements=ArrayElements0;
	}
	array_vertex(nat Count0): function_vertex(effects,no_effects,shape::FromSignature(signature_shape(Count0),false)) {}
	current_step OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0) override;
	friend string ExposeToString(const array_vertex& V);
};

// Printing managed vertices.
string ToStringVertexManaged(const box<vertex_managed>& V) {
	static nat VertexIndex;
	if(!*V->VertexName)
		V->VertexName=ToString("v"_VS,VertexIndex++);
	return *V->VertexName;
}
string ExposeToString(const vertex_managed& V) {
	return ToStringVertexManaged(V);
}
string ExposeToString(const abstraction_vertex& V) {
	static nat AbstractionIndex;
	if(!*V.VertexName)
		V.VertexName=ToString("a"_VS,AbstractionIndex++);
	return *V.VertexName;
}
string ExposeToString(const function_vertex& V) {
	static nat FunctionIndex;
	if(!*V.VertexName)
		V.VertexName=ToString("f"_VS,FunctionIndex++);
	return *V.VertexName;
}
string ExposeToString(const array_vertex& V) {
	if(!*V.VertexName)
		V.VertexName=ToString(ToStringVertexManaged(V),"<",Length(V.VertexElements),">");
	return *V.VertexName;
}
string ExposeToString(const call_vertex& V) {
	if(!*V.VertexName)
		V.VertexName=ToString(V.CallFunctionVertex,"[",V.CallParameterVertex,"]");
	return *V.VertexName;
}
string ExposeToString(const callee_beta_vertex& V) {
	if(!*V.VertexName)
		V.VertexName=ToString(V.CallerVertex,".",V.CallFunctionVertex,"[",V.CallParameterVertex,"]");
	return *V.VertexName;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier context structures.

// Visibility.
enum class visibility: nat8 {VerifyTop,Verify,Iterate};
string ExposeToString(visibility E) {
	if(E==visibility::VerifyTop  ) return "verify_top"_VS;
	else if(E==visibility::Verify) return "verify"_VS;
	else                           return "iterate"_VS;
}

// Relation subscriptions.
struct relation_subscriber_managed;
expose_mutable ExposeUnique(const relation_subscriber_managed&);
using relation_subscriber = box<relation_subscriber_managed>;
using relation_publisher  = var<bag<relation_subscriber>>;
struct relation_subscriber_managed: managed, disposable {
	// Publishers could be generalize to a pair of a Visibility,Equation indicating Visibility->RelationPublishers[Equation], where the equation is in Visibility or an ancestor.
	var<bag<relation_publisher>> Publishers; // Publishers we're subscribing to; for maintaining referential integrity.
	void OnCloned(cloner& Cloner) override {
		if(IsLive)
			for(auto[Publisher,_]:Publishers)
				if(!(Cloner.TargetIterate<=Publisher.Context()))
					Publisher.Set(box(*this));
	}
	virtual void OnSubstitute(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation,bool AllowDisposedSource) {}
	virtual void OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation) {}
	virtual void OnRelate(const inferences& Inferences,const box<call_vertex>& Call,bool AllowDisposedSource) {}
	void Dispose() override {
		// Called for equation_managed subclass of relation_subscriber_managed.
		VERSE_ENSURE(IsLive);
		disposable::Dispose();
		for(auto[Publisher,_]:Publishers)
			VERSE_ENSURE(Publisher.Remove(*this));
		Publishers=False;
	}
	bool SubscribeTo(const relation_publisher& Publisher) {
		if(!Publisher.Set(*this))
			return Publishers.Set(Publisher), false;
		return true;
	}
};

// Describes exploration context vertex equality assumptions of the form fx{v0=v1}@c from the specification
// in the form of a nested union-find data structure, and caches other context-specific vertex information.
expose_mutable ExposeUnique(const struct equate_managed&); using equate=box<equate_managed>;
struct equate_managed: managed {

	// Persistent.
	equation                                              Equation;
	strength                                              NextStrength;
	option<equate>                                        NextRep;
	vertex                                                EquateVertex;
	shape                                                 SucceedsRepShape;

	// Transient information during data flow analysis.
	var<bag<equate>>                                      Dominators;
	var<bag<equate>>                                      MatchingCallReps;
	var<map<equate,equate>>                               SucceedsPredecessors;
	var<map<var<bag<equate>>,tuple<option<equate>,nat8>>> SucceedsPredecessorDominators;
	shape                                                 FlowShape;

	// Operations.
	equate_managed(const equation& Equation0,const vertex& EquateVertex0):
		Equation(Equation0), NextStrength(strength_unrelates), EquateVertex(EquateVertex0) {
		SucceedsRepShape = EquateVertex.GetShape();
	}
	void SetNextStrength(strength NextStrength1) {
		EnsureStrengthValid(NextStrength1);
		NextStrength=NextStrength1;
	}
	equate Rep(strength Strength) {
		if(Strength<NextStrength)
			return pin(box(*this));
		else if(auto L1=NextRep->Rep(NextStrength); NextStrength==Strength)
			return NextRep=Truth(L1), L1;
		else
			return L1->Rep(Strength);
	}
	bool IsDominator(const var<bag<equate>>& Dominators) {
		for(auto[R,_]:MatchingCallReps)
			if(Dominators.Has(R))
				return true;
		return false;
	}
};
string ExposeToString(const equate& Equate);
void DumpEquate(const equate& L) {
	Print("Equate ",L," Dominators=",L->Dominators);
}

// Equation maps.
template<class v> struct equation_map_managed: public relation_subscriber_managed {
	var<map<equation,v>> Map;
	void OnSubstitute(const inferences& Inferences,const equation& Target,const equation& Source,bool AllowDisposedSource) override;
	void OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation) override;
	void OnRelate(const inferences& Inferences,const box<call_vertex>& Call,bool AllowDisposedSource) override;
	v GetSubscribeToEquationHere(const equation& Target);
};
template<> void equation_map_managed<relation_publisher>::OnSubstitute(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation,bool AllowDisposedSource);
template<> void equation_map_managed<relation_publisher>::OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation);
template<> void equation_map_managed<relation_publisher>::OnRelate(const inferences& Inferences,const box<call_vertex>& Call,bool AllowDisposedSource);
template<class v> using equation_map=box<equation_map_managed<v>>;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Registers.

struct reg {
	nat         RegDepth;         // Depth of frame where we reside.
	future<nat> RegBase;          // Base position in frame.
	nat         RegOffset;        // If this is an array element, else 0.
	array<reg>  ArrayElementRegs; // If this is an array, else empty.
	var<string> RegName;          // Name for debugging, if not blank.
	reg(nat Depth0,const future<nat>& RegBase0,nat RegOffset0=0,nat n=0,const string& S=False):
		RegDepth(Depth0), RegBase(RegBase0), RegOffset(RegOffset0),
		ArrayElementRegs(n && RegOffset==0? For(n,[&](nat i){return reg(Depth0,RegBase0,i+1,n,S? ToString(S,"<",n,">[",i,"]"): False);}): False),
		RegName(S&&n&&RegOffset==0? ToString(S,"<",n,">"): S) {}
	reg(nat Index0): RegDepth(Max<nat>()), RegBase(Index0), RegOffset(0) {}
	explicit reg(): RegDepth(Max<nat>()), RegBase(Max<nat>()), RegOffset(0) {}
	nat CoerceIndex() const {
		VERSE_ENSURE(RegDepth!=Max<nat>());
		return RegBase.Coerce()+RegOffset;
	}
	friend string ToStringReg(const reg& a) {
		auto   io=a.RegBase.Cast();
		return a.RegDepth!=Max<nat>()? ToString(a.RegDepth,"r"_VS,io? ToString(io.Coerce()): "?"_VS): io.Coerce()!=nat(-1)? ToString(io.Coerce()): "false"_VS;
	}
	friend string ExposeToString(const reg& a) {
		if(auto S=*a.RegName)
			return S;
		return ToStringReg(a);
	}
	friend equality_ordering ExposeCompare(const reg& A,const reg& B) {
		return equality_ordering(A.RegDepth==B.RegDepth && A.RegBase.Coerce()==B.RegBase.Coerce() &&
			A.RegOffset==B.RegOffset && A.ArrayElementRegs==B.ArrayElementRegs);
	}
};
struct regs: future<var<array<reg>>> {
	regs(): future<var<array<reg>>>(TopContext->Run([&]{
		return future<var<array<reg>>>();
	})) {}
	regs(nat Depth,const array<string>& RegNames=False): future<var<array<reg>>>(TopContext->Run([&]{
		return var<array<reg>>(For(Length(RegNames),[&](nat i) {return reg(Depth,i,0,0,RegNames[i]);}));
	})) {}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Stage sites.

using stage_site_future=future<nat>;
using stage_site=nat;
stage_site_future FreshStageSite() {
	return stage_site_future();
}
stage_site CoerceStageSite(const stage_site_future& F) {
	return F.Coerce();
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Polymorphic frames for runtime and verifier.

enum class frame_sigil {FrameSigil};
static constexpr auto FrameSigil = frame_sigil::FrameSigil;
using symbol_map = box<future_map<string,reg>>;
expose_mutable ExposeUnique(const struct verifier_context_state&);

template<class unificand> struct frame_managed: managed {
	future_array<unificand> FrameUnificands;
	regs                    FrameRegs;
	fx                      FrameAllowFx;
	frame_managed(frame_sigil,const regs& FrameRegs0,fx FrameAllowFx0=effects,const future_array<unificand>& FrameUnificands0=False): FrameUnificands(FrameUnificands0), FrameRegs(FrameRegs0), FrameAllowFx(FrameAllowFx0) {}
	frame_managed(frame_sigil,const regs& FrameRegs0,fx FrameAllowFx0,const future_array<unificand>& FrameUnificands0,const symbol_map& ScopeSymbols0): FrameUnificands(FrameUnificands0), FrameAllowFx(FrameAllowFx0), FrameRegs(FrameRegs0) {}
};
struct scope_managed: frame_managed<vertex> {
	symbol_map ScopeSymbols; // Symbols defined here, coinciding with Ident@c:=r inference rule assumptions.
	scope_managed(frame_sigil,const regs& ScopeRegs0,fx FrameAllowFx0,const future_array<vertex>& Locals0=False,const symbol_map& ScopeSymbols0=symbol_map{}):
		frame_managed<vertex>(FrameSigil,ScopeRegs0,FrameAllowFx0,Locals0), ScopeSymbols(ScopeSymbols0) {}
};

using run_frame    = box<frame_managed<future<>>>;
using beta_frame   = box<frame_managed<vertex>>;
using verify_scope = box<scope_managed>;

template<class frames_type> using unificand_type = if_type<IsEqual<frames_type,array<run_frame>>,future<>,vertex>;
template<class frame_type>  static reg NativeReg(const frame_type& Frame,const char* S) {
	for(auto R:*Frame->FrameRegs.Coerce())
		if(*R.RegName==S)
			return R;
	VERSE_ERR("NativeReg: Missing ",S);
}

array<beta_frame>  GetFrames(const array<verify_scope>& Scopes) {return Scopes;}
array<beta_frame>  GetFrames(const array<beta_frame>&   Frames) {return Frames;}
array<run_frame>   GetFrames(const array<run_frame>&    Frames) {return Frames;}

symbol_map             FrameSymbols(const verify_scope& Scope) {return Scope->ScopeSymbols;}
symbol_map             FrameSymbols(const beta_frame&   Frame) {return symbol_map{};}
symbol_map             FrameSymbols(const run_frame&    Frame) {return symbol_map{};}

fx FrameAllowFx(const array<verify_scope>& Scope) {return Scope.End(0)->FrameAllowFx;}
fx FrameAllowFx(const array<beta_frame>&   Frame) {return Frame.End(0)->FrameAllowFx;}
fx FrameAllowFx(const array<run_frame>&    Frame) {return effects;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Opcodes for the evaluator.

struct op: managed {

	// Interface.
	virtual bool OnResolve(const locus& Locus,const box<op>& LocalOp) const=0;
	virtual void OnAllocate(const array<verify_scope>& Scopes) const {}

	// Verifier and runtime.
	struct program;
	struct atom;
	struct unify;
	struct fail;
	struct span;
	struct length;
	struct lambda;
	struct sequence;
	struct choice;
	struct range;
	struct call;
	struct enter;
	struct hold;
	struct stage;
	struct assume;
	struct check;
	struct iterate;
	struct scope;
	struct beta;

	// Verifier only.
	struct exists;
	struct conditional;
	struct print;
	struct reduce;
	struct test;
	struct stuck;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier contexts.

struct verifier_context_managed;
struct verifier_iterate_managed;
expose_mutable ExposeUnique(const verifier_context_managed&);
using verifier_context = box<verifier_context_managed>;
using verifier_iterate = box<verifier_iterate_managed>;
template<class unificand> constexpr auto Runs = IsEqual<unificand,future<>>;
fx DefaultGetSuspendedFx(fx NewChildFx);
static void RefineSuspensions(const context& C,bool& Narrowed,bool RefineSuspended);

struct verifier_context_state {
	bool              FlexibleStart;
	fx                CompletedChildFx;
	fx                AbstractingFx, CompletedChildAbstractingFx;
	function<fx()>    GetAssumedFx;
	function<fx(fx)>  GetSuspendedFx;
	array<beta_frame> ContextFrames;
	verifier_context_state(bool FlexibleStart0,const function<fx()>& GetAssumedFx0,const function<fx(fx)>& GetSuspendedFx0):
		FlexibleStart(FlexibleStart0), CompletedChildFx(effects),
		AbstractingFx(only_succeeds), CompletedChildAbstractingFx(effects),
		GetAssumedFx(GetAssumedFx0), GetSuspendedFx(GetSuspendedFx0) {}
	virtual void EnsureVirtual() {}
	error OnDescribeSuspensionHelper(const suspension& Sus,bool& Recurse) const;
};
fx DefaultGetSuspendedFx(fx NewChildFx) {
	auto VCS=Coerce<box<verifier_context_state>>(Thread);
	return VCS->FlexibleStart? NewChildFx&no_unifies: NewChildFx;//!!need strength-unifies
}

// Verifier context.
struct verifier_context_managed: context_managed, verifier_context_state {
	function<error(const verifier_context&)> Describe;
	template<class describe> verifier_context_managed(const describe& Describe0,fx AllowFx0,bool FlexibleStart0,const function<fx()>& GetAssumedFx0,const function<fx(fx)>& OnRefine0):
		context_managed(true,true,AllowFx0,no_effects,Thread->Depth+1),
		verifier_context_state(FlexibleStart0,GetAssumedFx0,OnRefine0),
		Describe(Describe0) {}
	error OnDescribe() const override {
		return Describe(*this);
	}
	error OnDescribeSuspension(const suspension& Sus,bool& Recurse) const override {
		return OnDescribeSuspensionHelper(Sus,Recurse);
	}
	fx OnRefineFx(bool& Narrowed) override {
		return Run([&]{
			RefineSuspensions(*this,Narrowed,true);
			AbstractingFx = CompletedChildAbstractingFx; // GetSuspendedFx may alter it.
			auto NewFx    = GetSuspendedFx(CompletedChildFx);
			return NewFx;
		});
	}
	friend expose_mutable ExposeUnique(const verifier_context_managed&);
};

// Thread for verifier iteration.
struct verifier_iterate_state: verifier_context_state {
	const visibility IterateVisibility;
	verifier_iterate_state(const function<fx()>& GetAssumedFx0,visibility IterateVisibility0,const function<fx(fx)>& GetSuspendedFx0):
		verifier_context_state{true,GetAssumedFx0,GetSuspendedFx0},
		IterateVisibility(IterateVisibility0) {
	}
};
struct iterate_exploration {
	nat             OriginalForkCount;
	future<tuple<>> WakeDecidesFork, KillSucceedsFailsForks, KillDecidesFork;
};

verifier_iterate VerifierExploration(context C=Thread);
bool ResolveSemaphoreBatch(const future<tuple<>>& S,bool DoResolve=true);
static function<fx()> GetAssumedFxConst(fx Fx);
static void TraceVisibility(const pin<context>& C=Thread);

struct verifier_iterate_managed: iterate_managed, verifier_iterate_state {
	var<bag<equation>>                 Equations;          // Equations here.
	var<map<vertex,equate>>            LocalEquates;       // Exploration context vertex equality assumptions for flexible & inflexible vertices.
	equation_map<relation_publisher>   RelationPublishers; // Subscribed to by equation::Calls&Functions&Parameters and child verifier_iterate RelationPublishers.
	var<map<stage_site,fx>>            ContextStageFx, ContextJoinStageFx;
	nat                                ForkCount, SilenceCount;
	option<iterate_exploration>        IterateExploration;
	option<nat>                        ForkIndex;
	array<future<tuple<>>>             Stops;
	verifier_iterate_managed(const verifier_iterate_state& IterateState0,bool IsCommittable0,fx AllowFx0,nat Depth0,fx HoldFx0,const step& ParentStep0):
		iterate_managed       (False,IsCommittable0,AllowFx0,Depth0,HoldFx0,ParentStep0),
		verifier_iterate_state(IterateState0),
		Equations             (Run([&]{return var<bag<equation>>();})),
		LocalEquates          (Run([&]{return var<map<vertex,equate>>();})),
		RelationPublishers    (Run([&]{return equation_map<relation_publisher>();})),
		ContextStageFx        (Run([&]{return var<map<stage_site,fx>>();})),
		ContextJoinStageFx    (Run([&]{return var<map<stage_site,fx>>();})),
		ForkCount(0), SilenceCount(0) {}
	error OnDescribeSuspension(const suspension& Sus,bool& Recurse) const override {
		return OnDescribeSuspensionHelper(Sus,Recurse);
	}
	fx OnRefineFx(bool& Narrowed) override {
		return Run([&]{
			RefineSuspensions(*this,Narrowed,true);
			return GetSuspendedFx(CompletedChildFx);
		});
	}
	struct iterate_ready {
		bool DecidesForkSucceedsFails, DecidesKillSucceedsFails, DecidesRefork, FailsAwakeDecides;
		bool AnyReady() const {return DecidesForkSucceedsFails || DecidesKillSucceedsFails || DecidesRefork || FailsAwakeDecides;}
	};
	iterate_ready CheckIterateReady(bool DoResolve) {
		auto ExplorationContext = VerifierExploration();
		return iterate_ready {
			.DecidesForkSucceedsFails = ForkIndex==False     && CompletedChildFx<=contradicts+AllowFx && !IsCommitted && !(CompletedChildFx<=succeeds) && !(CompletedChildFx<=fails),
			.DecidesKillSucceedsFails = ForkIndex==Truth(0U) && (CompletedChildFx<=succeeds || CompletedChildFx<=fails) && ResolveSemaphoreBatch(IterateExploration->KillSucceedsFailsForks,DoResolve),
			.DecidesRefork            = ForkIndex==Truth(0U) && ExplorationContext->ForkCount!=IterateExploration->OriginalForkCount && ResolveSemaphoreBatch(IterateExploration->KillSucceedsFailsForks,DoResolve),
			.FailsAwakeDecides        = ForkIndex==Truth(2U) && (CompletedChildFx<=succeeds || CompletedChildFx<=fails) && ResolveSemaphoreBatch(IterateExploration->WakeDecidesFork,DoResolve)
		};
	}
	void RefineFx() {
		bool Narrowed;
		do {
			Narrowed=false;
			RefineSuspensions(*this,Narrowed,false);
		}
		while(Narrowed);
		TraceVisibility();
	}
	void OnRefineIterateFx() override {
		if(IterateVisibility==visibility::Iterate) {
			auto ExplorationContext = VerifierExploration();
			if(!IsCommitted)
				Run([&]{
					RefineFx();
				});

			// Rule (IterateSplitImply).
			if(VerboseSplitImply) Print("OnLeftStep: CompletedChildFx=",CompletedChildFx);
			if(auto Ready=CheckIterateReady(true); Ready.AnyReady()) {
				if(Ready.DecidesForkSucceedsFails) {
					// This iterate has newly become <decides>, so fork succeeds-fork and fails-fork in parallel.
					if(VerboseSplitImply) Print("OnLeftStep: Fork because ",CompletedChildFx);
					return StartIterateSplit(*this,1);
				}
				if(Ready.DecidesKillSucceedsFails) {
					// This decides-fork CompletedChildFx has become known, so unsilence it and kill succeeds-fork & fails-fork.
					if(VerboseSplitImply) Print("OnLeftStep: Kill succeeds-fork and fails-fork because ",only_cardinalities&CompletedChildFx,": ",ExplorationContext->SilenceCount);
					--ExplorationContext->SilenceCount;
				}
				if(Ready.DecidesRefork) {
					// This decides-fork has forked further, so restart succeeds-fork and decides-fork to maintain coherence.
					if(VerboseSplitImply) Print("OnLeftStep: Refork decides-fork");
					ExplorationContext->SilenceCount--;
					return StartIterateSplit(*this,0); // Sets new ForkCount.
				}
				if(Ready.FailsAwakeDecides) {
					// This fails-fork is now known, so awake decides-fork to monitor unassumed CompletedChildFx.
					if(VerboseSplitImply) Print("OnLeftStep: Wake decides-fork because ",CompletedChildFx);
				}
			}
			if(ForkIndex) // Makes no sense for fork 0!!
				LocalPendingFx &= ForkIndex.Coerce()<2? succeeds: fails;
		}
	}
	static void StartIterateSplit(const iterate& Self0,bool DecidesForkCountDelta) {
		// Initiate the rule (IterateSplitImply).
		// Fork to consider success and failure cases in parallel, ensuring confluence and monotonicity.
		auto Self                = Self0.Coerce<verifier_iterate_managed>();
		auto ExplorationContext  = VerifierExploration();
		Self->IterateExploration = Truth(ExplorationContext->Context()->Run([&]{
			// Allocate semaphores in parent of exploration context.
			return iterate_exploration{ExplorationContext->ForkCount+DecidesForkCountDelta};
		}));
		Verse::SuspendStep([=](const step& Step0)->current_step {
			return ForForkStep(locus{},False,0,3,Step0,[=](nat i,const step& Step1)->current_step {
				Self->ForkIndex    = Truth(i);
				if(i==1)
					Self->GetAssumedFx = GetAssumedFxConst(succeeds);
				if(i==0) {
					// We initially succeeds-suspend this decides-fork by wrapping its ExplorationContext context in a WhenResolveStep.
					if(VerboseSplitImply) Print("StartIterateSplit: Sleep decides-fork");
					ExplorationContext->SilenceCount += 1;
					ExplorationContext->ForkCount    += DecidesForkCountDelta;
					auto SavedContext                 = Thread;
					Thread                            = ExplorationContext; // This is sound because we're stepping.
					auto Step2                        = Internal::LeaveIterate()->ParentStep;
					return WhenResolveStep(VERIFIER_HERE("StartIterateSplit (IterateSplitImply): Wake decides-fork"),only_succeeds,Self->IterateExploration->WakeDecidesFork,Step2,[=](tuple<>,const step& Step3)->current_step {
						ExplorationContext->ParentStep = Step3;
						if(VerboseSplitImply) Print("StartIterateSplit: Woke decides-fork ",ExplorationContext->SilenceCount);
						return EnterRecursiveStep(SavedContext,[=]()->current_step {
							return AddStopStep(Self->IterateExploration->KillDecidesFork,"decides-fork",Step1);
						});
					});
				}
				else {
					// Our succeeds-fork and fails-fork are initially running, but stoppable.
					return AddStopStep(Self->IterateExploration->KillSucceedsFailsForks,i==1? "succeeds-fork": "fails-fork",Step1);
				}
			});
		})->ReadySuspensionBatch();
	}
	void OnCommit() override;
	void OnAbandon() override;
	equate FreshEquate(const vertex& U,const equation& E) {
		VERSE_ENSURE(!LocalEquates.Has(U));
		return LocalEquates.GetInit(U,E,U);
	}
	friend verifier_iterate VerifierExploration(context C) {
		for(;;) {
			if(auto VI=C.Cast<verifier_iterate_managed>(); VI && VI->IterateVisibility!=visibility::Iterate)
				return VI.Coerce();
			C=C.Context();
		}
	}
	bool Stopped() override {
		for(auto Stop:Stops)
			if(Stop.Cast())
				return true;
		return false;
	}
	friend bool ResolveSemaphoreBatch(const future<tuple<>>& S,bool DoResolve) {
		bool IsResolved=IsA<tuple<>>(S);
		if(!IsResolved && DoResolve)
			S.ResolveBatch(False,false);
		return !IsResolved;
	}
	static current_step AddStopStep(const future<tuple<>>& Stop,const char* What,const step& Step0) {
		auto ExplorationContext  = VerifierExploration();
		VERSE_ENSURE(Stop.Context()==ExplorationContext.Context());
		ExplorationContext->Stops += array{Stop};
		if(IsA<tuple<>>(Stop)) { // Stop is resolved now, so reconsider ExplorationContext.
			if(VerboseSplitImply) Print("AddStopStep: Stop immediate ",What);
			return ExplorationContext->ReadySuspensionBatch(), ResumeStep(Step0);
		}
		ExplorationContext->Run([&]{ // Create a dependency to ensure fork is readied when Stop resolved.
			WhenResolve(VERIFIER_HERE("AddStopStep (IterateSplitImply): AddStop"),only_succeeds,Stop,[=](tuple<>){VERSE_UNEXPECTED;});
		});
		return Step0;
	}
	option<step> EnterContext(const char* What) override {
		if(Stopped()) {
			if(VerboseSplitImply) Print("EnterContext: Stop Enter ",What);
			return Truth(ParentStep);
		}
		VERSE_ENSURE(!context_managed::EnterContext(What));
		return False;
	}
	friend expose_mutable ExposeUnique(const verifier_iterate_managed&);
};

template<class unificand,class describe> auto MakeContext(const describe& Describe,fx AllowFx,bool FlexibleStart,const function<fx()>& GetAssumedFx,const function<fx(fx)>& GetSuspendedFx) {
	if constexpr(Runs<unificand>)
		return context(true,true,AllowFx,no_effects,Thread->Depth+1);
	else
		return verifier_context(Describe,AllowFx,FlexibleStart,GetAssumedFx,GetSuspendedFx);
}
template<class frames_type> frames_type SetContextFrames(const frames_type& Scopes) {
	if constexpr(!IsEqual<frames_type,array<run_frame>>)
		Coerce<box<verifier_context_state>>(Thread)->ContextFrames=Scopes;
	return Scopes;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Context relationships.

// Visibility context as verifier_iterate.
verifier_iterate IterateStart(const context& C=Thread) {
	return C->Visibility().Coerce<verifier_iterate>();
}
bool IsVerify(pin<context> C=Thread) {
	auto VS=Coerce<box<verifier_context_state>>(C);
	return VS->ContextFrames.End(0).IsA<verify_scope>();
}

// Implying and effects.
static fx AssumedFxHere(const context& C) {
	return Coerce<box<verifier_context_state>>(C)->GetAssumedFx();
}
static bool IsImplying(const context& C=Thread) {
	auto VS=Cast<box<verifier_context_state>>(C);
	return VS && VS->GetAssumedFx()<=abstracts;
}
static fx StageFxHere(context C,stage_site_future StageSite) {
	for(;;) {
		if(auto VI=IterateStart(C); auto Fx=VI->ContextStageFx.Get(CoerceStageSite(StageSite)))
			return Fx.Coerce();
		else if(VI->IterateVisibility==visibility::VerifyTop)
			return abstracts;
		else
			C=VI->Context();
	}
}

// Effects inference helpers.
static function<fx()> GetAssumedFxHere(const context& C=Thread) {
	return [=]{
		return AssumedFxHere(C);
	};
}
static function<fx()> GetAssumedFxConst(fx Fx) {
	return [=]() {
		return Fx;
	};
}

// Flexibility. Returns outermost d where we infer c flexes d.
static box<verifier_context_state> FlexibleStart(pin<context> C=Thread) {
	for(;;) {
		if(auto VS=Coerce<box<verifier_context_state>>(C); VS->FlexibleStart)
			return VS;
		C=C.Context();
	}
}
current_step RunParentStep(const step& Step0,const function<current_step(const step&)>& F) {
	auto Parent = Thread->Context();
	auto Child  = Thread;
	Thread      = Parent;
	return F([=]()->current_step {
		if(auto Step1=Child->EnterContext("RunComposeStep")) // Doesn't set ParentStep when returning to child here.
			return Step1.Coerce();
		return ResumeStep(Step0);
	});
}
current_step RunFarStep(const context& FarContext,const step& Step0,const function<current_step(const step&)>& F) {
	if(Thread->Depth>FarContext->Depth-1)
		return RunParentStep(Step0,[=](const step& Step1)->current_step {
			return RunFarStep(FarContext,Step1,F);
		});
	if(Thread->Depth==FarContext->Depth-1)
		return FarContext->RunStep(Step0,F);
	VERSE_ERR("RunComposeStep");
}
current_step EnterComposeStep(const context& FarContext,const step& Step0,const function<current_step(const step&)>& F) {
	if(!Cast<verifier_context_state>(FarContext) || FlexibleStart(FarContext)==FlexibleStart(Thread))
		return F(Step0);
#if 0
	auto Sus=FarContext->Run([&]{
		return SuspendStep(F);
	});
	return Sus->ReadySuspensionStep(Step0);
#else
	if(Thread->Depth>FarContext->Depth-1)
		return RunParentStep(Step0,[=](const step& Step1)->current_step {
			return EnterComposeStep(FarContext,Step1,F);
		});
	if(Thread->Depth==FarContext->Depth-1) // Problem: May leave multiple suspensions.
		return FarContext->RunStep(Step0,F);
#endif
	VERSE_ERR("EnterComposeStep");
}

// Natives.
template<class frame_type> static nat NativeIndex(const frame_type& Frame,const char* S) {
	return NativeReg(Frame,S).RegBase.Coerce();
}
vertex GetVerifierNative(const char* S) {
	auto NativeFrame = Coerce<box<verifier_context_state>>(Thread)->ContextFrames[0];
	return NativeFrame->FrameUnificands.Elements[NativeIndex(NativeFrame,S)];
};

// Arrays in the verifier.
vertex::vertex(): Value(box<variable_vertex>()) {}
vertex::vertex(const tuple<>&): vertex(GetVerifierNative("false")) {}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Vertex operations.

option<comparable> vertex::CastHead() const {
	if(auto V=Value.Cast<vertex_managed>(); !V || V->VertexHead)
		return Truth(Value);
	else
		return False;
}
shape vertex::GetShape() const {
	if(auto V=Value.Cast<vertex_managed>())
		return V->VertexShape;
	else
		return shape::FromAtom(Value);
}
option<comparable> vertex::CastAtom() const {
	return !Value.IsA<vertex_managed>()? Truth(Value): False;
}
static option<box<vertex_managed>> CastFlexible(const vertex& V,const context& C=Thread) {
	if(auto W=V.CastVertex<vertex_managed>())
		if(FlexibleStart(C)==FlexibleStart(W->Context()))
			return W;
	return False;
}
void vertex::SetVertexName(const var<string>& S) const {
	if(auto F=Value.Cast<vertex_managed>())
		static_cast<future<>&>(F->VertexName) = S;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Calls.

fx BetaFx(const box<call_vertex>& CallVertex) {
	if(auto CallerVertex=CallVertex->CallerVertex) {
		fx Fx = CallerVertex->CallProductFx;
		for(auto[C,_]:CallerVertex->CallBetaContexts)
			Fx &= C->SuspendedFx;
		return Fx;
	}
	return only_succeeds;
}
const char* CallMatches(const box<call_vertex>& A,const box<call_vertex>& B) {
	if(A->CallParameterVertex==B->CallParameterVertex) // Per (SameCallEqual), u=v implies u(p)=v(p), even if non-<computes> call to overloads.
		return "(SameCallEqual)";
	else if(BetaFx(A)<=computes || BetaFx(B)<=computes) // Per (ComputesCallEqual), these are <computes> calls where u=v and p=q implies u(p)=v(q)
		return "(ComputesCallEqual)";
	else
		return nullptr;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Vertex information equate to a visibility context.

// Maintain commutative and reflexive strength with which vertices are unified.
strength EquatedStrength(const pin<equate>& L0,const pin<equate>& L1,strength ResultStrength=strength_succeeds) {
	if(L0==L1)
		return ResultStrength;
	auto MaxStrength  = L0->NextStrength&L1->NextStrength;
	auto NextStrength = ResultStrength+MaxStrength;
	if(strength_decides<NextStrength)
		return strength_resolves;
	return EquatedStrength(
		L0->NextStrength<=MaxStrength? pin(L0->NextRep.Coerce()): L0,
		L1->NextStrength<=MaxStrength? pin(L1->NextRep.Coerce()): L1,
		NextStrength
	);
}
equation FindEquation(const vertex& Source,verifier_iterate Visibility=IterateStart());
void PrintEquated(const char* Rule,const pin<equate>& InL0,const pin<equate>& InL1,strength Strength);
equate SetEquatedStrength(const char* Rule,bool& Narrowed,const pin<equate>& InL0,const pin<equate>& InL1,strength Strength);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier equation support.

// Parent visibility. Never skips visibility contexts.
static option<verifier_iterate> GetParentVisibility(const context& C) {
	auto Visibility = Thread->Visibility();
	if(Visibility==C->Visibility())
		return False;
	return Visibility.Context()->Visibility().Cast<verifier_iterate>();
}

// Callbacks.
struct on_vertex: managed {
	virtual void OnVertex(const vertex& A) {}
};
expose_mutable ExposeUnique(const on_vertex&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier inference.

using call_set=var<bag<box<call_vertex>>>;
struct inferences {
	var<bag<tuple<context,vertex,vertex>>> PendingEquates;
	~inferences();
	void EquateVertices(const vertex& U0,const vertex& U1) const;
	void Merge(bool ContextLocal,const call_set& TargetRS,const call_set& SourceCalls,bool AllowDisposedSource) const;
	void Merge(bool ContextLocal,const equation_map<call_set>& TargetCallsMap,const equation_map<call_set>& SourceCallsMap,bool AllowDisposedSource) const;
	void Merge(bool ContextLocal,const equation_map<equation_map<call_set>>& TargetCallsMapMap,const equation_map<equation_map<call_set>>& SourceCallsMapMap,bool AllowDisposedSource) const;
	void CheckEqProduct(const equation& TargetFE,const equation& SourceFE) const;
	void Infer();
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Equations.

equate Equate(const vertex& U);
struct equation_managed: relation_subscriber_managed {

	// Persistent.
	var<map<vertex,equate>>                Vertices;
	var<bag<box<on_vertex>>>               LocalVertexSubscribers;
	var<bag<box<array_vertex>>>            EquationArrays;
	equation_map<call_set>                 Parameters, Functions;
	equation_map<equation_map<call_set>>   Calls;
	option<equate>                         ComparableEquate;
	strength                               TrivialStrength;

	// Transient during data flow analysis:
	bool                                   Trace;
	fx                                     EquationHeadFx, EquationDeepFx;

	// Persistent things.
	explicit equation_managed(const vertex& U): // Called only by Equate(v) only when no local equation exists.
		TrivialStrength(strength_resolves), Trace(false), EquationHeadFx(abstracts), EquationDeepFx(abstracts) {
		auto Visibility = IterateStart();
		VERSE_ENSURE(Context()->Visibility()==Visibility);
		if(Visibility->Equations.Set(*this); auto ParentVisibility=GetParentVisibility(U.Context())) {
			inferences Inferences; // Infers nothing new.
			EquateNow(Inferences,FindEquation(U,ParentVisibility.Coerce()));
		}
		else AddVertex(Visibility,Visibility->FreshEquate(U,*this));
	}
	strength EquatedStrength(const vertex& A,const vertex& B) const {
		if(TrivialStrength==strength_succeeds) // Optimization.
			return strength_succeeds;
		else
			return ::EquatedStrength(Vertices[A],Vertices[B]);
	}
	fx EquatedFx(const vertex& A,const vertex& B) const {
		if(VERSE_ENSURE(!A.GetShape().IsFail()); B.GetShape().IsFail())
			return EquationDeepFx; // The (FailIntro) case fx{u=fail}@c.
		return
			EquationDeepFx &       // The (UnifyDeepFails) and (ProductCardinalities) portion of fx{u=v}@c.
			EquatedStrength(A,B);  // The (DataFlow) portion of fx{u=v}@c.
	}
	bool EquatedComparable(const vertex& A) {
		return ComparableEquate && EquatedStrength(ComparableEquate->EquateVertex,A)<=strength_decides;
	}
	void SetEquatedComparable(bool& Narrowed,const equate& Equate) {
		if(ComparableEquate)
			SetEquatedStrength(nullptr,Narrowed,Equate,ComparableEquate.Coerce(),strength_decides);
		else
			ComparableEquate=Truth(Equate);
	}
	void AddVertex(const verifier_iterate& Visibility,const equate& Equate) {
		auto U=Equate->EquateVertex;
		VERSE_ENSURE(!Vertices.Set(U,Equate));
		for(auto[Sub,__]:LocalVertexSubscribers)
			Sub.Context()->Run([&,Sub=Sub]{return Sub->OnVertex(U);});
		if(auto ASO=U.CastVertex<array_vertex>())
			if(auto N=Length(ASO->VertexElements); N>0)
				VERSE_ENSURE(!EquationArrays.Set(ASO.Coerce()));
	}
	void EquateNow(const inferences& Inferences,const equation& SourceEquation) {
		auto Visibility = IterateStart();
		VERSE_ENSURE(IsLive && SourceEquation->IsLive && Context()->Visibility()==Visibility && SourceEquation!=box(*this));

		// TrivialStrength must be recalculated after equations are merged together.
		TrivialStrength = strength_resolves;

		// Merge equations in child visibility contexts first.
		if(Vertices.ReadCount()>0)
			Visibility->RelationPublishers->OnEquate(Inferences,*this,SourceEquation);

		auto SourceParentVisibility = GetParentVisibility(SourceEquation.Context());
		if(SourceParentVisibility) {
			// Copy ancestor SourceEquation into this one and subscribe to changes.
			for(auto[U,_]:SourceEquation->Vertices)
				AddVertex(Visibility,Visibility->FreshEquate(U,*this));
			SourceParentVisibility->Run([&]{
				SourceEquation->SubscribeToEquationHere(*this);
			});
		}
		else {
			// Merge local SourceEquation into this one.
			for(auto[Sub,_]:SourceEquation->LocalVertexSubscribers)
				for(auto[U,UL]:Vertices)
					Sub.Context()->Run([&,Sub=Sub,U=U]{return Sub->OnVertex(U);});
			for(auto[U,UL]:SourceEquation->Vertices)
				UL->Equation=*this,
				AddVertex(Visibility,UL.ReadValue());
			for(auto[Sus,_]:SourceEquation->LocalVertexSubscribers)
				LocalVertexSubscribers.Set(Sus);
			for(auto[Publisher,_]:SourceEquation->Publishers) // Move SourceEquation's parent equation subscriptions to *this.
				SubscribeTo(Publisher);
			Trace = Trace || SourceEquation->Trace;
			VERSE_ENSURE(Visibility->Equations.Remove(SourceEquation));
			SourceEquation->Dispose(); // Actually unsubscribes.
		}

		// Perform all substitutions at and below Visibility.
		Visibility->RelationPublishers->OnSubstitute(Inferences,*this,SourceEquation,false);

		// Merge relations and discover further inferences.
		Inferences.Merge(!SourceParentVisibility,Parameters,SourceEquation->Parameters,false);
		Inferences.Merge(!SourceParentVisibility,Functions ,SourceEquation->Functions ,false);
		Inferences.Merge(!SourceParentVisibility,Calls     ,SourceEquation->Calls     ,false);
		VERSE_ENSURE(IsLive);
	}
	static void EquateVerticesNow(const inferences& Inferences,const vertex& U0,const vertex& U1) {
		if(auto E0=FindEquation(U0), E1=FindEquation(U1); E0!=E1) {
			auto Visibility = Thread->Visibility();
			bool L0         = E0.Context()->Visibility()==Visibility;
			bool L1         = E1.Context()->Visibility()==Visibility;
			if(L0>L1 || L0==L1 && E0->Vertices.ReadCount()>=E1->Vertices.ReadCount())
				Equate(U0)->Equation->EquateNow(Inferences,E1);
			else
				Equate(U1)->Equation->EquateNow(Inferences,E0);
		}
	}
	void OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation) override {
		// This equation is being told that parent (or local after commit) SourceEquation is merging with parent TargetEquation.
		VERSE_ENSURE(IsLive && SourceEquation->IsLive && SourceEquation.Context()->Visibility()!=Context()->Visibility() && TargetEquation.Context()->Visibility()!=Context()->Visibility() && TargetEquation->Vertices.ReadCount() && Context()==Thread);
		auto U0=TargetEquation->Vertices.begin().Key();
		auto U1=SourceEquation->Vertices.begin().Key();
		VERSE_ENSURE(FindEquation(U0)==box(*this) || FindEquation(U1)==box(*this));
		EquateVerticesNow(Inferences,U0,U1);
	}
	void OnSubstitute(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation,bool AllowDisposedSource) override {
		VERSE_ENSURE(IsLive && TargetEquation->IsLive && SourceEquation.Context()->Visibility()!=Context()->Visibility() && TargetEquation.Context()->Visibility()!=Context()->Visibility());
		GetParentVisibility(TargetEquation.Context())->Run([&]{
			// We were subscribed to SourceEquation in parent visibility context.
			// Caller parent visibility already removed *this equation from RelationPublishers.
			// Now we subscribe to TargetEquation in parent visibility.
			TargetEquation->SubscribeToEquationHere(*this);
		});
	}

	// Recalculation.
	void PropagateEquality(bool& Narrowed) {
		auto Visibility=IterateStart();

		// Inference rule (UnifyIntro): assume unifications have the strength implied by their context.
		auto GlobalComparableVertex = GetVerifierNative("comparable");
		for(auto[U,UL]:Vertices) {
			if(auto UM=U.CastVertex<vertex_managed>())
				for(auto[V,_]:UM->VertexPredecessors)
					SetEquatedStrength("(UnifyIntro)",Narrowed,Vertices[U],Vertices[V],ambiguates+(resolves&AssumedFxHere(U.Context())));
			if(U.GetShape().ShapeComparable && (U.CastAtom() || AssumedFxHere(U.Context())<=resolves)) //!! Later want vertex abstracts.
				SetEquatedComparable(Narrowed,UL.ReadValue()); // To fix, need vertex abstracts.
			if(auto UC=U.CastVertex<call_vertex>())
				if(auto FE=FindEquation(UC->CallFunctionVertex); FE->Vertices.Has(GlobalComparableVertex))
					if(AssumedFxHere(UC->Context())<=resolves)
						SetEquatedComparable(Narrowed,UL.ReadValue());
		}
		if(TrivialStrength==strength_succeeds) // After SetEquatedComparable logic.
			return;

		// Inference rules (SameCallEqual) and (ComputesCallEqual) on vertices.
		for(auto[ParameterEquation,FunctionRSM]:Calls->Map)
			for(auto[FunctionEquation,ParameterFunctionCalls]:FunctionRSM->Map)
				for(auto[Call0,_]:ParameterFunctionCalls.ReadValue())
					if(!Call0.IsA<callee_beta_vertex>())
						for(auto[Call1,__]:ParameterFunctionCalls.ReadValue())
							if(Call1==Call0)
								break;
							else if(!Call1.IsA<callee_beta_vertex>())
								if(auto Rule=CallMatches(Call0,Call1))
									SetEquatedStrength(Rule,Narrowed,Vertices[Call0],Vertices[Call1],
										FunctionEquation ->EquatedStrength(Call0->CallFunctionVertex ,Call1->CallFunctionVertex )+
										ParameterEquation->EquatedStrength(Call0->CallParameterVertex,Call1->CallParameterVertex));

		// Inference rule (ProductEqual): Two tuples are as equal as the weakest equality among their elements.
		// Inference rule (ProductComparable): A tuple is comparable of all of its elements are comparable.
		for(auto[AS0,_]:EquationArrays) {
			auto n=Length(AS0->VertexElements);
			bool ArrayComparable=true;
			if(!EquatedComparable(AS0)) {
				for(nat i=0; i<n && ArrayComparable; i++)
					if(auto U=AS0->VertexElements[i]; true)
						ArrayComparable=FindEquation(U)->EquatedComparable(U);
				if(ArrayComparable)
					SetEquatedComparable(Narrowed,Vertices[AS0]);
			}
			for(auto[AS1,__]:EquationArrays) {
				if(AS1==AS0)
					break;
				else if(Length(AS1->VertexElements)==n) {
					auto Strength=strength_succeeds;
					for(auto i=0; i<n; i++) {
						auto A0 = AS0->VertexElements[i];
						auto A1 = AS1->VertexElements[i];
						auto S  = FindEquation(A0)->EquatedStrength(A0,A1);
						if(S>Strength)
							if(Strength=S; S>strength_decides)
								break;
					}
					SetEquatedStrength("(ProductEqual)",Narrowed,Vertices[AS0],Vertices[AS1],Strength);
				}
			}
		}

		//if(Trace) Print("PropagateEquality ",box(*this));
	}
	void PropagateEquationFx(bool& Narrowed) {
		VERSE_ENSURE(IsLive);

		// Calculate EquationHeadFx for inference rule (EqualsFromVertex).
		auto Fx = abstracts;
		shape EquationShape;
		var<map<equate,shape>> SucceedsRepShapes;
		for(auto[U,UL]:Vertices) {
			auto UShape   = U.GetShape();
			auto UR       = UL->Rep(strength_succeeds);
			EquationShape = Intersection(EquationShape,UShape);
			if(!UShape.IsAny())
				if(auto UC=UL->EquateVertex.Context(); !UShape.IsFail() || AssumedFxHere(UC)<=succeeds)
					SucceedsRepShapes.Set(UR,Intersection(SucceedsRepShapes.GetInit(UR),UShape));
			if(auto Flexible=U.CastVertex<vertex_managed>())
				for(auto[FV,_]:Flexible->VertexHeadFailSources)
					if(auto SourceFx=FindEquation(FV)->EquationHeadFx; SourceFx<=fails)
						Fx &= SourceFx;
		}
		if(EquationShape.IsFail()) // Rule (HeadEqualFails) for fails.
			Fx&=fails;
		for(auto[VR,ShapeCursor]:SucceedsRepShapes) // Rule (HeadEqualFails) for contradicts; needed for test(S00){x:=2=3 => x=4}.
			if(ShapeCursor.ReadValue().IsFail()) // Works because we exclude vertices that fail and aren't assumed succeeds.
				Fx&=contradicts;
		if(!(Fx<=resolves))
			for(auto[_,UL]:Vertices)
				if(
					UL->EquateVertex.CastAtom() || // Rule (AtomIntro).
					UL->EquateVertex.CastVertex<abstraction_vertex>() && !UL->EquateVertex.CastVertex<array_vertex>() || // Hack to be fixed with proper stage support on converges&recurses functions.
					AssumedFxHere(UL->EquateVertex.Context())<=resolves // Rule (UnifyIntro).
					)
					Fx&=resolves;
		VERSE_ENSURE(Fx<=EquationHeadFx);
		if(Fx!=EquationHeadFx)
			Narrowed=true, EquationHeadFx=Fx;

		// Calculate EquationDeepFx used only by unification for inference rules (UnifyDeepFails), (ProductCardinalities).
		if(EquationArrays.ReadCount()) {
			auto P = succeeds;
			for(auto[PE,FunctionCalls]:Functions->Map)
				for(auto[UC,_]:FunctionCalls.ReadValue())
					P = ProductFx(P,FindEquation(UC)->EquationDeepFx);
			Fx &= P<=fails? P: P+resolves; // Equation may (HeadEqualFails) even if product elements succeed.
		}
		//VERSE_ENSURE(Fx<=EquationDeepFx);
		if((Fx&EquationDeepFx)!=EquationDeepFx)
			Narrowed=true, EquationDeepFx&=Fx;
	}
	void ResetDominators(strength Strength) {
		// This is a reset process here (versus a monotonic accumulation in the spec) because we store
		// dominator strength-representatives (instead of all dominators) as an optimization.
		//if(Trace) Print();
		//if(Trace) Print("ResetDominators ",Strength,": ",*this);
		auto Visibility=IterateStart();

		// Init Vertices equates.
		for(auto[U,UL]:Vertices) {
			auto UR=UL->Rep(Strength);
			UL->SucceedsPredecessors          = False;
			UL->SucceedsPredecessorDominators = False;
			UL->Dominators					  = False;
			UL->Dominators.Set(UR); // Inference rules (SelfDominator), (EqualDominator).
			if(Strength==strength_succeeds) {
				UL->FlowShape                 = UR->SucceedsRepShape;
				UL->FlowShape.ShapeComparable = EquatedComparable(U); // Don't trust stored ShapeComparable, as it may be abstracts.
			}
		}
		for(auto[U,UL]:Vertices)
			if(auto UR=UL->Rep(strength_succeeds); auto UM=U.CastVertex<vertex_managed>())
				for(auto[W,_]:UM->VertexPredecessors)
					if(auto WL=Vertices[W],WR=WL->Rep(strength_succeeds); WR!=UR)
						UL->SucceedsPredecessors.Set(WL,WR)/*,
						UL->SucceedsPredecessorDominators.Set(WL->Dominators,tuple{Truth(WR),true})*/;

		// Fill in MatchingCallReps supporting (CallComputesEqual).
		for(auto[U,UL]:Vertices)
			UL->MatchingCallReps=False;
		for(auto[U,UL]:Vertices) {
			auto UR=UL->Rep(Strength);
			UR->MatchingCallReps.Set(UR);
		}
		if(Strength==strength_succeeds)
			for(auto[PE,CallsM]:Calls->Map)
				for(auto[FE,Calls1]:CallsM->Map)
					for(auto[UC,__]:Calls1.ReadValue())
						for(auto[VC,___]:Calls1.ReadValue())
							if(VC==UC)
								break;
							//else if(UC.IsA<call_vertex>() && VC.IsA<call_vertex>() && CallMatches(UC,VC)) {
							else if(UC.IsA<callee_beta_vertex>() && VC.IsA<callee_beta_vertex>() && CallMatches(UC,VC)) {
								auto UL=Vertices[UC], UR=UL->Rep(Strength);
								auto VL=Vertices[VC], VR=VL->Rep(Strength);
								if(UR!=VR) {
									if(FE->EquatedStrength(UC->CallFunctionVertex,VC->CallFunctionVertex)<=Strength &&
									   PE->EquatedStrength(UC->CallParameterVertex,VC->CallParameterVertex)<=Strength) {
										//Print("CM ",CallMatches(UC,VC)); // Applying to nested :int.
										UR->MatchingCallReps.Set(VR);
										VR->MatchingCallReps.Set(UR);

										//should not depend on UR!=VR or equated strength
										//but it's toxic to do this inside assumed enter because it will be succeeds-uncontested
										//so these should be weak - flow only - never equate
										//UL->SucceedsPredecessors.Set(VL,VR);
										//VL->SucceedsPredecessors.Set(UL,UR);
									}
									//else Print("!! ",FE->EquatedStrength(UC->CallFunctionVertex,VC->CallFunctionVertex)," :: ",PE->EquatedStrength(UC->CallParameterVertex,VC->CallParameterVertex)," :: ",FE," :: ",PE);
								}
							}
		for(auto[U,UL]:Vertices)
			for(auto[VL,VR]:UL->SucceedsPredecessors)
				UL->SucceedsPredecessorDominators.Set(VL->Dominators,tuple{Truth(VR.ReadValue()),true});
	}
	void ExpandDominators(bool& Narrowed,strength Strength) {
		if(strength_succeeds>=TrivialStrength)
			return; // If we have decides-equates, other equations still need our SucceedsPredecessor*, which are evolving.
		//if(Trace) Print();
		//if(Trace) Print("ResetDominators ",Strength,": ",*this);
		auto Visibility=IterateStart();

		// Data Flow Analysis Stage 1, inference rules (*Reach): Expand element SucceedsPredecessor[Dominator]s with
		// first-reached boundary elements and dead ends.
		if(ANY_FUNCTION_FLOW || NEW_FUNCTION_FLOW || EquationArrays.ReadCount()) {
			for(auto[PE,FunctionCalls]:Functions->Map) {
				for(auto[UC,_]:FunctionCalls.ReadValue()) if(Visibility->LocalEquates.Has(UC)) {
					auto UCL=Visibility->LocalEquates[UC];
					auto USL=Vertices[UC->CallFunctionVertex];
					var<bag<equate>> InteriorVectorPriors;
					InteriorVectorPriors.Set(USL);
					for(auto[WSL,__]:InteriorVectorPriors)
						for(auto[XSL,___]:WSL->SucceedsPredecessors) // We don't need no_unifies logic here as it's below in stage 2.
							for(InteriorVectorPriors.Set(XSL); auto[YC,____]:FunctionCalls.ReadValue())
								if(YC!=UC) if(YC->CallFunctionVertex==XSL->EquateVertex && CallMatches(YC,UC)) {
									auto YL=Visibility->LocalEquates[YC], YR=YL->Rep(strength_succeeds);
									if(!UCL->SucceedsPredecessors.Set(YL,YR))
										Narrowed=true;
									if(!UCL->SucceedsPredecessorDominators.Set(YL->Dominators,tuple{Truth(YR),false}))
										Narrowed=true;
								}
					InteriorVectorPriors.Remove(USL);
					bool Change;
					do {
						Change=false;
						for(auto[WSL,WSLCursor]:InteriorVectorPriors) // Remove non-dead-ends.
							for(auto[XSL,__]:WSL->SucceedsPredecessors)
								if(!InteriorVectorPriors.Has(XSL))
									Change=true, WSLCursor.Remove();
					} while(Change);
					for(auto[WSL,__]:InteriorVectorPriors) {
						var<bag<equate>> DeadEndDominators;
						//bool DirectFunctionCall=false;
						for(auto[XC,__]:FunctionCalls.ReadValue()) {
							auto XCL=Visibility->LocalEquates[XC];
							if(auto XSL=Vertices[XC->CallFunctionVertex]; XSL->Rep(Strength)==WSL->Rep(Strength))
								DeadEndDominators.Set(Visibility->LocalEquates[XC]->Rep(Strength));
							// For function calls where L(p)<->f.L(p) but we don't have succeeds{L->f.L}, avoid dead end.
							//if(UCL->SucceedsPredecessors.Has(XCL))
							//	Print("YO"), DirectFunctionCall=true;
						}
						//if(!DirectFunctionCall)
						UCL->SucceedsPredecessorDominators.Set(DeadEndDominators,tuple{False,false});//!!Narrowed
					}
				}
			}
		}

		// Data Flow Analysis Stage 2: Find Strength dominator candidates.
		for(auto[U,UL]:Vertices) {
			auto UR=UL->Rep(Strength);

			// Succeeds-flow shape ignoring <unifies>. Strength-limiting is fine because we use SucceedsRepShape.
			if(Strength==strength_succeeds) {
				for(auto[WL,WRC]:UL->SucceedsPredecessors)
					UL->FlowShape.Intersect(Narrowed,WL->FlowShape);
				if(bool Ignore; UL->FlowShape.ShapeComparable) // May change <decides> representatives, but that doesn't affect succeeds-flow.
					SetEquatedComparable(Ignore,UL.ReadValue());
			}

			// Rule (FlowDominator): fx{v>>u}@c where u@d if no_unifies{fs[d,OP]}@c and exists w. w->u and not fx{u=w}c and fx{v>>w}@c
			if(auto UM=U.CastVertex<vertex_managed>())
				if(FlexibleStart(UM->Context())->CompletedChildFx<=no_unifies)
					for(auto[WDominators,_]:UL->SucceedsPredecessorDominators) // Should be StrengthPredecessorDominators?
						for(auto[VR,__]:WDominators)
							if(!UL->Dominators.Set(VR))
								Narrowed=true;
			//if(Trace) Print("    ",UL.ReadValue()," with ",UL->Dominators," >> ",UL.ReadValue());
		}
	}
	void NarrowDominators(bool& NarrowedFlow,strength Strength) {
		if(TrivialStrength<=strength_succeeds)
			return;
		auto Visibility=IterateStart();
		//if(Trace) Print();
		//if(Trace) Print("NarrowDominators:");

		// Data Flow Analysis Stage 3: Narrow dominators solely in this equation until fixed point converges.
		for(;;) {
			bool NarrowedHere=false;

			// Rule (FlowConsistency):
			// fx{v>>u}@c requires for all w where succeeds{w->*u}:
			//     (fx{v=w}@c or fx{v>>w}@c) and fx{v<=u}@c) and
			//     (succeeds{w->u} or exists ws cs q. v=ws[q] and q=p and cs>>us and cs>>ws)
			for(auto[U,UL]:Vertices) {
				auto UShape = U.GetShape();
				auto UC     = U.CastVertex<call_vertex>();
				auto UR     = UL->Rep(Strength);
				for(auto[VR,VRC]:UL->Dominators) {
					option<bool> KeepBecauseEqualized;
					for(auto[WDominators,WRepDirect]:UL->SucceedsPredecessorDominators) {
						auto [WR,Direct]=WRepDirect.ReadValue();
						bool Keep = (WR&&WR.Coerce()==VR || VR->IsDominator(WDominators)) &&
							        (Strength!=strength_succeeds || VR->SucceedsRepShape<=UShape);
						if(VECTOR_EQUALIZERS && !Keep && !Direct) {
							if(!KeepBecauseEqualized) {
								auto USL             = Visibility->LocalEquates[UC->CallFunctionVertex];
								auto USE             = USL->Equation;
								auto PE              = FindEquation(UC->CallParameterVertex);
								KeepBecauseEqualized = Truth(false);
								for(auto[WC,_]:USE->Functions->Map[PE])
									if(auto WCL=Vertices[WC],WCR=WCL->Rep(Strength); VR==WCR)
										if(CallMatches(UC.Coerce(),WC) && PE->Vertices[UC->CallParameterVertex]->Rep(Strength)==PE->Vertices[WC->CallParameterVertex]->Rep(Strength))
											for(auto WSL=USE->Vertices[WC->CallFunctionVertex]; auto[CSR,__]:USL->Dominators)
												if(WSL->Rep(Strength)==CSR || CSR->IsDominator(WSL->Dominators))
													KeepBecauseEqualized=Truth(true);
							}
							Keep = KeepBecauseEqualized.Coerce();
						}
						if(!Keep)	
							NarrowedHere=true, VRC.Remove();
					}
				}
			}

			// Inference rule (FunctionConsistency): fx{vs>>us}@c where abstracts{us(p)}@c requires:
            //     (if SELF_DOM then fx{us[p]>>us[p]}@c else for all w->us[p], fx{us[p]>>w}) or
			//     exists ws.   fx{ws[p]>>us[p]}@c and fx{ws=vs}@c or
			//     exists ws q. fx{ws[q]>>us[p]}@c and fx{ws=vs}@c and fx{q=p}@c and (fx&computes){us(p)}@c and (fx&computes){ws(q)}@c
			for(auto[PE,FunctionCalls]:Functions->Map)
				for(auto[UC,_]:FunctionCalls.ReadValue())
					if(auto UCE=FindEquation(UC); UCE->Context()->Visibility()==Visibility) { // Only skips if out of equation because no relevant imperative CallMatches.
						auto USL=Vertices[UC->CallFunctionVertex], USR=USL->Rep(Strength), UL=Visibility->LocalEquates[UC], UR=UL->Rep(Strength);
						auto BetaFxUC=BetaFx(UC); // We achieve the p-vs-q and computes check via CallMatches below.
						for(auto[VSR,VSRC]:USL->Dominators) {
							bool Keep=false;
							if(BetaFxUC<=Strength) {
#if SELF_DOM
								if(VECTOR_EQUALIZERS && Strength==strength_succeeds && UR->IsDominator(UL->Dominators))
									Keep=true;
#else
								if(VECTOR_EQUALIZERS && Strength==strength_succeeds)
									for(Keep=true; auto[WDominators,WRepDirect]:UL->SucceedsPredecessorDominators) {
										auto[WR,__] = WRepDirect.ReadValue();
										if(!(WR==UR || UR->IsDominator(WDominators)))
											Keep=false;
									}
#endif
								for(auto[XC,__]:FunctionCalls.ReadValue())
									if(auto XSL=Vertices[XC->CallFunctionVertex],XSR=XSL->Rep(Strength); VSR==XSR)
										if(CallMatches(UC,XC))
											if(PE->Vertices[UC->CallParameterVertex]->Rep(Strength)==PE->Vertices[XC->CallParameterVertex]->Rep(Strength))
												if(auto XL=UCE->Vertices[XC],XR=XL->Rep(Strength); XR->IsDominator(UL->Dominators))
													if(BetaFx(XC)<=Strength)
														Keep=true;
							}
							if(!Keep)
								NarrowedHere=true, VSRC.Remove();
							//if(Trace && !Keep && Strength==strength_succeeds)
							//	Print(Keep? "Keep ":"Drop ",Strength,": ",VSR,">>",USL," at ",PE," because no ...>>",UL/*," leaving ",USL->Dominators*/);
						}
					}
			if(!NarrowedHere)
				return;
			NarrowedFlow=true;
		}
	}
	void DominatorElim(bool& Narrowed,strength Strength) {
		if(TrivialStrength<=Strength)
			return;

		// Inference rule (DominatorElim): Equate vertices with their dominators.
		for(auto[U,UL]:Vertices)
			for(auto[VR,_]:UL->Dominators) // Print before equate to avoid before-vs-after confusion.
				if(VerboseEquate || Trace)
					PrintEquated("(DominatorElim)",VR,UL.ReadValue(),Strength);
		for(auto[U,UL]:Vertices) 
			for(auto[VR,_]:UL->Dominators)
				::SetEquatedStrength(nullptr,Narrowed,VR,UL.ReadValue(),Strength);
		//if(Trace) Print("Equated ",Strength,": ",*this);

#if TRIVIAL_STRENGTH
		// Update greatest strength at which all vertices are equated. Purely an optimization.
		TrivialStrength=strength_succeeds;
		for(auto RS0=Vertices.begin()->Rep(strength_succeeds),RD0=RS0->Rep(strength_decides); auto[U,LC]:Vertices)
			if(auto RS1=LC->Rep(strength_succeeds); RS1!=RS0)
				if(TrivialStrength=strength_decides; RS1->Rep(strength_decides)!=RD0) {
					TrivialStrength=strength_resolves;
					break;
				}
#endif
	}
	void SubscribeToEquationHere(const relation_subscriber& RelationSubscriber) {
		auto Visibility = IterateStart();
		auto Publisher  = Visibility->RelationPublishers->Map.GetInit(*this);
		if(!RelationSubscriber->SubscribeTo(Publisher))
			if(auto ParentVisibility=GetParentVisibility(Context())) // Is this equation in visibility parent of current context?
				ParentVisibility->Run([&]{
					SubscribeToEquationHere(Visibility->RelationPublishers);
				});
	}
};
void PrintEquated(const char* Rule,const pin<equate>& InL0,const pin<equate>& InL1,strength Strength) {
	if(Rule && InL0->Rep(Strength)!=InL1->Rep(Strength)) {
		Print(Rule," ",only_cardinalities&resolves&Strength,"{",InL0,"=",InL1,"}: ",InL0->Equation);
	}
}
equate SetEquatedStrength(const char* Rule,bool& Narrowed,const pin<equate>& InL0,const pin<equate>& InL1,strength Strength) {
	EnsureStrengthValid(Strength);
	if(strength_resolves<=Strength)
		return InL0;
	auto L0 = InL0->Rep(Strength);
	auto L1 = InL1->Rep(Strength);
	if(L0!=L1) {
		if(VerboseEquate)
			PrintEquated(Rule,InL0,InL1,Strength);
		strength NextStrength = L0->NextStrength & L1->NextStrength;
		VERSE_ENSURE(NextStrength>Strength);
		if(NextStrength<strength_resolves) {
			// Equate weaker representatives first and ensure links survive stronger merge.
			auto LN = SetEquatedStrength(nullptr,Narrowed,L0,L1,NextStrength);
			if(LN==L1)
				L1=Exchange(L0,L1);
			else if(LN!=L0)
				L0->SetNextStrength(NextStrength), L0->NextRep=Truth(LN);
		}
		if(Strength<=strength_succeeds)
			L0->SucceedsRepShape = Intersection(L0->SucceedsRepShape,L1->SucceedsRepShape);
		L1->SetNextStrength(Strength);
		L1->NextRep = Truth(L0);
		Narrowed    = true;
	}
	return L0;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Equation operation implementations.

equate Equate(const vertex& U) {
	if(auto E=IterateStart()->LocalEquates.Get(U))
		return E.Coerce();
	else
		return equation(U)->Vertices[U];
}
equation FindEquation(const vertex& Source,verifier_iterate Visibility) {
	// Find equation closest to the current context. Is there a way to cache the result without complicating OnSubstitute?
	auto SourceContext = Source.Context()->Visibility();
	for(;;) {
		if(auto SourceEquate=Visibility->LocalEquates.Get(Source))
			return SourceEquate->Equation;
		auto NextVisibility=Visibility.Context()->Visibility().Cast<verifier_iterate>();
		if(Visibility==SourceContext || !NextVisibility)
			return Visibility->Run([&]{
				return Equate(Source)->Equation;
			});
		Visibility=NextVisibility.Coerce();
	}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Inference implementation.

inferences::~inferences() {
	VERSE_ENSURE(!PendingEquates.ReadCount());
}
void inferences::EquateVertices(const vertex& U0,const vertex& U1) const {
	if(auto E0=FindEquation(U0); !E0->Vertices.Has(U1))
		PendingEquates.Set(tuple{Thread->Visibility(),U0,U1});
}
void inferences::CheckEqProduct(const equation& TargetEquation,const equation& SourceEquation) const {
	VERSE_ENSURE(TargetEquation->IsLive);
	if(TargetEquation!=SourceEquation) {
		// We're newly equating equal-indexed elements of distinct tuples.
		// Infer (ProductEqual) by seeing if they're elements of yet-undiscovered equal arrays.
		// Confluence requires we find the lowest matching length to check.
		var<map<nat,box<array_vertex>>>                        ASMap;
		option<tuple<nat,box<array_vertex>,box<array_vertex>>> CheckArrays; 
		for(auto[AS,_]:SourceEquation->EquationArrays)
			ASMap.Set(Length(AS->VertexElements),AS);
		for(auto[BS,_]:TargetEquation->EquationArrays)
			if(auto n=Length(BS->VertexElements); auto AS=ASMap.Get(n))
				if(!CheckArrays || n<CheckArrays->get<0>())
					CheckArrays=Truth(tuple{n,AS.Coerce(),BS});
		if(CheckArrays) {
			bool DoEquate=true;
			auto[n,AS,BS]=CheckArrays.Coerce();
			for(nat i=0; i<n && DoEquate; i++)
				DoEquate=FindEquation(AS->VertexElements[i])==FindEquation(BS->VertexElements[i]);
			if(DoEquate)
				EquateVertices(SourceEquation->EquationArrays.begin().Key(),TargetEquation->EquationArrays.begin().Key());
		}
	}
}
void inferences::Merge(bool ContextLocal,const call_set& TargetCalls,const call_set& SourceCalls,bool AllowDisposedSource) const {
	// Infers (SameCallEqual), (ComputesCallEqual).
	for(auto[SourceCall,_]:SourceCalls)
		for(auto[TargetCall,__]:TargetCalls)
			if(CallMatches(SourceCall,TargetCall))
				EquateVertices(TargetCall,SourceCall);
	TargetCalls += SourceCalls;
}
equation LocalEquation(bool ContextLocal,const equation& E) {
	return ContextLocal? E: FindEquation(E->Vertices.begin().Key());
}
void inferences::Merge(bool ContextLocal,const equation_map<call_set>& TargetCallsMap,const equation_map<call_set>& SourceCallsMap,bool AllowDisposedSource) const {
	VERSE_ENSURE(TargetCallsMap!=SourceCallsMap && TargetCallsMap->IsLive);
	if(ContextLocal)
		SourceCallsMap->Dispose();
	auto Visibility = IterateStart();
	for(auto[SourceEquation,SourceCallsCursor]:SourceCallsMap->Map) {
		VERSE_ENSURE(AllowDisposedSource || SourceEquation->IsLive); // Doesn't hold during OnCommit.
		auto TargetCalls=TargetCallsMap->GetSubscribeToEquationHere(LocalEquation(ContextLocal,SourceEquation));
		Merge(ContextLocal,TargetCalls,SourceCallsCursor.ReadValue(),AllowDisposedSource);
	}
}
void inferences::Merge(bool ContextLocal,const equation_map<equation_map<call_set>>& TargetCallsMapMap,const equation_map<equation_map<call_set>>& SourceCallsMapMap,bool AllowDisposedSource) const {
	VERSE_ENSURE(TargetCallsMapMap!=SourceCallsMapMap && TargetCallsMapMap->IsLive);
	if(ContextLocal)
		SourceCallsMapMap->Dispose();
	auto Visibility = IterateStart();
	for(auto[SourceEquation,SourceCallsCursor]:SourceCallsMapMap->Map) {
		VERSE_ENSURE(SourceEquation->IsLive);
		auto TargetCallsMap=TargetCallsMapMap->GetSubscribeToEquationHere(LocalEquation(ContextLocal,SourceEquation));
		for(auto[SourceFunctionEquation,_]:SourceCallsCursor->Map) // Works for Calls because Parameters are first.
			for(auto[TargetFunctionEquation,__]:TargetCallsMap->Map)
				CheckEqProduct(TargetFunctionEquation,SourceFunctionEquation);
		Merge(ContextLocal,TargetCallsMap,SourceCallsCursor.ReadValue(),AllowDisposedSource);
	}
}
template<class v> v equation_map_managed<v>::GetSubscribeToEquationHere(const equation& TargetEquation) {
	VERSE_ENSURE(Context()->Visibility()==Thread->Visibility() && IsLive);
	TargetEquation->SubscribeToEquationHere(*this);
	return Map.GetInit(TargetEquation);
}
template<class v> void equation_map_managed<v>::OnSubstitute(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation,bool AllowDisposedSource) {
	VERSE_ENSURE(Context()==Thread && IsLive); // Cleared in OnAbandon.
	Inferences.Merge(true,GetSubscribeToEquationHere(TargetEquation),Map.Remove(SourceEquation).Coerce(),AllowDisposedSource);
}
template<class v> void equation_map_managed<v>::OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation) {}
template<class v> void equation_map_managed<v>::OnRelate(const inferences& Inferences,const box<call_vertex>& Call,bool AllowDisposedSource) {}
template<> void equation_map_managed<relation_publisher>::OnSubstitute(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation,bool AllowDisposedSource) {
	VERSE_ENSURE(TargetEquation!=SourceEquation && TargetEquation->IsLive && IsLive);
	VERSE_ENSURE(AllowDisposedSource || IterateStart()->RelationPublishers==box(*this)); // Doesn't hold during OnCommit.
	if(auto SourceRelationSubscribers=Map.Remove(SourceEquation)) // The only place we remove an equation subscription from RelationPublishers.
		for(auto[RelationSubscriber,_]:SourceRelationSubscribers.Coerce())
			RelationSubscriber.Context()->Run([&,&RelationSubscriber=RelationSubscriber]{
				RelationSubscriber->OnSubstitute(Inferences,TargetEquation,SourceEquation,AllowDisposedSource); // May resubscribe to TargetEquation.
			});
	// Need not resubscribe *this to TargetEquation, as prior descendant SubscribeToEquationHere will do it.
};
template<> void equation_map_managed<relation_publisher>::OnEquate(const inferences& Inferences,const equation& TargetEquation,const equation& SourceEquation) {
	var<bag<relation_subscriber>> Seen; // Needed to avoid recursive combinatorial cost of redundant recursion.
	for(auto E:array{TargetEquation,SourceEquation})
		if(auto RelationSubscribers=Map.Get(E))
			for(auto[RelationSubscriber,_]:RelationSubscribers.Coerce())
				if(!Seen.Set(RelationSubscriber))
					RelationSubscriber.Context()->Run([&,&RelationSubscriber=RelationSubscriber]{
						RelationSubscriber->OnEquate(Inferences,TargetEquation,SourceEquation);
					});
}
template<> void equation_map_managed<relation_publisher>::OnRelate(const inferences& Inferences,const box<call_vertex>& Call,bool AllowDisposedSource) {
	auto Visibility        = IterateStart();
	auto CallEquation      = FindEquation(Call);
	auto FunctionEquation  = FindEquation(Call->CallFunctionVertex);
	auto ParameterEquation = FindEquation(Call->CallParameterVertex);

	// Notify child visibility subscribers.
	var<bag<relation_subscriber>> Seen; // Needed to avoid recursive combinatorial cost of redundant recursion.
	for(auto E:array{CallEquation,FunctionEquation,ParameterEquation})
		if(auto RelationSubscribers=Map.Get(E))
			for(auto[RelationSubscriber,_]:RelationSubscribers.Coerce())
				if(!Seen.Set(RelationSubscriber))
					RelationSubscriber.Context()->Run([&,&RelationSubscriber=RelationSubscriber]{
						RelationSubscriber->OnRelate(Inferences,Call,AllowDisposedSource);
					});

	// Merge Calls,Parameters,Functions call_set to infer (SameCallEqual), (ComputesCallEqual), (ProductEqual).
	call_set SourceCalls;
	SourceCalls.Set(Call);
	if(CallEquation.Context()->Visibility()==Visibility) {
		auto TargetCallsMap=CallEquation->Calls->GetSubscribeToEquationHere(ParameterEquation);
		for(auto[TargetFunctionEquation,_]:TargetCallsMap->Map)
			Inferences.CheckEqProduct(TargetFunctionEquation,FunctionEquation);
		Inferences.Merge(true,TargetCallsMap->GetSubscribeToEquationHere(FunctionEquation),SourceCalls,AllowDisposedSource);
	}
	if(ParameterEquation.Context()->Visibility()==Visibility)
		Inferences.Merge(true,ParameterEquation->Parameters->GetSubscribeToEquationHere(FunctionEquation),SourceCalls,AllowDisposedSource);
	if(FunctionEquation.Context()->Visibility()==Visibility)
		Inferences.Merge(true,FunctionEquation->Functions->GetSubscribeToEquationHere(ParameterEquation),SourceCalls,AllowDisposedSource);
}
template<class t=call_vertex,class... us>
requires requires(vertex F,vertex P,us... US) {box<t>(F,P,US...);}
box<t> MakeCallVertex(const vertex& LocalFunction,const vertex& Parameter,const us&... US) {
	inferences Inferences;
	auto LocalFunctionVertexManaged = LocalFunction.CastVertex<vertex_managed>();
	auto Call                       = box<t>(LocalFunction,Parameter,US...);
	IterateStart()->RelationPublishers->OnRelate(Inferences,Call,false);
	Inferences.Infer();
	return Call;
}
void inferences::Infer() {
	for(auto[T,_]:PendingEquates) // PendingEquates may grow as this runs.
		if(auto[C,U0,U1]=T; true)
			C->Run([&,&U0=U0,&U1=U1]{
				equation_managed::EquateVerticesNow(*this,U0,U1);
			});
	PendingEquates=False;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Thread implementation.

void verifier_iterate_managed::OnAbandon() {
	if(VerboseSplitImply && !IsCommitted && IterateVisibility==visibility::Iterate) Print("OnAbandon with CompletedChildFx=",CompletedChildFx," SuspendedFx=",SuspendedFx);
	RelationPublishers->Dispose();
	for(auto[E,__]:Equations)
		E->Dispose();
}
void verifier_iterate_managed::OnCommit() {
	if(VerboseSplitImply) Print("OnCommit with CompletedChildFx=",CompletedChildFx," SuspendedFx=",SuspendedFx);
	auto Visibility=IterateStart();
	iterate_managed::OnCommit();
	VERSE_ENSURE(Context()->Visibility()==Visibility); // Because already committed.
	
	// Dispose to cut off this committed context's RelationPublishers from any updates by parent context.
	OnAbandon();

	// Reconstruct isomorphic equations, relations, and subscriptions in Visibility,
	// without affecting this committed context's RelationPublishers.
	inferences Inferences;
	for(auto[U,_]:LocalEquates)
		Equate(U);
	for(auto[U,_]:LocalEquates)
		if(auto UM=U.CastVertex<vertex_managed>()) {
			for(auto[V,__]:UM->VertexPredecessors)
				Inferences.EquateVertices(U,V);
			if(auto Call=UM.Coerce().Cast<call_vertex>())
				Visibility->RelationPublishers->OnRelate(Inferences,Call.Coerce(),true);
		}
	Inferences.Infer();

	// Move LocalVertexSubscribers.
	for(auto[SourceEquation,_]:Equations)
		for(auto TargetEquation=FindEquation(SourceEquation->Vertices.begin().Key()); auto[VertexSubscriber,__]:SourceEquation->LocalVertexSubscribers)
			TargetEquation->LocalVertexSubscribers.Set(VertexSubscriber);

	// Because inferences here solely reconstructed isomorphic equations and relations in Visibility,
	// we need only update our child subscribers via OnSubstitute and move our subscribers to their new equations,
	// and this loses no information and causes no further inference.
	for(auto[SourceEquation,RelationSubscribers]:RelationPublishers->Map) {
		auto TargetEquation=FindEquation(SourceEquation->Vertices.begin().Key());
		for(auto[RelationSubscriber,_]:RelationSubscribers.ReadValue())
			if(Visibility<RelationSubscriber.Context()->Visibility()) {
				TargetEquation->SubscribeToEquationHere(RelationSubscriber); // Move from this committed context to Visibility.
				if(TargetEquation!=SourceEquation) {
					VERSE_ENSURE(!SourceEquation->IsLive);
					RelationSubscriber.Context()->Run([&]{
						RelationSubscriber->OnSubstitute(Inferences,TargetEquation,SourceEquation,true);
					});
				}
			}
	}
	Equations     = False;
	LocalEquates  = False;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Printing everything.

string ExposeToString(const equate& Equate) {
	return ToString(Equate->EquateVertex);
}
string ExposeToString(const equation& E) {
	string S;
	bool AnyU=false;
	for(auto[A,AL]:E->Vertices)
		if(auto ARD=AL->Rep(strength_decides); AL.ReadValue()==ARD)             // Found a unique decides-group.
			for(bool AnyD=false; auto[B,BL]:E->Vertices)
				if(auto BRS=BL->Rep(strength_succeeds); BL.ReadValue()==BRS)    // Found a unique succeeds-group.
					if(auto BRD=BRS->Rep(strength_decides); BRD==ARD)           // The unique succeeds-group is in this decides-group.
						for(bool AnyS=false; auto[C,CL]:E->Vertices)
							if(auto CRS=CL->Rep(strength_succeeds); CRS==BRS) { // Found a member of this succeeds-group.
								if(AnyU && !AnyD && !AnyS) S += "  !  "_VS; AnyU=true;
								if(AnyD && !AnyS         ) S += " ? "_VS;   AnyD=true;
								if(AnyS                  ) S += "="_VS;     AnyS=true;
								S+=ToString(C);
							}
	VERSE_ENSURE(S);
	return S;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Runtime language features.

struct run_beta_context_managed: context_managed {
	context BetaComposeContext;
	run_beta_context_managed(const context& BetaComposeContext0): 
		context_managed(true,true,Thread->AllowFx,contradicts,Thread->Depth+1), BetaComposeContext(BetaComposeContext0) {}
	current_step RunBetaCheckRange(const step& Step0) {
		if(HasSuspensions()) {
			//!! Note this is useless because runtime EvalCallStep doesn't wait for multiple functions.
			//!! Need to spec and rework to fix: f()<interacts>:=Print(456); f[].
			return Step0;
		}
		return BetaComposeContext->TerminateStep(Step0);
	}
	current_step OnRunSuspensionStep(const step& Step0) {
		return RunStep(Step0,[=,Self=box(*this)](const step& Step1)->current_step {
			return RunSuspensionsStep([=]()->current_step {
				return Self->RunBetaCheckRange(Step1);
			});
 		});
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier operations.

struct unify_suspension: suspension_managed {
	const char* What;
	locus       Locus;
	vertex      A, B;
	unify_suspension(const char* What0,const locus& Locus0,const vertex& A0,const vertex& B0): 
		suspension_managed(only_cardinalities&abstracts), What(What0), Locus(Locus0), A(A0), B(B0) {}
	error OnDescribe() const override {
		return R00(Locus,ToString(What," unify_suspension(",A,",",B,") with ",FindEquation(A)));
	}
	current_step OnRunSuspensionStep(const step& Step0) override {
		return Suspend(), Step0;
	}
	fx OnRefineFx(bool& Narrowed) override {
		// Compared to the spec, this captures the porton of fx{u=v} attributable to data flow,
		// but not the portion attributable to as[i] in products. Beta reductions are separate.
		// but not the portion attributed of the vertices which are captured in suspensions elsewhere.
		return converges&computes&no_unifies&accepts&FindEquation(A)->EquatedFx(A,B);
	}
};
struct cast_unify_suspension: unify_suspension {
	using unify_suspension::unify_suspension;
	error OnDescribe() const override {
		return R00(Locus,ToString(What," cast_unify_suspension(",A,",",B,") with ",FindEquation(A)));
	}
};
void UnifyBatch(const char* What,const locus& Locus,const vertex& A,const vertex& B) {
	if(A==B)
		return;

	// Apply rule (UnifyFlexible) attaching context-independent unificands to flexible variables.
	auto AF=CastFlexible(A).Coerce();
	AF->VertexPredecessors.Set(B);
	if(auto BF=CastFlexible(B))
		BF->VertexPredecessors.Set(A);

	// Apply (UnifyIntro) bookkeeping here.
	inferences Inferences;
	Inferences.EquateVertices(A,B);
	Inferences.Infer();

	// Track op::unify effects here.
	box<unify_suspension>(What,Locus,A,B)->Suspend();
}
current_step UnifyStep(const char* What,const locus& Locus,const vertex& A,const vertex& B,const step& Step0) {
	VERSE_ENSURE(CastFlexible(A));
	UnifyBatch(What,Locus,A,B);
	return ResumeStep(Step0);
}
template<class t> t GetArrayElement(const array<t>& AS,nat i) {
	return AS[i];
}
vertex GetArrayElement(const box<array_vertex>& AS,nat i) {
	return AS->VertexElements[i];
}
template<class unificand> auto MakeArrayUnificand(nat n) {
	if constexpr(Runs<unificand>)
		return For(n,[&](nat i) {return future<>();});
	else if(n>0) {
		auto Array=box<array_vertex>(n);
		Array->VertexElements=For(n,[&](nat i) {
			return MakeCallVertex<call_vertex>(Array,vertex(i),False);
		});
		return Array;
	}
	else return vertex(False).CastVertex<array_vertex>().Coerce();
}
static current_step FailTargetStep(const char* What,const locus& Locus,const future<>& Target,const step& Step0) {
	return FailStep(Locus,What,Step0);
}
static void FailTargetBatch(const char* What,const locus& Locus,const vertex& Target) {
	// Instigate failure with Target at the strength of this context.
	auto Fail=box<fail_vertex>();
	UnifyBatch(What,Locus,Target,Fail);
}
static current_step FailTargetStep(const char* What,const locus& Locus,const vertex& Target,const step& Step0) {
	return FailTargetBatch(What,Locus,Target), ResumeStep(Step0);
}
static current_step DivergeTargetStep(const char* What,const locus& Locus,const vertex& Target,const step& Step0) {
	// Instigate failure with Target at the strength of this context.
	auto Fail=MakeContext<vertex>(HereContext(Locus,ToString(What)),Thread->AllowFx,true,GetAssumedFxConst(only_succeeds),DefaultGetSuspendedFx)->Run([&]{
		return box<fail_vertex>();
	});
	return UnifyStep(What,Locus,Target,Fail,Step0);
}
struct abstracting_suspension: suspension_managed {
	locus          Locus;
	function<fx()> AbstractingFx;
	abstracting_suspension(const locus& Locus,const function<fx()>& AbstractingFx0): AbstractingFx(AbstractingFx0) {}
	error OnDescribe() const override {
		return R00(Locus,"abstracting_suspension"_VS);
	}
	current_step OnRunSuspensionStep(const step& Step0) override {
		return Suspend(), Step0;
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier casting, extracting a nested mix of verifier and runtime values from a verifier vertex.

// WhenCastStep provides runtime and verifier casting of variables to arbitrary C++ types.
// The case of casting to a simple head-normal form aligns with the specification's 'cast' op.
// Casting to more advanced types translates to nestings of 'cast' propagating CastHeadFailSources.
using strength_sources             = array<tuple<vertex,vertex>>;
template<class t> using on_succeed = function<void(const strength_sources&,const t&)>;
template<class t> struct casted {
	strength_sources StrengthSources;
	t                Result;
};
struct when_cast_suspension: suspension_managed {
	fx                                CastHoldFx;
	vertex                            Source;
	const option<box<vertex_managed>> CastFailTarget;
	var<bag<vertex>>                  CastHeadFailSources;
	bool                              Narrow, Weaken;
	when_cast_suspension(fx CastHoldFx0,const vertex& Source0,const option<box<vertex_managed>>& CastFailTarget0,bool Narrow0,bool Weaken0):
		suspension_managed(CastHoldFx0),
		CastHoldFx(CastHoldFx0), Source(Source0), CastFailTarget(CastFailTarget0),
		Narrow(Narrow0), Weaken(Weaken0) {}
	fx OnRefineFx(bool& Narrowed) {
		// Spec needs work to explain monotonicity and <unifies> here relative to <unifies> elsewhere.
		for(auto[FV,__]:CastHeadFailSources)
			// Use shallow effects; deep would make test(F00){xs:=array{3} => xs(0)=4} P00 since xs deep-fails.
			if(auto SourceHeadFx=FindEquation(FV)->EquationHeadFx; SourceHeadFx<=fails)
				CastHoldFx&=SourceHeadFx&converges&computes&no_unifies&accepts;
		return CastHoldFx;
	}
	void OnCastFail(const strength_sources& StrengthSources) {
		if(Narrow) {
			// Note weakened fx here. StrengthSources is built up from a series of nested product and head casts.
			auto WeakenContext = MakeWeakenContext("when_cast_suspension.Fail-WeakenContext",StrengthSources,true);
			if(CastFailTarget)
				// Tell CastFailTarget we're failing, using WeakenContext to prevent escalation.
				WeakenContext->Run([&]{
					FailTargetBatch("when_cast_suspension.Fail",locus{},CastFailTarget.Coerce());
				});
			ReadySuspensionBatch();
		}
	}
	verifier_context MakeWeakenContext(const char* What1,const strength_sources& StrengthSources,bool DoWeaken) {
		option<verifier_context>  DomainContexts;
		option<array<beta_frame>> DomainFrames;
		if(DoWeaken) { // True for everything but EvalCallStep-function_vertex.
			auto DomainContext = MakeContext<vertex>(
				HereContext(locus{},"when_cast_suspension.DomainContext"_VS),Thread->AllowFx,
				false,GetAssumedFxHere(),DefaultGetSuspendedFx);
			DomainContext->Run([&]{
				SetContextFrames(Coerce<box<verifier_context_state>>(Thread->Context())->ContextFrames);
				for(auto StrengthSource:StrengthSources) { // Checks unifications without adding to VertexPredecessors.
					auto[CastSource,CastTarget]=StrengthSource;
					box<cast_unify_suspension>(What1,locus{},CastSource,CastTarget)->Suspend();
				}
			});
			DomainContext->Suspend();
			DomainContexts=Truth(DomainContext);
		}
		auto WeakenContext=MakeContext<vertex>(HereContext(locus{},"when_cast_suspension.WeakenContext"_VS),
			Thread->AllowFx,
			false,
			[=,OriginalContext=Thread] {
				auto AssumedFx = Coerce<box<verifier_context_state>>(OriginalContext)->GetAssumedFx();
				if(DoWeaken) // Without this, x=fail op escalates.
					AssumedFx += only_cardinalities & abstracts & DomainContexts->CompletedChildFx;
				return AssumedFx;
			},
			DefaultGetSuspendedFx
		);
		WeakenContext->Run([&]{
			SetContextFrames(Coerce<box<verifier_context_state>>(Thread->Context())->ContextFrames); // For GetVerifierNative.
		});
		return WeakenContext;
	}
};
struct when_cast_base: on_vertex {
	vertex                    Source;
	box<when_cast_suspension> WhenCastSuspension;
	when_cast_base(const vertex& Source0,const box<when_cast_suspension>& WhenCastSuspension0):
		Source(Source0), WhenCastSuspension(WhenCastSuspension0) {
		// Note that Source failing with strength S implies Target fails with strength at least S.
		// We don't notify, as this failure only affects recalculation and not suspension actions.
		if(WhenCastSuspension->CastFailTarget) {
			WhenCastSuspension->CastFailTarget->VertexHeadFailSources.Set(Source);
			WhenCastSuspension->CastHeadFailSources.Set(Source);
		}
	}
	virtual void OnConstructed() {
		if(WhenCastSuspension->Narrow)
			VERSE_ENSURE(CastFlexible(Source));
		auto SourceEquation=FindEquation(Source);
		SourceEquation->LocalVertexSubscribers.Set(*this);
		for(auto[U,UL]:SourceEquation->Vertices)
			OnVertex(U);
	}
	template<class t> void ApplyShape() {
		if(WhenCastSuspension->Narrow)
			UnifyBatch("when_cast_base-Unify",locus{},Source,box<vertex_managed>(false,ShapeOf<t>()));
	}
};
template<class t>     struct when_cast;
template<class t>     struct when_cast<array<t>>;
template<class... ts> struct when_cast<tuple<ts...>>;
template<class t>     expose_mutable ExposeUnique(const when_cast<t>&);
expose_mutable ExposeUnique(const when_cast_base&);
template<class t> struct when_cast: when_cast_base {
	// Default casting for atomic types.
	on_succeed<t>    OnCastSucceed;
	array<casted<t>> Casteds;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<t>& OnCastSucceed0):
		when_cast_base(Source0,TargetSus0), OnCastSucceed(OnCastSucceed0) {
		ApplyShape<t>();
	}
	void OnVertex(const vertex& U) override {
		if constexpr(IsSubtype<t,box<vertex_managed>>) {
			if(auto VO=U.CastVertex<t>()) {
				auto SS=array{tuple{Source,U}};
				auto Casted=casted<t>{SS,VO.Coerce()};
				Casteds+=array{Casted};
				OnCastSucceed(SS,VO.Coerce());
			}
		}
		else if(auto AO=U.CastHead()) {
			if(auto TO=Cast<t>(AO.Coerce())) {
				auto Casted=casted<t>{array{tuple{Source,U}},TO.Coerce()};
				Casteds+=array{Casted};
				OnCastSucceed(Casted.StrengthSources,Casted.Result);
			}
			else WhenCastSuspension->OnCastFail(array{tuple{Source,vertex(AO.Coerce())}});
		}
	}
};
template<> struct when_cast<vertex>: when_cast_base {
	// Accept the specified vertex as-is. Useful for nesting inside arrays with eta conversion.
	on_succeed<vertex>    OnCastSucceed;
	array<casted<vertex>> Casteds;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<vertex>& OnCastSucceed0):
		when_cast_base(Source0,TargetSus0), OnCastSucceed(OnCastSucceed0), Casteds(array{casted<vertex>{False,Source0}}) {
	}
	void OnConstructed() override {
		when_cast_base::OnConstructed();
		OnCastSucceed(False,Source);
	}
};
template<> struct when_cast<falsity>: when_cast_base {
	// Fail.
	array<falsity> Casteds;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<falsity>& OnCastSucceed):
		when_cast_base(Source0,TargetSus0) {
		ApplyShape<falsity>();
		WhenCastSuspension->OnCastFail(False);
	}
};
template<class t> struct when_cast<array<t>>: when_cast_base {
	// Cast to array.
	on_succeed<array<t>>     OnCastSucceed;
	bool                     GotArray;
	array<box<when_cast<t>>> Casts;
	array<casted<array<t>>>  Casteds;
	bool                     FirstSuccess;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<array<t>>& OnCastSucceed0):
		when_cast_base(Source0,TargetSus0), OnCastSucceed(OnCastSucceed0), GotArray(false), FirstSuccess(false) {
		ApplyShape<array<t>>();
	}
	void OnVertex(const vertex& U) override {
		if(auto AO=U.CastHead()) {
			if(auto BSO=AO->Cast<array_vertex>(); BSO && !GotArray) {
				GotArray = true;
				Casts    = For(Length(BSO->VertexElements),[&](nat i) {
					return box<when_cast<t>>(MakeCallVertex<call_vertex>(Source,vertex(i),False),WhenCastSuspension,[=,Self=box(*this)](const strength_sources& SS,const t& T) {
						Self->SucceedWithElements(0,i,SS,array<t>{});
					});
				});
				for(auto WC:Casts)
					WC->OnConstructed();
				if(!BSO->VertexElements)
					return SucceedWithElements(0,0,array{tuple{Source,vertex(False)}},False);
			}
			else if(!AO->Cast<abstraction_vertex>())
				WhenCastSuspension->OnCastFail(array{tuple{Source,U}});
		}
	}
	void SucceedWithElements(nat i,nat i0,const strength_sources& StrengthSources,const array<t>& AS) {
		if(i<Length(Casts)) {
			auto ElementCasteds=Casts[i]->Casteds;
			for(nat j=0,n=Length(ElementCasteds); j<n; j++)
				if(i!=i0 || j==n-1) { // We're only notifying new successes, so at index i0, only consider new element n-1.
					SucceedWithElements(i+1,i0,StrengthSources+ElementCasteds[j].StrengthSources,AS+array<t>{ElementCasteds[j].Result});
				}
		}
		else if(!FirstSuccess) {
			FirstSuccess=true;
			auto Casted=casted<array<t>>{StrengthSources,AS};
			Casteds+=array{Casted};
			OnCastSucceed(StrengthSources,AS);
		}
	}
};
template<class... ts> requires(sizeof...(ts)>0) struct when_cast<tuple<ts...>>: when_cast_base {
	// Cast to tuple.
	on_succeed<tuple<ts...>>             OnCastSucceed;
	bool                                 GotArray;
	option<tuple<box<when_cast<ts>>...>> Casts;
	array<casted<tuple<ts...>>>          Casteds;
	bool                                 FirstSuccess;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<tuple<ts...>>& OnCastSucceed0):
		when_cast_base(Source0,TargetSus0), OnCastSucceed(OnCastSucceed0), GotArray(false), FirstSuccess(false) {
		ApplyShape<tuple<ts...>>();
	}
	void OnVertex(const vertex& U) override {
		if(auto AO=U.CastHead()) {
			if(auto BSO=AO->Cast<array_vertex>(); BSO && Length(BSO->VertexElements)==sizeof...(ts) && !GotArray) {
				GotArray   = true;
				auto Self  = box(*this);
				auto BS    = BSO.Coerce();
				Casts      = Truth(StaticFor<sizeof...(ts)>([&]<nat i> {
					using t = typename std::tuple_element<i,tuple<ts...>>::type;
					return box<when_cast<t>>(MakeCallVertex<call_vertex>(Self->Source,vertex(i),False),Self->WhenCastSuspension,[=](const strength_sources& SS,const t& T) {
						Self->SucceedWithElements(i,SS);
					});
				}));
				StaticFor<sizeof...(ts)>([&]<nat i> {
					return Casts->template get<i>()->OnConstructed(), False;
				});
			}
			else if(BSO || !AO->Cast<abstraction_vertex>())
				WhenCastSuspension->OnCastFail(array{tuple{Source,U}});
		}
	}
	template<class... us> void SucceedWithElements(nat i0,const strength_sources& StrengthSources,const us&... SS) {
		constexpr nat i=sizeof...(SS);
		if constexpr(i<sizeof...(ts)) {
			auto ElementCasteds=Casts->template get<i>()->Casteds;
			for(nat j=0,ni=Length(ElementCasteds); j<ni; j++) {
				if(i!=i0 || j==ni-1) { // We're only notifying new successes, so at index i0, only consider new element ni-1.
					SucceedWithElements(i0,StrengthSources+ElementCasteds[j].StrengthSources,SS...,ElementCasteds[j].Result);
				}
			}
		}
		else if(!FirstSuccess) {
			FirstSuccess=true;
			auto Casted=casted<tuple<ts...>>{StrengthSources,tuple<ts...>(SS...)};
			Casteds+=array{Casted};
			OnCastSucceed(StrengthSources,tuple<ts...>(SS...));
		}
	}
};
template<> struct when_cast<comparable>: when_cast_base {
	// Cast to anything comparable.
	on_succeed<comparable>            OnCastSucceed;
	bool                              GotArray;
	array<box<when_cast<comparable>>> Casts;
	array<casted<comparable>>         Casteds;
	when_cast(const vertex& Source0,const box<when_cast_suspension>& TargetSus0,const on_succeed<comparable>& OnCastSucceed0):
		when_cast_base(Source0,TargetSus0), OnCastSucceed(OnCastSucceed0), GotArray(false) {
		ApplyShape<comparable>();
	}
	void OnVertex(const vertex& U) override {
		if(auto AO=U.CastHead()) {
			if(auto BSO=AO->Cast<array_vertex>(); BSO && !GotArray) {
				GotArray=true;
				box<when_cast<array<comparable>>>(Source,WhenCastSuspension,OnCastSucceed)->OnConstructed();
			}
			else if(!AO->Cast<abstraction_vertex>()) {
				auto Casted=casted<comparable>{array{tuple{Source,U}},AO.Coerce()};
				Casteds+=array{Casted};
				OnCastSucceed(Casted.StrengthSources,Casted.Result);
			}
		}
	}
};

// Verifier casting.
template<class t0,class t=cast_type<t0>,class describe,class on_succeed_step>
requires requires(describe Describe,on_succeed_step OnSucceedStep,t T,step S) {
	error(Describe());
	current_step(OnSucceedStep(T,S));
}
current_step WhenCastStep(const describe& Describe,fx Fx,const vertex& Source,const option<vertex>& CastFailTarget,const step& Step0,const on_succeed_step& OnSucceedStep,bool Narrow,bool Weaken) {
	// Should decompose this into subscribing to a stream of t and publishing a stream of t.
	struct local_when_cast_suspension: when_cast_suspension {
		describe         Describe;
		on_succeed_step  OnSucceedStep;
		array<casted<t>> Received;
		nat              Latest;
		local_when_cast_suspension(const describe& Describe0,fx Fx0,const vertex& Source0,const option<box<vertex_managed>>& CastFailTarget0,bool Narrow0,bool Weaken0,const on_succeed_step& OnSucceedStep0): 
			when_cast_suspension(Fx0,Source0,CastFailTarget0,Narrow0,Weaken0), Describe(Describe0), OnSucceedStep(OnSucceedStep0), Latest(0) {}
		error OnDescribe() const override {
			return Describe();
		}
		current_step OnRunSuspensionStep(const step& Step0) override {
			if(Latest==Length(Received)) {
				bool Narrowed=false;
				IsReady=false;
				SuspendedFx=OnRefineFx(Narrowed);
				return this->Suspend(), Step0;
			}
			//!! This is too strong when it mixes a sub-succeeds-unified head and fails elsewhere;
			// must incorporate MakeWeakenContext strength.
			CastHoldFx         = succeeds&converges&computes&no_unifies&accepts;
			auto i             = Latest++;
			MakeWeakenContext("WhenCastStep-Succeed",Received[i].StrengthSources,Weaken);
			return OnSucceedStep(Received[i].Result,[=,Self=box(*this)]()->current_step{
				return Self->OnRunSuspensionStep(Step0);
			});
		}
	};
	auto FlexibleCastFailTarget=CastFailTarget? Truth(CastFlexible(CastFailTarget.Coerce()).Coerce()): False;
	box<local_when_cast_suspension> WhenCastSuspension(Describe,Fx,Source,FlexibleCastFailTarget,Narrow,Weaken,OnSucceedStep);
	WhenCastSuspension->Suspend();
	auto WC=box<when_cast<t>>(Source,WhenCastSuspension,[=](const strength_sources& SS,const t& T) {
		WhenCastSuspension->Received+=array{casted<t>{SS,T}};
		WhenCastSuspension->ReadySuspensionBatch();
	});
	WC->OnConstructed();
	return ResumeStep(Step0);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier functions. Calling.

// Verifier array calling.
current_step array_vertex::OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0) {
	auto ResolvesNow            = var<bool>(false);
	auto Fx                     = only_cardinalities&(VertexElements? resolves: succeeds); // Succeeds ok because of FailTargetStep inside.
	CallerVertex->CallProductFx = only_succeeds;
	return WhenCastStep<comparable>(
		VERIFIER_HERE("array_vertex.OnVerifierCallStep.Parameter"),Fx,CallerVertex->CallParameterVertex,Truth(vertex(CallerVertex)),
		[=,Self=box(*this)]()->current_step {
			if(!Self->VertexElements) // Per (CallProductFails) n1=0 case, so Parameter-contradicts masks cast-fails.
				return FailTargetStep("ArrayCallStep-immediate",locus{},vertex(CallerVertex),Step0);
			if(!*ResolvesNow && (iterates<=contradicts+Thread->AllowFx || Length(Self->VertexElements)==1)) {
				return ForForkStep(CallerVertex->CallOpLocus,!IsImplying()? Truth(computes): False,0,Length(Self->VertexElements),Step0,[=](nat i,const step& Step1)->current_step {
					return UnifyStep("ArrayCallStep-fork",locus{},CallerVertex->CallParameterVertex,vertex(i),Step1);
				});
			}
			return Step0;
		},
		[=,Self=box(*this)](const comparable& A,const step& Step1)->current_step {
			ResolvesNow=true;
			if(auto i=CastIndex(A); i>=Length(Self->VertexElements)) // (CallProductFails) not 0<=head<n1 case, here so cast-contradicts masks parameter-fails.
				return FailTargetStep("ArrayCallStep-parameter",locus{},vertex(CallerVertex),Step1);
			return Step1;
		},
		false,true
	);
}

// Make a verifier function without domain-range separation or constraints. Overloads runtime MakeStepFunction.
template<class f> requires requires(f F,vertex RU,vertex PU,step S) {current_step(F(RU,PU,S));}
vertex MakeStepFunction(const f& F) {
	return box<lambda_vertex>([=](const box<lambda_vertex>& Lambda,const box<caller_vertex>& CallerVertex,const step& Step0)->current_step {
		// Perhaps function data flow will eliminate the need for CalleeBetaVertex in this trivial case.
		auto CallBetaContext = MakeContext<vertex>(
			HereContext(locus{},"verifier_step_function"_VS),Thread->AllowFx,
			false,GetAssumedFxHere(),DefaultGetSuspendedFx);
		return CallBetaContext->RunStep(Step0,[=](const step& Step1)->current_step {
			SetContextFrames(Coerce<box<verifier_context_state>>(Thread->Context())->ContextFrames); // For GetVerifierNative.
			auto CalleeBetaVertex = MakeCallVertex<callee_beta_vertex>(Lambda,CallerVertex->CallParameterVertex,CallerVertex,CallBetaContext,CallerVertex);
			return UnifyStep("CallerVertex-CalleeBetaVertex",locus{},vertex(CallerVertex),NEW_FUNCTION_FLOW? vertex(CallerVertex): vertex(CalleeBetaVertex),[=]()->current_step {
				return F(CalleeBetaVertex,CalleeBetaVertex->CallParameterVertex,Step1);
			});
		});
	});
}

// Make a function with proper constraints.
template<class unificand,class init,class domain,class range> unificand MakeLambda(
	const locus&            Locus,
	const init&             Init,
	const domain&           Domain,
	const range&            Range,
	const array<unificand>& Supertypes=False
) {
	if constexpr(Runs<unificand>)
		return MakeStepFunction([=](const future<>& Target,const future<>& Parameter,const step& Step0)->current_step {
			auto Initial=Init(Thread,Thread,Target,Parameter);
			return Domain(Initial,Target,Parameter,[=]()->current_step {
				return Range(Initial,Target,Parameter,Step0);
			});
		});
	else
		return box<lambda_vertex>([=](const box<lambda_vertex>& Lambda,const box<caller_vertex>& CallerVertex,const step& Step0)->current_step {
			auto CallBetaContext = MakeContext<unificand>(
				HereContext(CallerVertex->CallOpLocus,"Beta"_VS),Thread->AllowFx,
				false,GetAssumedFxHere(),DefaultGetSuspendedFx);
			return CallBetaContext->RunStep(Step0,[=](const step& Step1)->current_step {
				auto DomainContext   = MakeContext<unificand>(HereContext(Locus,"Domain"_VS),Thread->AllowFx,
					false,GetAssumedFxHere(),DefaultGetSuspendedFx);
				auto RangeContext    = MakeContext<unificand>(HereContext(Locus,"Range"_VS),Thread->AllowFx,
					false,GetAssumedFxHere(),DefaultGetSuspendedFx);
				auto CalleeBetaVertex = MakeCallVertex<callee_beta_vertex>(Lambda,CallerVertex->CallParameterVertex,CallerVertex,CallBetaContext,CallerVertex);
				CallBetaContext->GetSuspendedFx = [=](fx NewChildFx) {
					auto CallEquation              = FindEquation(CalleeBetaVertex);
					auto FunctionEquation          = FindEquation(Lambda);
					auto ParameterEquation         = FindEquation(CallerVertex->CallParameterVertex);
					auto GlobalComparableVertex    = GetVerifierNative("comparable");
					if(NewChildFx<=resolves && ParameterEquation->EquatedComparable(CallerVertex->CallParameterVertex))
						for(auto[U,_]:CallEquation->Vertices) // Perhaps should look for decides-equate with, not equality, so it works for user calls to GlobalComparableVertex.
							if(auto Call=U.template CastVertex<callee_beta_vertex>(); Call && 
								Call->CallFunctionVertex==GlobalComparableVertex && 
								Call->CallParameterVertex==CallerVertex->CallParameterVertex)
								NewChildFx &= decides;
					for(auto[Call,_]:CallEquation->Calls->Map[ParameterEquation]->Map[FunctionEquation]) 
						if(CallMatches(CalleeBetaVertex,Call)) //!! TODO: Reconcile with inference rules and (CallClosedDown).
							NewChildFx &=
								// Modulate by function and parameter strength as demonstrated by e.g.
								// test(D00){a&b:int => f(:nat):int => f[a] => allow{a=b} where f[b]}
								ambiguates +
								AssumedFxHere(Call.Context()) +
								FunctionEquation->EquatedFx(Lambda,Call->CallFunctionVertex) +
								ParameterEquation->EquatedFx(CallerVertex->CallParameterVertex,Call->CallParameterVertex);
					return NewChildFx;
				};
				auto Initial = Init(DomainContext,RangeContext,CalleeBetaVertex,CallerVertex->CallParameterVertex);
				return UnifyStep("CallerVertex-CalleeBetaVertex",locus{},vertex(CallerVertex),NEW_FUNCTION_FLOW? vertex(CallerVertex): vertex(CalleeBetaVertex),[=]()->current_step {
					return DomainContext->RunStep(
						[=]()->current_step {
							return RangeContext->RunStep(Step1,[=](const step& Step2)->current_step {
								for(auto CurrentCalleeBetaVertex=CalleeBetaVertex; auto Supertype:Supertypes) {
									auto SupertypeCalleeBetaVertex = MakeCallVertex<callee_beta_vertex>(Supertype,CurrentCalleeBetaVertex->CallParameterVertex,CallerVertex,CallBetaContext,CallerVertex);
									//if(!NEW_FUNCTION_FLOW)//!!this can't be right
										UnifyBatch("RunRange.Unify.Supertypes",locus{},SupertypeCalleeBetaVertex,CurrentCalleeBetaVertex);
									CurrentCalleeBetaVertex = SupertypeCalleeBetaVertex;
								}
								return ResumeStep([=]()->current_step {
									return Range(Initial,CalleeBetaVertex,CalleeBetaVertex->CallParameterVertex,Step2);
								});
							});
						},
						[=](const step& Step2)->current_step {
							return Domain(Initial,CalleeBetaVertex,CalleeBetaVertex->CallParameterVertex,Step2);
						}
					);
				});
			});
		});
}

// Make a strict verifier function with typing.
template<class unificand,class f,class r,class... ps> auto MakeStrictFunction(const char* What,fx LambdaAllowFx,const f& F,r(*)(const step&,const unificand&,ps...)) {
	using pt = parameter_tuple<ps...>;
	return MakeLambda<unificand>(locus{},
		[=](const context& DomainContext,const context& RangeContext,const unificand& Result,const unificand& Parameter) {
			if constexpr(!Runs<unificand>) {
				auto Frames = Coerce<box<verifier_context_state>>(Thread->Context())->ContextFrames;
				SetContextFrames(Frames);
				DomainContext->Run([&]{SetContextFrames(Frames);});
				RangeContext ->Run([&]{SetContextFrames(Frames);});
			}
			return False;
		},
		[=](tuple<>,const unificand& Result,const unificand& Parameter,const step& Step0)->current_step {
			return WhenCastStep<typename pt::type>(
				VERIFIER_HERE(What),LambdaAllowFx,Parameter,Truth(Result),Step0,
				[=](const typename pt::type& Parameter,const step& Step1)->current_step {
					return pt::CallStep(Step1,F,Result,Parameter); // Only works because not abstract.
				},
				false,true
			);
		},
		[=](tuple<>,const unificand& Result,const unificand& Parameter,const step& Step0)->current_step {
			return Step0;
		}
	);
}
template<class unificand,class f,class ct=callable<f>> auto MakeStrictFunction(const char* What,fx LambdaAllowFx,const f& F) {
	return MakeStrictFunction<unificand>(What,LambdaAllowFx,F,static_cast<ct*>(nullptr));
}

// Casting to simple types (strict identity functions).
template<class unificand,class t> auto MakeCastFunction(fx LambdaAllowFx,const array<unificand>& Functions=False) {
	return MakeLambda<unificand>(locus{},
		[=](const context& DomainContext,const context& RangeContext,const unificand& Result,const unificand& Parameter) {
			if constexpr(!Runs<unificand>) {
				auto Frames = Coerce<box<verifier_context_state>>(Thread->Context())->ContextFrames+array{beta_frame(FrameSigil,regs(0))};
				SetContextFrames(Frames);
				DomainContext->Run([&]{SetContextFrames(Frames);});
				RangeContext ->Run([&]{SetContextFrames(Frames);});
			}
			return False;
		},
		[=](tuple<>,const unificand& Result,const unificand& Parameter,const step& Step0)->current_step {
			return WhenCastStep<t>(
				VERIFIER_HERE("MakeCastFunction-WhenCastStep"),LambdaAllowFx,Result,Truth(Result),Step0,
				[=](const t& T,const step& Step1)->current_step {
					return Step1;
				},
				true,true
			);
		},
		[=](tuple<>,const unificand& Result,const unificand& Parameter,const step& Step0)->current_step {
			return UnifyStep("MakeCastFunction-Unify",locus{},Result,Parameter,Step0);
		},
		Functions
	);
}

// Input lambda vertex.
current_step input_function_vertex::OnVerifierCallStep(const box<caller_vertex>& CallerVertex,const step& Step0) {
	VERSE_ERR("input_function_vertex.OnVerifierCallStep");
	//Stuck(function_allows,VERIFIER_HERE("input_function_vertex.OnVerifierCallStep"));
	//return Step0;
}

current_step EvalCallStep(const locus& Locus,const future<>& ResultUnificand,const future<>& FunctionUnificand,const future<>& ParameterUnificand,const option<future<>>&,const step& Step0) {
	return WhenCastStep<function<>>(
		Here(Locus,"EvalCallStep"_VS),accepts,FunctionUnificand,Truth(ResultUnificand),Step0,
		[=](const function<> Function,const step& Step1)->current_step {
			return Function->OnCallStep(ResultUnificand,ParameterUnificand,Step1);
		},
		false,true
	);
}
step EvalCallStep(const locus& Locus,const vertex& ResultVertex,const vertex& FunctionVertex,const vertex& ParameterVertex,const option<vertex>& IgnoreVertex,const step& Step0) {
	// This initiates the verifier inference rules (CallProductIntro), (CallClosedIntro).
	VERSE_ENSURE(CastFlexible(FunctionVertex));
	auto Describe     = [=]{return C00(Locus,"EvalCallStep"_VS);};
	auto CallerVertex = var<option<box<caller_vertex>>>(False);
	auto GotArray     = var<bool>(false);
	// No WhenCastStep.Weaken; rely on data flow for tighter determination.
	return WhenCastStep<box<function_vertex>>(
		Describe,effects,FunctionVertex,Truth(ResultVertex),Step0,
		[=](const box<function_vertex>& LambdaVertex,const step& Step1)->current_step {
			//Print("ECS ",LambdaVertex);
			if(IgnoreVertex && LambdaVertex==IgnoreVertex.Coerce() || LambdaVertex.Cast<input_function_vertex>())
				return Step1;
			auto Step2=step([=]()->current_step {
				if(LambdaVertex.Cast<array_vertex>()) { // Treat calling one array as equivalent to calling all.
					if(*GotArray)
						return Step1;
					else
						GotArray=true;
				}
				return LambdaVertex->OnVerifierCallStep((*CallerVertex).Coerce(),Step1);
			});
			if(!*CallerVertex) {
				// Every user call receives a unique identity. If <computes>, it becomes irrelevant.
				CallerVertex = Truth(MakeCallVertex<caller_vertex>(FunctionVertex,ParameterVertex,Locus));
				return UnifyStep("EvalCallStep",locus{},ResultVertex,(*CallerVertex).Coerce(),Step2);
			}
			return Step2;
		},
		false,false // Don't weaken because data flow analysis handles it more tightly for arrays&functions.
	);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier scopes.

template<class unificand> using continue_step = function<current_step(const unificand&,const step&)>;

// Load from reg.
template<class frame_type,class unificand=unificand_type<array<frame_type>>> unificand Load(const array<frame_type>& Frames,const reg& Reg) {
	auto BaseOption=Reg.RegBase.Cast();
	if(!BaseOption)
		VERSE_ERR("Load: Unallocated reg");
	auto Base=BaseOption.Coerce();
	if(Reg.RegDepth!=Max<nat>()) {
		auto FrameUnificands = Frames[Reg.RegDepth]->FrameUnificands.Elements;
		auto i               = Base+Reg.RegOffset;
		if(auto U=FrameUnificands.Get(i))
			return U.Coerce();
		if constexpr(Runs<unificand>)
			return FrameUnificands.GetInit(i);
		else {
			if(Reg.RegOffset || Reg.ArrayElementRegs) { // Create verifier array_vertex & call_vertex on-demand.
				auto ArrayReg = (*Frames[Reg.RegDepth]->FrameRegs.Coerce())[Base];
				auto n        = Length(ArrayReg.ArrayElementRegs);
				auto AS       = MakeArrayUnificand<unificand>(n); // Not in the Frames context as below.
				vertex(AS).SetVertexName(ArrayReg.RegName);
				VERSE_ENSURE(!FrameUnificands.Set(Base,AS));
				for(nat j=0; j<n; j++) {
					auto E=AS->VertexElements[j];
					vertex(E).SetVertexName(ArrayReg.ArrayElementRegs[j].RegName);
					VERSE_ENSURE(!FrameUnificands.Set(Base+1+j,E));
				}
			}
			auto R=FrameUnificands.GetInit(i);
			R.SetVertexName(Reg.RegName);
			return R;
		}
	}
	else if(Base!=Max<nat>())
		return unificand(Base);
	else
		return unificand(False);
}
template<class frame_type> context LoadContext(const array<frame_type>& Frames,const reg& Reg) {
	return Frames[Reg.RegDepth].Context();
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier frames and targets where we resolve syntax and operations.

box<op> MakeNop();

// A redex describes a place where the verifier has reduced a scoped term into an op relating its input and output.
struct redex {
	locus               Locus;
	array<verify_scope> Scopes;
	box<op>             Op;
	reg                 InputReg, OutputReg;

	// Operations.
	vertex LoadOutput() const {
		return Load(Scopes,OutputReg);
	}
	bool FarInput() const;
	box<op> MakeInputIdentityOp() const;
	void Introduce() const {
		Scopes.End(0)->ScopeSymbols->Introduce();
	}

	// Reduction.
	current_step ReduceStep(const step& Step0,const box<op>& ReducedOp=MakeNop(),bool Introduces=true) const;
	template<class t> current_step ReduceStep(const step& Step0,const t& T,bool Introduces=true) const {
		return ReduceStep(Step0,NewAlias(T).Op,Introduces);
	}

	// Creation.
	reg FixedReg(nat FixedRegIndex) const {
		return (*Scopes.End(0)->FrameRegs.Coerce())[FixedRegIndex];
	}
	reg FreshReg(const string& S) const {
		return reg(Length(Scopes)-1,TopContext->Run([]{return future<nat>();}),0,0,S/*ToString(OutputReg,"_",S)*/);
	}
	tuple<array<reg>,array<reg>,box<op>> FreshArrayRegs(reg SpanReg,nat n,const string& S) const;
	template<class t> redex NewAlias(const t& T) const {
		return redex{Locus,Scopes,Op,InputReg,OutputReg}.WithOp(T);
	}
	template<class t> redex FreshRedex(const reg& Input0,const reg& Output0,const t& T) const {
		return redex{Locus,Scopes,MakeNop(),Input0,Output0}.WithOp(T);
	}
	template<class s,class t> requires HasExposeToString<s> redex FreshRedex(const s& S,const t& T) const {
		auto InputReg1  = FreshReg(ToString(S,"_in"));
		auto OutputReg1 = FreshReg(ToString(S,"_out"));
		return FreshRedex(InputReg1,OutputReg1,T);
	}

	// Transform existing redex keeping input and output.
	redex WithLocus(const locus& Locus1) const {
		return redex{Locus1.Else(Locus),Scopes,Op,InputReg,OutputReg};
	}
	redex WithUpdatedScopes(const array<verify_scope>& Scopes1) const {
		// Called solely by EvalStep(op::reduce).
		return redex{Locus,Scopes1,Op,InputReg,OutputReg};
	}
	redex WithEndSliceScopes(nat n) const {
		return redex{Locus,Scopes.EndSlice(n),Op,InputReg,OutputReg};
	}
	redex WithInputScopes() const {
		return redex{Locus,Scopes.Slice(0,InputReg.RegDepth+1),Op,InputReg,OutputReg};
	}

private:
	// Update operation in current redex.
	redex WithOp(const box<op>& Op0) const {
		return redex{Locus,Scopes,Op0,InputReg,OutputReg};
	}
	redex WithOp(const future<box<syntax>>& Syntax) const;
	redex WithOp(const clause& Clause) const;
	redex WithOp(tuple<>) const;
	redex WithOp(const reg& Reg) const;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Coherent op resolution and register allocation across forks.
// Verify ensures ReduceStep is called top-down to resolve and execute each individual redex's ops,
// with ResolveOpBatch merging local ops into the one global tree of ops for the program,
// performing negotiation and allocation, and producing errors upon conflict.

void ResolveOpBatch(const locus& Locus,const future<box<op>>& TargetOp,const box<op>& ResolvedLocalOp) {
	if(auto TargetOp1=TargetOp.Cast(); !TargetOp1)
		TargetOp.ResolveBatch(ResolvedLocalOp,false);
	else if(!TargetOp1->OnResolve(Locus,ResolvedLocalOp)) {
		//Print("Target:"), Print(TargetOp), Print("Local:"), Print(ResolvedLocalOp);
		ErrorBatch(V04(Locus));
	}
}
void ResolveRegBatch(const locus& Locus,const reg& TargetReg,const reg& LocalReg) {
	if(TargetReg.RegOffset!=LocalReg.RegOffset) // Could also check array regs consistency.
		return ErrorBatch(V04(Locus,False));
	LocalReg.RegBase.ResolveBatch(TargetReg.RegBase,false);
}
void ResolveRegsBatch(const locus& Locus,const regs& TargetRegs,const regs& LocalRegs) {
	if(auto N0=LocalRegs.Cast(),N1=TargetRegs.Cast(); N0&&N1) {
		if(Length(*N0.Coerce())!=Length(*N1.Coerce()))
			return ErrorBatch(V04(Locus,False));
	}
	else LocalRegs.ResolveBatch(TargetRegs,false);
}
void ResolveStageSiteBatch(const locus& Locus,const stage_site_future& TargetStageSite,const stage_site_future& LocalStageSite) {
	LocalStageSite.ResolveBatch(TargetStageSite,false);
}
void AllocateRegBatch(const array<verify_scope>& Scopes,const reg& Reg) {
	if(!IsA<nat>(Reg.RegBase)) {
		VERSE_ENSURE(Reg.RegOffset==0); // Ensuring we don't allocate array element regs representing call_vertex.
		auto RegsVar   = Scopes[Reg.RegDepth]->FrameRegs.Coerce();
		nat  Index     = Length(*RegsVar);
		RegsVar       += array{Reg}+Reg.ArrayElementRegs;
		Reg.RegBase.ResolveBatch(Index,false);
	}
}
void AllocateRegsBatch(const array<verify_scope>& Scopes,const regs& TargetRegs,const array<string> FixedRegNames=False,nat DepthOffset=0) {
	if(!TargetRegs.IsA())
		TargetRegs.ResolveBatch(regs(Length(Scopes)+DepthOffset,FixedRegNames),false);
}
void AllocateStageSiteBatch(const stage_site_future& StageSite) {
	static nat i=0;
	if(!StageSite.IsA())
		StageSite.ResolveBatch(++i,false);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Specific opcodes.

template<> struct expose<op::sequence>: default_expose<op::sequence> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/sequence"_VP;}
};
struct op::sequence: op {
	array<box<op>> Ops;
	sequence(const array<box<op>>& Ops0): Ops(Ops0) {}
	template<class... ps> requires requires(ps... PS) {array<box<op>>{PS...};} sequence(const ps&... PS): Ops{PS...} {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalList=LocalOp.Cast<op::sequence>())
			if(auto n=Length(Ops); n==Length(LocalList->Ops)) {
				for(nat i=0; i<n; i++)
					ResolveOpBatch(Locus,Ops[i],LocalList->Ops[i]);
				return true;
			}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		for(auto Op:Ops)
			Op->OnAllocate(Scopes);
	}
};

template<> struct expose<op::atom>: default_expose<op::atom> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/atom"_VP;}
};
struct op::atom: op {
	reg        TargetReg;
	comparable Value;
	atom(const reg& TargetReg0,const comparable& Value0): TargetReg(TargetReg0), Value(Value0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalAtom=LocalOp.Cast<op::atom>())
			if(LocalAtom->Value==Value)
				return true;
		return false;
	}
};

template<> struct expose<op::unify>: default_expose<op::unify> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/unify"_VP;}
};
struct op::unify: op {
	reg TargetReg, SourceReg;
	unify(const reg& TargetReg0,const reg& SourceReg0): TargetReg(TargetReg0), SourceReg(SourceReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalUnify=LocalOp.Cast<op::unify>())
			return true;
		return false;
	}
};

template<> struct expose<op::span>: default_expose<op::span> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/span"_VP;}
};
struct op::span: op {
	reg TargetReg, ArrayReg;
	nat Length;
	span(const reg& TargetReg0,const reg& ArrayReg0,nat Length0): TargetReg(TargetReg0), ArrayReg(ArrayReg0), Length(Length0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalSpan=LocalOp.Cast<op::span>())
			if(LocalSpan->Length==Length)
				return true;
		return false;
	}
};

template<> struct expose<op::fail>: default_expose<op::fail> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/fail"_VP;}
};
struct op::fail: op {
	reg TargetReg; // Only matters if 0-ary.
	fail(const reg& TargetReg0): TargetReg(TargetReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalFail=LocalOp.Cast<op::fail>())
			return true;
		return false;
	}
};

template<> struct expose<op::choice>: default_expose<op::choice> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/choice"_VP;}
};
struct op::choice: op {
	box<op> Op0, Op1;
	choice(const box<op>& Op0_0,const box<op>& Op1_0): Op0(Op0_0), Op1(Op1_0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalChoice=LocalOp.Cast<op::choice>()) {
			ResolveOpBatch(Locus,Op0,LocalChoice->Op0);
			ResolveOpBatch(Locus,Op1,LocalChoice->Op1);
			return true;
		}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		Op0->OnAllocate(Scopes);
		Op1->OnAllocate(Scopes);
	}
};

template<> struct expose<op::range>: default_expose<op::range> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/range"_VP;}
};
struct op::range: op {
	reg TargetReg, FirstReg, LastReg;
	range(const reg& TargetReg0,const reg& FirstReg0,const reg& LastReg0):
		TargetReg(TargetReg0), FirstReg(FirstReg0), LastReg(LastReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalRange=LocalOp.Cast<op::range>())
			if(LocalRange->TargetReg==TargetReg && LocalRange->FirstReg==FirstReg && LocalRange->LastReg==LastReg)
				return true;
		return false;
	}
};

enum class function_specifiers:nat8 {None=0,Closed=1,Open=2};
VERSE_ENUM_LATTICE_OPEN(function_specifiers,nat8);

template<> struct expose<op::lambda>: default_expose<op::lambda> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/lambda"_VP;}
};
struct op::lambda: op {
	fx          LambdaAllowFx;
	reg         OutputReg, InputReg, LambdaInputReg;
	bool        Invariant;
	regs        ParameterRegs, DomainRegs, ComposeRegs, RangeRegs;
	box<op>     DomainOp, RangeOp;
	lambda(fx LambdaAllowFx0,const reg& OutputReg0,const reg& InputReg0,const reg& LambdaInputReg0,bool Invariant0,const box<op>& DomainOp0,const box<op>& RangeOp0):
		LambdaAllowFx(LambdaAllowFx0), 
		OutputReg(OutputReg0), InputReg(InputReg0), LambdaInputReg(LambdaInputReg0),
		Invariant(Invariant0), DomainOp(DomainOp0), RangeOp(RangeOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalLambda=LocalOp.Cast<op::lambda>())
			if(LocalLambda->LambdaAllowFx==LambdaAllowFx)
				return
					// Don't resolve DomainOp and RangeOp, which are out-of-existential copies of CheckOp.
					ResolveRegsBatch(Locus,ParameterRegs,LocalLambda->ParameterRegs),
					ResolveRegsBatch(Locus,DomainRegs,LocalLambda->DomainRegs),
					ResolveRegsBatch(Locus,ComposeRegs,LocalLambda->ComposeRegs),
					ResolveRegsBatch(Locus,RangeRegs,LocalLambda->RangeRegs),
					ResolveOpBatch(Locus,DomainOp,LocalLambda->DomainOp),
					ResolveOpBatch(Locus,RangeOp,LocalLambda->RangeOp), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		// Don't allocate DomainOp and RangeOp for the reason above.
		DomainOp->OnAllocate(Scopes);
		RangeOp->OnAllocate(Scopes);
		AllocateRegsBatch(Scopes,ParameterRegs,array<string>{"DomainInput"_VS} ,0);
		AllocateRegsBatch(Scopes,DomainRegs   ,array<string>{"DomainOutput"_VS},1);
		AllocateRegsBatch(Scopes,ComposeRegs  ,array<string>{}                 ,2);
		AllocateRegsBatch(Scopes,RangeRegs    ,array<string>{"RangeOutput"_VS} ,3);
	}
};

template<> struct expose<op::call>: default_expose<op::call> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/call"_VP;}
};
struct op::call: op {
	reg         ResultReg, FunctionReg, ParameterReg;
	option<reg> IgnoreSelfReg;
	call(reg ResultReg0,const reg& FunctionReg0,const reg& ParameterReg0,option<reg> IgnoreSelfReg0):
		ResultReg(ResultReg0), FunctionReg(FunctionReg0), ParameterReg(ParameterReg0), IgnoreSelfReg(IgnoreSelfReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalCall=LocalOp.Cast<op::call>())
			if(bool(LocalCall->IgnoreSelfReg)==bool(IgnoreSelfReg))
				return true;
		return false;
	}
};

template<> struct expose<op::enter>: default_expose<op::enter> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/enter"_VP;}
};
struct op::enter: op {
	reg     InputReg;
	box<op> EnterOp;
	enter(const reg& InputReg0,const box<op>& EnterOp0):
		InputReg(InputReg0), EnterOp(EnterOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalEnter=LocalOp.Cast<op::enter>()) {
			ResolveOpBatch(Locus,EnterOp,LocalEnter->EnterOp);
			return true;
		}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		EnterOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::hold>: default_expose<op::hold> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/hold"_VP;}
};
struct op::hold: op {
	reg InputReg, OutputReg, AbstractionReg;
	hold(const reg& InputReg0,const reg& OutputReg0,const reg& AbstractionReg0):
		InputReg(InputReg0), OutputReg(OutputReg0), AbstractionReg(AbstractionReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalHold=LocalOp.Cast<op::hold>())
			return InputReg==LocalHold->InputReg && OutputReg==LocalHold->OutputReg && AbstractionReg==LocalHold->AbstractionReg;
		return false;
	}
};

template<> struct expose<op::stage>: default_expose<op::stage> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/stage"_VP;}
};
struct op::stage: op {
	stage_site_future StageSite;
	reg               InputReg, OutputReg, AbstractionReg;
	regs              ComposeRegs, StageRegs;
	box<op>           StageAbstractionOp, StageValueOp;
	stage(const stage_site_future& StageSite0,const reg& InputReg0,const reg& OutputReg0,const reg& AbstractionReg0,const box<op>& StageAbstractionOp0,const box<op>& StageValueOp0):
		StageSite(StageSite0),
		InputReg(InputReg0), OutputReg(OutputReg0), AbstractionReg(AbstractionReg0), StageAbstractionOp(StageAbstractionOp0), StageValueOp(StageValueOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalStage=LocalOp.Cast<op::stage>()) {
			ResolveRegsBatch(Locus,ComposeRegs,LocalStage->ComposeRegs);
			ResolveRegsBatch(Locus,StageRegs,LocalStage->StageRegs);
			ResolveOpBatch(Locus,StageAbstractionOp,LocalStage->StageAbstractionOp);
			ResolveOpBatch(Locus,StageValueOp,LocalStage->StageValueOp);
			ResolveStageSiteBatch(Locus,StageSite,LocalStage->StageSite);
			return true;
		}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		AllocateRegsBatch(Scopes,ComposeRegs,array<string>{"StageDoInputReg"_VS},0);
		AllocateRegsBatch(Scopes,StageRegs,array{"StageCheckValue"_VS,"StageCheckResult"_VS,"StageCheckFunction"_VS},1);
		AllocateStageSiteBatch(StageSite);
		StageAbstractionOp->OnAllocate(Scopes);
		StageValueOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::check>: default_expose<op::check> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/check"_VP;}
};
struct op::check: op {
	fx            CheckAllowFxMask;
	option<error> CheckError;
	reg           OutputReg;
	regs          CheckRegs;
	box<op>       CheckOp;
	check(fx CheckAllowFxMask0,const option<error> CheckError0,const reg& OutputReg0,const box<op>& CheckOp0):
		CheckAllowFxMask(CheckAllowFxMask0), CheckError(CheckError0), OutputReg(OutputReg0), CheckOp(CheckOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalCheck=LocalOp.Cast<op::check>())
			if(LocalCheck->CheckAllowFxMask==CheckAllowFxMask && LocalCheck->CheckError==CheckError) {
				ResolveRegsBatch(Locus,CheckRegs,LocalCheck->CheckRegs);
				ResolveOpBatch(Locus,CheckOp,LocalCheck->CheckOp);
				return true;
			}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		AllocateRegsBatch(Scopes,CheckRegs,array<string>{"CheckOutput"_VS});
		CheckOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::assume>: default_expose<op::assume> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/assume"_VP;}
};
struct op::assume: op {
	fx      AssumeFx;
	reg     OutputReg;
	regs    ParameterRegs, AssumeRegs;
	box<op> AssumeOp;
	assume(fx AssumeFx0,const reg& OutputReg0,const box<op>& AssumeOp0):
		AssumeFx(AssumeFx0), OutputReg(OutputReg0), AssumeOp(AssumeOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalAssume=LocalOp.Cast<op::assume>())
			if(LocalAssume->AssumeFx==AssumeFx) {
				ResolveRegsBatch(Locus,ParameterRegs,LocalAssume->ParameterRegs);
				ResolveRegsBatch(Locus,AssumeRegs,LocalAssume->AssumeRegs);
				ResolveOpBatch(Locus,AssumeOp,AssumeOp);
				return true;
			}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		AllocateRegsBatch(Scopes,ParameterRegs,array<string>{"AssumeInput"_VS},0);
		AllocateRegsBatch(Scopes,AssumeRegs,array<string>{"AssumeOutput"_VS},1);
		AssumeOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::iterate>: default_expose<op::iterate> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/iterate"_VP;}
};
struct op::iterate: op {
	reg       SourceReg, InputReg;
	regs      DomainRegs, SucceedsComposeRegs, SucceedsRegs, FailsRegs;
	box<op>   DomainOp, SucceedsOp, FailsOp;
	iterate(const reg& SourceReg0,const reg& InputReg0,const box<op>& DomainOp0,const box<op>& SucceedsOp0,const box<op>& FailsOp0):
		SourceReg(SourceReg0), InputReg(InputReg0), DomainOp(DomainOp0), SucceedsOp(SucceedsOp0), FailsOp(FailsOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalIterate=LocalOp.Cast<op::iterate>())
			return
				ResolveRegsBatch(Locus,DomainRegs,LocalIterate->DomainRegs),
				ResolveRegsBatch(Locus,SucceedsComposeRegs,LocalIterate->SucceedsComposeRegs),
				ResolveRegsBatch(Locus,SucceedsRegs,LocalIterate->SucceedsRegs),
				ResolveRegsBatch(Locus,FailsRegs,LocalIterate->FailsRegs),
				ResolveOpBatch(Locus,DomainOp,LocalIterate->DomainOp),
				ResolveOpBatch(Locus,SucceedsOp,LocalIterate->SucceedsOp),
				ResolveOpBatch(Locus,FailsOp,LocalIterate->FailsOp), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		AllocateRegsBatch(Scopes,DomainRegs,array<string>{"DomainOutput"_VS});
		AllocateRegsBatch(Scopes,SucceedsComposeRegs,False,1);
		AllocateRegsBatch(Scopes,SucceedsRegs,array{"SucceedsCurrentValue"_VS,"SucceedsNextValues"_VS},2);
		AllocateRegsBatch(Scopes,FailsRegs,array<string>{"FailCurrentValue"_VS});
		DomainOp->OnAllocate(Scopes);
		SucceedsOp->OnAllocate(Scopes);
		FailsOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::scope>: default_expose<op::scope> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/scope"_VP;}
};
struct op::scope: op {
	fx      ExpectFx;
	regs    ScopeRegs;
	box<op> Body;
	scope(fx ExpectFx0,const box<op>& Body0): ExpectFx(ExpectFx0), Body(Body0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalScope=LocalOp.Cast<op::scope>())
			if(LocalScope->ExpectFx==ExpectFx)
				return
					ResolveRegsBatch(Locus,ScopeRegs,LocalScope->ScopeRegs),
					ResolveOpBatch(Locus,Body,LocalScope->Body), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		Body->OnAllocate(Scopes);
		AllocateRegsBatch(Scopes,ScopeRegs);
	}
};

template<> struct expose<op::length>: default_expose<op::length> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/length"_VP;}
};
struct op::length: op {
	reg ArrayReg,LengthReg;
	length(const reg& ArrayReg0,const reg& LengthReg0):
		ArrayReg(ArrayReg0), LengthReg(LengthReg0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalLength=LocalOp.Cast<op::length>())
			return true;
		return false;
	}
};

template<> struct expose<op::beta>: default_expose<op::beta> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/beta"_VP;}
};
struct op::beta: op {
	reg ComposeOutputReg;
	regs BetaComposeRegs;
	box<op> BetaComposeOp, BetaRangeOp;
	beta(const reg& ComposeOutputReg0,const box<op>& BetaComposeOp0,const box<op>& BetaRangeOp0): 
		ComposeOutputReg(ComposeOutputReg0), BetaComposeOp(BetaComposeOp0), BetaRangeOp(BetaRangeOp0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalBeta=LocalOp.Cast<op::beta>())
			return
				ResolveRegsBatch(Locus,BetaComposeRegs,LocalBeta->BetaComposeRegs),
				ResolveOpBatch(Locus,BetaComposeOp,LocalBeta->BetaComposeOp),
				ResolveOpBatch(Locus,BetaRangeOp,LocalBeta->BetaRangeOp), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		AllocateRegsBatch(Scopes,BetaComposeRegs);
		BetaComposeOp->OnAllocate(Scopes);
		BetaRangeOp->OnAllocate(Scopes);
	}
};

template<> struct expose<op::exists>: default_expose<op::exists> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/exists"_VP;}
};
struct op::exists: op {
	array<reg> DeclareRegs;
	exists(const array<reg>& DeclareRegs0): DeclareRegs(DeclareRegs0) {}
	template<class... ts> exists(const ts&... TS) {
		(Add(TS),...);
	}
	void Add(const reg& Reg) {
		DeclareRegs+=array{Reg};
	}
	void Add(const array<reg>& Regs1) {
		DeclareRegs+=Regs1;
	}
	void Add(const redex& Redex) {
		DeclareRegs+=array{Redex.InputReg,Redex.OutputReg};
	}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalDeclare=LocalOp.Cast<op::exists>())
			if(auto RegsCount=Length(DeclareRegs); RegsCount==Length(LocalDeclare->DeclareRegs)) {
				for(nat i=0; i<RegsCount; i++)
					ResolveRegBatch(Locus,DeclareRegs[i],LocalDeclare->DeclareRegs[i]);
				return true;
			}
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		for(auto DeclareReg:DeclareRegs)
			AllocateRegBatch(Scopes,DeclareReg);
	}
};

template<> struct expose<op::conditional>: default_expose<op::conditional> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/conditional"_VP;}
};
struct op::conditional: op {
	bool    IfVerify,IfBeta,IfRun;
	box<op> Op;
	conditional(bool IfVerify0,bool IfBeta0,bool IfRun0,const box<op>& Op0):
		IfVerify(IfVerify0), IfBeta(IfBeta0), IfRun(IfRun0), Op(Op0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalConditional=LocalOp.Cast<op::conditional>())
			if(LocalConditional->IfVerify==IfVerify && LocalConditional->IfBeta==IfBeta && LocalConditional->IfRun==IfRun)
				return ResolveOpBatch(Locus,Op,LocalConditional->Op), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		Op->OnAllocate(Scopes);
	}
};

template<> struct expose<op::print>: default_expose<op::print> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/print"_VP;}
};
struct op::print: op {
	array<comparable> Things; // Currently supports string and reg.
	template<class... ts> print(const ts&... Things0): Things(Things0...) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalPrint=LocalOp.Cast<op::print>())
			if(Things==LocalPrint->Things)
				return true;
		return false;
	}
};

template<> struct expose<op::test>: default_expose<op::test> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/test"_VP;}
};
struct op::test: op {
	locus     Locus;
	string    ExpectedErrorCode;
	regs      TestRegs;
	box<op>   Body;
	test(const locus& Locus0,const string& ExpectedErrorCode0,const box<op>& Body0):
		Locus(Locus0), ExpectedErrorCode(ExpectedErrorCode0), Body(Body0) {}
	bool OnResolve(const locus& Locus1,const box<op>& LocalOp) const override {
		if(auto LocalTest=LocalOp.Cast<op::test>())
			if(LocalTest->ExpectedErrorCode==ExpectedErrorCode)
				return 
					ResolveRegsBatch(Locus1,TestRegs,LocalTest->TestRegs),
					ResolveOpBatch(Locus1.Else(Locus),Body,LocalTest->Body), true;
		return false;
	}
	void OnAllocate(const array<verify_scope>& Scopes) const override {
		Body->OnAllocate(Scopes);
		AllocateRegsBatch(Scopes,TestRegs);
	}
};

template<> struct expose<op::reduce>: default_expose<op::reduce> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/reduce"_VP;}
};
struct op::reduce: op {
	locus                    Locus;
	nat                      DepthChange;
	future<box<op>>          FutureOp;
	function<string()>       Describe;
	redex                    ReduceRedex;
	function<current_step(const redex&,const step&)> OnVerifyStep;
	reduce(const redex& ReduceRedex0,nat DepthChange0,const function<string()>& Describe0,const function<current_step(const redex&,const step&)>& OnVerifyStep0);
	bool OnResolve(const locus& Locus1,const box<op>& LocalOp) const override {
		if(auto LocalReduce=LocalOp.Cast<op::reduce>()) { // We could store and compare syntax.
			if(!LocalReduce->FutureOp.Cast()) // Called before we've resolved LocalReduce->GlobalOp, so only happens if shared.
				LocalReduce->FutureOp.ResolveBatch(FutureOp,false); // Negotiate upon ReduceStep.
			else VERSE_ENSURE(FutureOp.IsA());
			return true;
		}
		return false;
	}
	// OnAllocate inherits trivial implementation, because we handle it in ReduceStep.
};

template<> struct expose<op::stuck>: default_expose<op::stuck> {
	path ExposeStaticSignature() {return "/Verse.org/runtime/op/stuck"_VP;}
};
struct op::stuck: op {
	fx    Fx;
	error StuckError;
	stuck(fx Fx0,const error& StuckError0): Fx(Fx0), StuckError(StuckError0) {}
	bool OnResolve(const locus& Locus,const box<op>& LocalOp) const override {
		if(auto LocalTest=LocalOp.Cast<op::stuck>())
			if(LocalTest->Fx==Fx && LocalTest->StuckError==StuckError)
				return true;
		return false;
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Op printing.

// Opcode printing.
string ToStringOps(const box<op>& Op,const string& Indent=False,bool Nest=true) {
	if(auto Sequence=Op.Cast<op::sequence>()) {
		string rs=False;
		if(Nest)
			for(auto Op1:Sequence->Ops)
				rs+=ToStringOps(Op1,Indent);
		else rs="sequence"_VS;
		return rs;
	}
	if(auto Atom=Op.Cast<op::atom>())
		return ToString(Indent,Atom->TargetReg,"=",ToCode(Atom->Value),Nest? "\n": "");
	if(auto Unify=Op.Cast<op::unify>())
		return ToString(Indent,Unify->TargetReg,"=",Unify->SourceReg,Nest? "\n": "");
	if(auto Span=Op.Cast<op::span>())
		return ToString(Indent,"length(",Span->ArrayReg,",",Span->Length,")",Nest? "\n": "");
	if(auto Declare=Op.Cast<op::exists>())
		return ToString(Indent,"exists ",ToStringSeparated(Declare->DeclareRegs," "_VS),Nest? "\n": "");
	if(auto Fail=Op.Cast<op::fail>())
		return ToString(Fail->TargetReg,"=fail");
	if(auto Choice=Op.Cast<op::choice>()) {
		if(!Nest)
			return ToString(Indent,"choice");
		string S=False;
		S+=ToString(
			Indent,"choice 0:\n",
			ToStringOps(Choice->Op0,Indent+"|   ")
		);
		S+=ToString(
			Indent,"choice 1:\n",
			ToStringOps(Choice->Op1,Indent+"|   ")
		);
		return S;
	}
	if(auto Range=Op.Cast<op::range>())
		return ToString(Indent,"range ",Range->TargetReg,",",Range->FirstReg,",",Range->LastReg);
	if(auto Lambda=Op.Cast<op::lambda>())
		return ToString(
			Indent,"lambda",Lambda->Invariant? "<closed>": "",Lambda->LambdaAllowFx," ",Lambda->OutputReg,",",Lambda->InputReg,",",Lambda->LambdaInputReg,Nest? ToString(":\n",
			Indent,"|   domain:\n",ToStringOps(Lambda->DomainOp,Indent+"|   |   "),
			Indent,"|   range:\n", ToStringOps(Lambda->RangeOp ,Indent+"|   |   ")
		): False);
	if(auto Call=Op.Cast<op::call>())
		return ToString(Indent,Call->ResultReg,"=",Call->FunctionReg,"(",Call->ParameterReg,")",Call->IgnoreSelfReg? ToString(" ignore ",Call->IgnoreSelfReg.Coerce()): False,Nest? "\n": "");
	if(auto Enter=Op.Cast<op::enter>())
		return ToString(
			Indent,"Enter ",Enter->InputReg,Nest? ToString(":\n",
				ToStringOps(Enter->EnterOp,Indent+"|   ")
			): False);
	if(auto In=Op.Cast<op::hold>())
		return ToString(
			Indent,"hold ",",",In->InputReg,",",In->OutputReg,",",In->AbstractionReg);
	if(auto Stage=Op.Cast<op::stage>())
		return ToString(
			Indent,"stage ",Stage->InputReg,",",Stage->OutputReg,",",Stage->AbstractionReg,Nest? ToString(":\n",
				Indent,"|   abstraction:\n", ToStringOps(Stage->StageAbstractionOp ,Indent+"|   |   "),
				Indent,"|   value:\n",ToStringOps(Stage->StageValueOp,Indent+"|   |   ")
			): False);
	if(auto Check=Op.Cast<op::check>())
		return ToString(
			Indent,"check(",Check->CheckAllowFxMask,Check->CheckError? ToString(",",Check->CheckError->ErrorCode): False,") ",Check->OutputReg,Nest? ToString(":\n",
			    Indent,"|   body:\n", ToStringOps(Check->CheckOp,Indent+"|   |   ")
			): False);
	if(auto Assume=Op.Cast<op::assume>())
		return ToString(
			Indent,"assume(",Assume->AssumeFx,") ",Assume->OutputReg,Nest? ToString(":\n",
				Indent,"|   body:\n", ToStringOps(Assume->AssumeOp,Indent+"|   |   ")
			): False);
	if(auto Beta=Op.Cast<op::beta>())
		return ToString(Indent,"beta ",Beta->ComposeOutputReg," ",Nest? ToString(":\n",
			Indent,"|   compose:\n", ToStringOps(Beta->BetaComposeOp,Indent+"|   |   "),
			Indent,"|   range:\n", ToStringOps(Beta->BetaRangeOp,Indent+"|   |   ")
		): False);
	if(auto Iterate=Op.Cast<op::iterate>())
		return ToString(
			Indent,"iterate "," ",Iterate->SourceReg," ",Iterate->InputReg,Nest? ToString(":\n",
			Indent,"|   domain:\n", ToStringOps(Iterate->DomainOp,Indent  +"|   |   "),
			Indent,"|   succeed:\n",ToStringOps(Iterate->SucceedsOp,Indent+"|   |   "),
			Indent,"|   fail:\n",   ToStringOps(Iterate->FailsOp,Indent   +"|   |   ")
		): False);
	if(auto Scope=Op.Cast<op::scope>())
		return ToString(
			Indent,Scope->ExpectFx!=effects? ToString("expect(",Scope->ExpectFx,") "): ToString("scope "),
			Nest? ToString(":\n",ToStringOps(Scope->Body,Indent+"|   ")): False
		);
	if(auto Length=Op.Cast<op::length>())
		return ToString(Indent,"length ",Length->ArrayReg,",",Length->LengthReg,Nest? "\n": "");
	if(auto Conditional=Op.Cast<op::conditional>())
		return ToString(
			Indent,"conditional",
			Conditional->IfVerify? "-verify": "",
			Conditional->IfBeta?   "-beta":   "",
			Conditional->IfRun?    "-run":    "",
			Nest? ToString(":\n",ToStringOps(Conditional->Op,Indent+"|   ")): False);
	if(auto Log=Op.Cast<op::print>())
		return ToString(Indent,"print \"",Log->Things,Nest? "\n": "");
	if(auto Test=Op.Cast<op::test>())
		return ToString(
			Indent,"test ",Test->ExpectedErrorCode,
			Nest? ToString(":\n",ToStringOps(Test->Body,Indent+"|   ")): False
		);
	if(auto Reduce=Op.Cast<op::reduce>()) {
		auto Op1=Reduce->FutureOp.Cast();
		return ToString(
			Indent,"reduce ",Reduce->Describe(),
			Nest? Op1? ToString(":\n",ToStringOps(Op1.Coerce(),Indent+"|   ")): ToString("...\n"): False
		);
	}
	if(auto Stuck=Op.Cast<op::stuck>())
		return ToString(Indent,"stuck ",Stuck->Fx,": ",Stuck->StuckError);
	VERSE_UNEXPECTED;
}
string ExposeToString(const box<op>& Op) {
	return ToStringOps(Op,False,false);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Partial evaluator with continuation passing, ensures any step can fork in eval.

template<class unificand=future<>> result<tuple<>,error> Eval(const locus& Locus,const string& What,const function<current_step(const step&)>& Body,const string& ExpectFirstError="S00"_VS);
template<class unificand> current_step NarrowAbstractionFx(const locus& Locus,const string& What,const unificand& AbstractionUnificand,const unificand& OutputUnificand,const var<fx>& AbstractionFx,const step& Step0) {
	if constexpr(Runs<unificand>)
		return Step0;
	else
		return WhenCastStep<box<abstraction_vertex>>(
			[=]{return C00(Locus,What);},
			only_succeeds,AbstractionUnificand,Truth(OutputUnificand),Step0,
			[=](const box<abstraction_vertex>& AbstractionVertex,const step& Step2)->current_step {
				AbstractionFx &= AbstractionVertex->AbstractionKeepFx + AbstractionVertex->AbstractionAddFx;
				return Step2;
			},false,true);
};
template<class frame_type> current_step EvalStep(const locus& Locus,const array<frame_type>& Frames,const box<op>& Op,const step& Step0) {
	// This implements (OpIntro) and leads into all other op rules by case decomposition.
	using unificand           = unificand_type<array<frame_type>>;
	constexpr auto VerifyHere = IsEqual<frame_type,verify_scope>; 
	if(VerboseEval && (VerifyHere || !Op.Cast<op::reduce>()))
		Print("EvalStep: ",Op);
	if(auto Declare=Op.Cast<op::exists>()) // Rule (ExistsIntro).
		return Step0;
	if(auto Sequence=Op.Cast<op::sequence>()) // Rule (SequenceIntro).
		return ForStep(Length(Sequence->Ops),Step0,[=](nat i,const step& Step1)->current_step {
			return EvalStep(Locus,Frames,Sequence->Ops[i],Step1);
		});
	if(auto Scope=Op.Cast<op::scope>()) { // Rule (ScopeIntro).
		auto ScopeContext = MakeContext<unificand>(
			HereContext(Locus,"Eval-scope"_VS),Scope->ExpectFx&Thread->AllowFx,
			false,GetAssumedFxHere(),
			[=](fx NewChildFx) {
				// For expect<fx>{..}, checks here, else only at iteration contexts.
				return NewChildFx + (NewChildFx<=Scope->ExpectFx? no_effects: only_rejects);
			}
		);
		return ScopeContext->RunStep(Step0,[=](const step& Step1)->current_step {
			auto ScopeScopes = SetContextFrames(Frames+array{frame_type(FrameSigil,Scope->ScopeRegs,FrameAllowFx(Frames),False)});
			return EvalStep(Locus,ScopeScopes,Scope->Body,Step1);
		});
	}
	if(auto Reduce=Op.Cast<op::reduce>()) { // Rule (ResolvedIntro).
		if constexpr(!VerifyHere)
			return WhenResolveStep(VERIFIER_HERE("Eval-future"),effects,Reduce->FutureOp,Step0,[=](const box<op>& ResolvedOp,const step& Step1)->current_step {
				return EvalStep(Reduce->Locus.Else(Locus),Frames,ResolvedOp,Step1);
			});
	}
	if(auto Reduce=Op.Cast<op::reduce>()) { // Implementation machinery supporting many inference rules.
		if constexpr(VerifyHere) {
			VERSE_ENSURE(Length(Frames)-Length(Reduce->ReduceRedex.Scopes)==Reduce->DepthChange);
			return Reduce->OnVerifyStep(Reduce->ReduceRedex.WithUpdatedScopes(Frames),Step0);
		}
	}
	if(auto Atom=Op.Cast<op::atom>()) // Rule (AtomIntro).
		return UnifyStep("Eval-atom",Locus,Load(Frames,Atom->TargetReg),unificand(Atom->Value),Step0);
	if(auto Unify=Op.Cast<op::unify>()) { // Parts of (UnifyIntro), (VariableIntro), (ProductIntro).
		auto TargetUnificand = Load(Frames,Unify->TargetReg);
		auto SourceUnificand = Load(Frames,Unify->SourceReg);
		return UnifyStep("Eval-unify",Locus,TargetUnificand,SourceUnificand,Step0);
	}
	if(auto Span=Op.Cast<op::span>()) { // Part of rule (ProductIntro).
		auto TargetUnificand = Load(Frames,Span->TargetReg);
		if constexpr(Runs<unificand>) {
			auto ArrayReg = Span->ArrayReg;
			auto Base     = ArrayReg.CoerceIndex();
			auto Elements = Frames[ArrayReg.RegDepth]->FrameUnificands.Elements;
			auto AS       = For(Span->Length,[&](nat i) {return Elements.GetInit(Base+i+1);});
			VERSE_ENSURE(!Elements.Set(Base,AS));
		}
		return UnifyStep("Eval-span",Locus,TargetUnificand,Load(Frames,Span->ArrayReg),Step0);
	}
	if(auto Fail=Op.Cast<op::fail>()) // Rule (FailIntro).
		return FailTargetStep("Eval-fail",Locus,Load(Frames,Fail->TargetReg),Step0);
	if(auto Call=Op.Cast<op::call>()) {
		// Rule (CallProductIntro).
		// May lead to (CallProductSucceeds), (CallProductFails), (CallProductIterates), (CallClosedIntro).
		auto ResultUnificand    = Load(Frames,Call->ResultReg);
		auto FunctionUnificand  = Load(Frames,Call->FunctionReg);
		auto ParameterUnificand = Load(Frames,Call->ParameterReg);
		auto IgnoreUnificand    = Call->IgnoreSelfReg? Truth(Load(Frames,Call->IgnoreSelfReg.Coerce())): False;
		return EvalCallStep(Locus,ResultUnificand,FunctionUnificand,ParameterUnificand,IgnoreUnificand,Step0);
	}
	if(auto Iterate=Op.Cast<op::iterate>()) { // Rule (IterateIntro).
		auto DomainLambdaAllowFx = AllowIteratesFx(FrameAllowFx(Frames));
		auto Initial             = Load(Frames,Iterate->SourceReg);
		auto State               = [&]{if constexpr(Runs<unificand>) return False; else return
			verifier_iterate_state{GetAssumedFxConst(effects),visibility::Iterate,
				[=](fx NewChildFx){
					auto VI                          = Thread.Coerce<verifier_iterate>();
					auto Ready                       = VI->CheckIterateReady(false);
					auto[Releaseable,NewSuspendedFx] = VI->RefineIterateSuspendedFx(NewChildFx&AssumedFxHere(VI)&no_unifies,false);
					if(Releaseable && !VI->IsReleased || Ready.AnyReady())
						VI->ReadySuspensionBatch();
					return NewSuspendedFx;
				}
			};
		}();
		return IterateStep<if_type<Runs<unificand>,iterate_managed,verifier_iterate_managed>>(
			[=](const context& C) {
				return R00(Locus,"Eval-iterate"_VS);
			},
			State,Initial,true,DomainLambdaAllowFx,effects,Step0,
			[=]{
				return SetContextFrames(Frames+array{frame_type(FrameSigil,Iterate->DomainRegs,DomainLambdaAllowFx)});
			},
			[=](const array<frame_type>& DomainFrames)->current_step {
				return EvalStep(Locus,DomainFrames,Iterate->DomainOp,LeaveIterateStep);
			},
			[=](const array<frame_type>& DomainFrames,const unificand& CurrentValue,const continue_step<unificand>& NextStep,const step& Step1)->current_step {
				// Rule (IterateSucceedsElim).
				if(VerboseSplitImply) Print("iterate succeeds");
				unificand NextValues;
				auto InputContext   = LoadContext(Frames,Iterate->InputReg);
				auto ComposeFrames  = InputContext->Run([&]{
					return DomainFrames + array{frame_type(FrameSigil,Iterate->SucceedsComposeRegs,FrameAllowFx(Frames))};
				});
				auto RangeFrames    = ComposeFrames + array{frame_type(FrameSigil,Iterate->SucceedsRegs,FrameAllowFx(Frames),future_array<unificand>{CurrentValue,NextValues})};
				return EvalStep(Locus,RangeFrames,Iterate->SucceedsOp,[=]()->current_step {
					// Rule (IterateSucceedsElim): enqueue potential next iteration subsequent to this one.
					unificand TempFailTarget; // Consider whether 'iterate' op should have a FailTarget.
					return WhenCastStep<array<unificand>>(
						VERIFIER_HERE("Eval-iterate-cond"),effects,NextValues,Truth(TempFailTarget),Step1,
						[=](const array<unificand>& US,const step& Step2)->current_step {
							//!!consider paraconsistency further. Should uninhabited next-value hide range isses?
							return US? NextStep(US[0],Step2): current_step(Step2);
						},
						false,true
					);
				});
			},
			[=](const unificand& Current,const step& Step1)->current_step {
				// Rule (IterateFailsElim).
				if(VerboseSplitImply) Print("iterate fails");
				auto FailContext=MakeContext<unificand>(
					HereContext(Locus,"iterate-fails-context"_VS),Thread->AllowFx,
					false,GetAssumedFxHere(),DefaultGetSuspendedFx);
				return FailContext->RunStep(Step1,[=](const step& Step2)->current_step {
					auto FailFrames = SetContextFrames(Frames+array{frame_type(FrameSigil,Iterate->FailsRegs,FrameAllowFx(Frames),future_array<unificand>{Current})});
					return EvalStep(Locus,FailFrames,Iterate->FailsOp,Step2);
				});
			}
		);
	}
	if(auto Choice=Op.Cast<op::choice>()) // Rules (ChoiceSolveElim), (ChoiceImplyElim).
		return ForForkStep(Locus,!IsImplying()? Truth(effects): False,0,2,Step0,[=](nat i,const step& Step1)->current_step {
			//Print("CHOICE ",i);
			return EvalStep(Locus,Frames,i==0? Choice->Op0: Choice->Op1,Step1);
		});
	if(auto Lambda=Op.Cast<op::lambda>()) { // Rules (LambdaIntro), (LambdaDown), (BetaUpDown).
		auto TargetUnificand  = Load(Frames,Lambda->OutputReg);
		auto InputUnificand   = Load(Frames,Lambda->InputReg);
		auto InputContext     = LoadContext(Frames,Lambda->InputReg);
		auto InputInflexible  = !Runs<unificand> && FlexibleStart(InputContext)!=FlexibleStart();
		auto DomainAssumeFx   = only_succeeds+(contradicts&function_allows&Lambda->LambdaAllowFx);
		auto InitBetaFxFrames = [=]<class frame>(const array<frame>& Frames,const auto& ParameterContext,const auto& DomainContext,const auto& RangeContext,const unificand& Result,const unificand& Parameter) {
			auto ComposeContext = MakeContext<unificand>(HereContext(Locus,"op.beta-compose"_VS),Thread->AllowFx,
				InputInflexible,GetAssumedFxConst(effects),DefaultGetSuspendedFx); // Need for runtime so we can abandon.
			if constexpr(!Runs<unificand>) {
				ComposeContext->GetAssumedFx = [=,BetaOpContext=Thread]{
					auto AssumedFx = AssumedFxHere(BetaOpContext);
					if(InputInflexible) {
						auto AbstractingFx  = ComposeContext->CompletedChildAbstractingFx&no_unifies;
						auto InputStrength  = only_cardinalities&abstracts&Coerce<box<verifier_context_state>>(InputContext)->GetAssumedFx();
						auto DomainStrength = only_cardinalities&abstracts&DomainContext->SuspendedFx;
						return (abstracts&AssumedFx&AbstractingFx) + InputStrength + DomainStrength;
					}
					else return AssumedFx;
				};
				ComposeContext->GetSuspendedFx=[=,BetaOpContext=Thread](fx NewChildFx) { // GetSuspendedFx.
					auto VCS           = Coerce<box<verifier_context_state>>(Thread);
					VCS->AbstractingFx = only_succeeds;
					if(InputInflexible) // Without +=, what causes :Print=>3 rejection?
						NewChildFx     = ComposeContext->GetAssumedFx();
					return DefaultGetSuspendedFx(NewChildFx);
				};
			}
			SetContextFrames(Frames);
			auto ParameterFrames = ParameterContext->Run([&]{
				return SetContextFrames(Frames+array{frame(FrameSigil,Lambda->ParameterRegs,DomainAssumeFx,future_array<unificand>(Parameter))});
			});
			auto DomainFrames = DomainContext->Run([&]{
				return SetContextFrames(ParameterFrames+array{frame(FrameSigil,Lambda->DomainRegs,DomainAssumeFx)});
			});
			auto ComposeFrames = ComposeContext->Run([&]{
				return SetContextFrames(DomainFrames+array{frame(FrameSigil,Lambda->ComposeRegs,Lambda->LambdaAllowFx)});
			});
			auto RangeFrames = RangeContext->Run([&]{
				return SetContextFrames(ComposeFrames+array{frame(FrameSigil,Lambda->RangeRegs,Lambda->LambdaAllowFx,future_array<unificand>(Result))});
			});
			return RangeFrames;
		};
		auto LambdaUnificand     = MakeLambda<unificand>(Locus,
			[=](const auto& DomainContext,const auto& RangeContext,const unificand& Result,const unificand& Parameter) {
				auto BetaFrames = GetFrames(Frames); // Go into beta reduction, so beta_frame instead of verify_scope.
				return InitBetaFxFrames(BetaFrames,DomainContext,DomainContext,RangeContext,Result,Parameter);
			},
			[=](const array<box<frame_managed<unificand>>>& RangeFrames,const unificand& Result,const unificand& Parameter,const step& Step1)->current_step {
				return EvalStep(Locus,RangeFrames.EndSlice(2),Lambda->DomainOp,Step1);
			},
			[=](const array<box<frame_managed<unificand>>>& RangeFrames,const unificand& Result,const unificand& Parameter,const step& Step1)->current_step {
				return EvalStep(Locus,RangeFrames,Lambda->RangeOp,Step1);
			}
		);
		return UnifyStep("Eval-lambda",Locus,TargetUnificand,LambdaUnificand,[=]()->current_step {
			if constexpr(!Runs<unificand>) {
				auto InputLambdaVertex = box<input_function_vertex>(InputUnificand,LambdaUnificand);
				if(*Lambda->OutputReg.RegName) LambdaUnificand          .SetVertexName(var<string>(ToString(*Lambda->OutputReg.RegName,"_lam")));
				if(*Lambda->InputReg.RegName)  vertex(InputLambdaVertex).SetVertexName(var<string>(ToString(*Lambda->InputReg.RegName,"_input_lam")));
				Frames[Lambda->LambdaInputReg.RegDepth]->FrameUnificands.Init(Lambda->LambdaInputReg.CoerceIndex(),InputLambdaVertex);
				return EnterComposeStep(InputContext,
					[=]()->current_step {
						auto State = verifier_iterate_state{
							GetAssumedFxConst(effects),visibility::Verify,
							[=](fx NewChildFx) {
								auto VI=Thread.Coerce<verifier_iterate>();
								return VI->SilenceCount || NewChildFx<=Lambda->LambdaAllowFx?
									only_succeeds:
									(only_cardinalities&CompliesFx(NewChildFx,Lambda->LambdaAllowFx)) +
									(only_cardinalities&resolves&(InputInflexible? Coerce<box<verifier_context_state>>(InputContext)->GetAssumedFx(): effects)) + // For test(D00){a:int => f(:nat)():type{nat[a]} => f[a]}.
									(VerifyHere? only_rejects: no_effects);
							}
						};
						return IterateStep<verifier_iterate_managed>(
							HereContext(Locus,"Eval-lambda-beta"_VS),
							State,False,false,Lambda->LambdaAllowFx,abstracts&converges&computes&no_unifies,Step0,
							[=]{return False;},
							[=](tuple<>)->current_step {
								auto DomainContext   = MakeContext<unificand>(
									HereContext(Locus,"Eval-lambda-domain"_VS),DomainAssumeFx,
									true,GetAssumedFxConst(DomainAssumeFx),DefaultGetSuspendedFx);
								auto ParameterContext = MakeContext<unificand>(
									HereContext(Locus,"Eval-lambda-parameter"_VS),DomainAssumeFx,
									true,GetAssumedFxConst(DomainAssumeFx),DefaultGetSuspendedFx);
								auto ParameterVertex = ParameterContext->Run([&]{return vertex();}); ParameterVertex.SetVertexName(var<string>("ldp"_VS));
								auto RangeContext    = MakeContext<unificand>(
									HereContext(Locus,"Eval-lambda-range"_VS),Lambda->LambdaAllowFx,
									false,GetAssumedFxConst(effects),DefaultGetSuspendedFx);
								auto RangeResult     = vertex(); RangeResult.SetVertexName(var<string>("lrr"_VS));
								auto RangeFrames     = InitBetaFxFrames(Frames,ParameterContext,DomainContext,RangeContext,RangeResult,ParameterVertex);
								return DomainContext->RunStep(
									[=]()->current_step {
										return RangeContext->RunStep(LeaveIterateStep,[=](const step& Step1)->current_step {
#if 1
											// Partial implementation of new self-call: calls TargetUnificand but ignores 
											// this LambdaUnificand, ensuring we spine-verify range.
											auto RangeFunction  = vertex(); RangeFunction .SetVertexName(var<string>("lrl"));
											auto RangeParameter = vertex(); RangeParameter.SetVertexName(var<string>("lrp"));
											return UnifyStep("Eval-under-lambda-unify-RangeFunction",Locus,RangeFunction,TargetUnificand,[=]()->current_step {
												return UnifyStep("Eval-under-lambda-unify-RangeParameter",Locus,RangeParameter,ParameterVertex,[=]()->current_step{
													// We call TargetReg but ignore LambdaUnificand, because we choose to verify it here
													// we don't implement self-call plumbing necessary to simulate calling it.
													return EvalCallStep(Locus,RangeResult,RangeFunction,RangeParameter,Truth(LambdaUnificand),[=]()->current_step{
														return EvalStep(Locus,RangeFrames,Lambda->RangeOp,Step1);
													});
												});
											});
#else
											return EvalStep(Locus,RangeFrames,Lambda->RangeOp,Step1);
#endif
										});
									},
									[=](const step& Step1)->current_step {
										return EvalStep(Locus,RangeFrames.EndSlice(2),Lambda->DomainOp,Step1);
									}
								);
							},
							[=](tuple<>,tuple<>,const continue_step<tuple<>>& NextStep,const step& Step1)->current_step {return Step1;},
							[=](tuple<>,const step& Step1)->current_step {return Step1;}
						);
					},
					[=](const step& Step1)->current_step {
						return UnifyStep("Eval-lambda-Unify-Input",Locus,InputUnificand,InputLambdaVertex,Step1);
					}
				);
			}
			else return Step0;
		});
	}
	if(auto Beta=Op.Cast<op::beta>()) {
		auto BetaComposeContext = LoadContext(Frames,Beta->ComposeOutputReg);
		return RunFarStep(BetaComposeContext,
			[=]()->current_step {
				// Run range.
				if constexpr(Runs<unificand>) {
					auto RunRangeContext = box<run_beta_context_managed>(BetaComposeContext);
					return RunRangeContext->RunStep(Step0,[=](const step& Step1)->current_step {
						return EvalStep(Locus,Frames,Beta->BetaRangeOp,[=]()->current_step {
							return RunRangeContext->RunBetaCheckRange(Step1);
						});
					});
				}
				else return EvalStep(Locus,Frames,Beta->BetaRangeOp,Step0);
			},
			[=](const step& Step1)->current_step {
				// Run compose.
				auto CallFrames = SetContextFrames(Frames+array{frame_type(FrameSigil,Beta->BetaComposeRegs,effects)});
				return EvalStep(Locus,CallFrames,Beta->BetaComposeOp,Step1);
			});
	}
	if(auto Enter=Op.Cast<op::enter>()) { // Rules (InSolveElim), (InImplyIntro).
		auto InputContext = LoadContext(Frames,Enter->InputReg);
		return EnterComposeStep(InputContext,Step0,[=](const step& Step1)->current_step {
			return EvalStep(Locus,Frames,Enter->EnterOp,Step1);
		});
	}
	if(auto Hold=Op.Cast<op::hold>()) { // Rules (InSolveElim), (InImplyIntro).
		if constexpr(Runs<unificand>)
			return Step0;
		else {
			auto InputUnificand       = Load(Frames,Hold->InputReg);
			auto OutputUnificand      = Load(Frames,Hold->OutputReg);
			auto AbstractionUnificand = Load(Frames,Hold->AbstractionReg);
			auto AbstractionFx        = var<fx>(effects);
			if(FlexibleStart(InputUnificand.Context())==FlexibleStart(AbstractionUnificand.Context()))
				return Step0;
			return NarrowAbstractionFx(Locus,"EvalStep-in-abstraction"_VS,AbstractionUnificand,OutputUnificand,AbstractionFx,[=]()->current_step {
				box<abstracting_suspension>(Locus,[=]{
					return FrameAllowFx(Frames) & *AbstractionFx;
				})->Suspend();
				return Step0;
			});
		}
	}
	if(auto Stage=Op.Cast<op::stage>()) { // Rule (StageElim).
		auto AbstractionFx     = var<fx>(effects);
		auto StageAllowFx      = FrameAllowFx(Frames);
		auto InputUnificand    = Load(Frames,Stage->InputReg);
		auto InputContext      = LoadContext(Frames,Stage->InputReg);
		bool InputInflexible   = !Runs<unificand> && FlexibleStart(InputContext)!=FlexibleStart();
		auto StageOpContext    = Thread;
		auto StageGetAssumedFx = [=] {
			auto AssumedFx = FrameAllowFx(Frames) & *AbstractionFx & AssumedFxHere(StageOpContext);
			// Shows we must weaken by domain regardless of input flexibility: f(x:int):int=3; y:=f[y]
			if(InputInflexible)
				AssumedFx += only_cardinalities&abstracts&Coerce<box<verifier_context_state>>(InputContext)->GetAssumedFx();
			return AssumedFx+StageFxHere(StageOpContext,Stage->StageSite);
		};
		auto AbstractionContext   = MakeContext<unificand>(HereContext(Locus,"Eval-stage-type"_VS),Thread->AllowFx,
			true,StageGetAssumedFx,
			[=](fx NewChildFx) {return StageGetAssumedFx()&no_unifies;}
		);
		auto ComposeFrames        = Frames+array{frame_type(FrameSigil,Stage->ComposeRegs,effects)};
		auto AbstractionFrames    = AbstractionContext->Run([&]{
			// We evaluate reduced :t op.call inside this scope, with no available imply.
			return SetContextFrames(ComposeFrames+array{frame_type(FrameSigil,Stage->StageRegs,effects)});
		});
		auto OutputUnificand             = Load(Frames,Stage->OutputReg);
		auto AbstractionInputUnificand   = AbstractionFrames.End(0)->FrameUnificands.Elements.GetInit(0); // Position fixed by op::stage.
		auto AbstractionOutputUnificand  = AbstractionFrames.End(0)->FrameUnificands.Elements.GetInit(1); // Position fixed by op::stage.
		if(Runs<unificand>)
			return EvalStep(Locus,AbstractionFrames,Stage->StageValueOp,[=]()->current_step {
				return UnifyStep("Eval-stage-Output",Locus,OutputUnificand,AbstractionOutputUnificand,[=]()->current_step {
					return EvalStep(Locus,AbstractionFrames,Stage->StageAbstractionOp,Step0);
				});
			});
		return UnifyStep("Eval-stage-Output",Locus,OutputUnificand,AbstractionOutputUnificand,[=]()->current_step {
			auto AbstractionUnificand   = Load(Frames,Stage->AbstractionReg);
			return NarrowAbstractionFx(Locus,"EvalStep-in-abstraction"_VS,AbstractionUnificand,OutputUnificand,AbstractionFx,[=]()->current_step {
				return AbstractionContext->RunStep(
					[=]()->current_step {
						// Treat :t=v as identity mapping; v can't flex input. TODO: Suppress for :t:v.
						if constexpr(!VerifyHere)
							return Step0;
							else return IterateStep<verifier_iterate_managed>(
								[=](const iterate& I) {
									auto CurrentStageAllowFx = StageAllowFx & *AbstractionFx; //!!nonmonotonic; need to be able to get upper and lower bound
									return Coerce<box<verifier_context_state>>(I)->CompletedChildFx<=contradicts+CurrentStageAllowFx? R00(Locus,"Stage-Iterate"_VS): V10(Locus);
								},
								verifier_iterate_state{
									GetAssumedFxConst(effects),
									visibility::Verify,
									[=](fx NewChildFx) {
										//!!need to reconcile this with spec's stage->visibility transformation; is this just error message refinement or more?
										//!!need use sites to hold excess effects?
										auto VI                  = Thread.Coerce<verifier_iterate>();
										auto CurrentStageAllowFx = StageAllowFx & *AbstractionFx; //!!nonmonotonic; need to be able to get upper and lower bound
										auto NewStageFx          = VI->SilenceCount? only_succeeds: CompliesFx(NewChildFx,StageAllowFx);
										VI->ContextStageFx.Set(CoerceStageSite(Stage->StageSite),NewStageFx); // Ought to assert monotonic.
										return VI->SilenceCount || NewChildFx<=CurrentStageAllowFx?
											only_succeeds:
											(only_cardinalities&CompliesFx(NewChildFx,CurrentStageAllowFx)) +
											(only_cardinalities&resolves&(InputInflexible? Coerce<box<verifier_context_state>>(InputContext)->GetAssumedFx(): effects)) +
											(VerifyHere? only_rejects: no_effects); // This supports the V10 error message logic above.
									}
								},
								False,false,
								StageAllowFx & *AbstractionFx, //!!nonmonotonic; need to be able to get upper and lower bound
								effects,Step0,
								[=]{return False;},
								[=](tuple<>)->current_step {
									// TypeOp and ValueOp must share register allocation so they can interact.
									auto ComposeContext        = MakeContext<unificand>(HereContext(Locus,"op.beta-plumb"_VS),
										Thread->AllowFx & *AbstractionFx, //!!nonmonotonic; need to be able to get upper and lower bound
										InputInflexible/*!!?*/,
										[=]{return AssumedFxHere(InputContext);},
										DefaultGetSuspendedFx);
									auto ComposeFrames         = ComposeContext->Run([&]{
										return SetContextFrames(Frames+array{frame_type(FrameSigil,Stage->ComposeRegs,StageAllowFx & *AbstractionFx /*!!nonmono*/)});
									});
									auto CheckFrames           = SetContextFrames(ComposeFrames+array{frame_type(FrameSigil,Stage->StageRegs,StageAllowFx & *AbstractionFx /*!!nonmono*/)});
									return EvalStep(Locus,CheckFrames,Stage->StageValueOp,[=]()->current_step {
										return EvalStep(Locus,CheckFrames,Stage->StageAbstractionOp,LeaveIterateStep);
									});
								},
								[=](tuple<>,tuple<>,const continue_step<tuple<>>& NextStep,const step& Step2)->current_step {
									return NextStep(False,Step2);
								},
								[=](tuple<>,const step& Step2)->current_step {return Step2;}
							);
					},
					[=](const step& Step1)->current_step {
						return EnterComposeStep(InputContext,
							[=]()->current_step {
								return EvalStep(Locus,AbstractionFrames,Stage->StageAbstractionOp,Step1);
							},
							[=](const step& Step2)->current_step {
								return UnifyStep("Eval-stage-abstraction-input",Locus,InputUnificand,AbstractionInputUnificand,Step2);
							}
						);
					}
				);
			});
		});
	}
	if(auto Check=Op.Cast<op::check>()) { // Rule (CheckIntro).
		// check<fx>{s0} enables checking for fewer effects than allowed in enclosing context.
		auto CheckAllowFx  = FrameAllowFx(Frames) & Check->CheckAllowFxMask & abstracts; // No iterates because nobody handles it.
		auto CheckContext  = MakeContext<unificand>(
			[=](const verifier_context& Self) {
				return !Check->CheckError || Coerce<box<verifier_context_state>>(Self)->CompletedChildFx<=CheckAllowFx+contradicts?
					R00(Locus,"op.check-context"_VS):
					Check->CheckError.Coerce();
			},
			CheckAllowFx,
			true,GetAssumedFxConst(effects),
			[=](fx NewChildFx) {return (NewChildFx&no_unifies) + (NewChildFx<=CheckAllowFx? no_effects: only_rejects);}
		);
		auto CheckFrames          = CheckContext->Run([&]{
			return SetContextFrames(Frames+array{frame_type(FrameSigil,Check->CheckRegs,CheckAllowFx)});
		});
		auto OutputUnificand      = Load(Frames,Check->OutputReg);
		auto CheckResultUnificand = CheckFrames.End(0)->FrameUnificands.Elements.GetInit(0);
		return UnifyStep("Eval-check-Output",Locus,OutputUnificand,CheckResultUnificand,[=]()->current_step {
			return CheckContext->RunStep(Step0,[=](const step& Step2)->current_step {
				return EvalStep(Locus,CheckFrames,Check->CheckOp,Step2);
			});
		});
	}
	if(auto Assume=Op.Cast<op::assume>()) { // Rule (AssumeIntro).
		auto AssumeOpContext      = Thread;
		auto AssumeAllowFx        = FrameAllowFx(Frames) & abstracts; // No iterates because nobody handles it.
		auto AssumeBodyContext    = MakeContext<unificand>(HereContext(Locus,"op.assume-context"_VS),Thread->AllowFx,true,
			function<fx()>([=]{
				return Assume->AssumeFx & AssumedFxHere(AssumeOpContext);
			}),
			DefaultGetSuspendedFx);
		auto AssumeInputContext   = AssumeBodyContext->Run([&]{
			return MakeContext<unificand>(HereContext(Locus,"op.assume-context"_VS),Thread->AllowFx,true,
				function<fx()>([=]{
					return Assume->AssumeFx & AssumedFxHere(AssumeOpContext);
				}),
				DefaultGetSuspendedFx);
			});
		auto AssumeParameterFrames = AssumeInputContext->Run([&]{
			return SetContextFrames(Frames+array{frame_type(FrameSigil,Assume->ParameterRegs,AssumeAllowFx)});
		});
		auto AssumeFrames          = AssumeBodyContext->Run([&]{
			return SetContextFrames(AssumeParameterFrames+array{frame_type(FrameSigil,Assume->AssumeRegs,AssumeAllowFx)});
		});
		auto OutputUnificand       = Load(Frames,Assume->OutputReg);
		auto CheckResultUnificand  = AssumeFrames.End(0)->FrameUnificands.Elements.GetInit(0);
		return UnifyStep("Eval-assume-Output",Locus,OutputUnificand,CheckResultUnificand,[=]()->current_step {
			return AssumeBodyContext->RunStep(Step0,[=](const step& Step1)->current_step {
				return EvalStep(Locus,AssumeFrames,Assume->AssumeOp,Step1);
			});
		});
	}
	if(auto Length=Op.Cast<op::length>()) {
		auto ArrayUnificand  = Load(Frames,Length->ArrayReg);
		auto LengthUnificand = Load(Frames,Length->LengthReg);
		return WhenCastStep<nat>(
			Here(Locus,"Eval-length"_VS),only_cardinalities&abstracts,LengthUnificand,Truth(LengthUnificand),Step0,
			[=](nat n,const step& Step1)->current_step {
				return UnifyStep("Eval-length",Locus,ArrayUnificand,MakeArrayUnificand<unificand>(n),Step1);
			},
			false,true
		);
	}
	if(auto Conditional=Op.Cast<op::conditional>()) {
		if(Runs<unificand>? Conditional->IfRun: VerifyHere? Conditional->IfVerify: Conditional->IfBeta)
			return EvalStep(Locus,Frames,Conditional->Op,Step0);
		else
			return Step0;
	}
	if(auto Range=Op.Cast<op::range>()) {
		auto TargetUnificand=Load(Frames,Range->TargetReg);
		return WhenCastStep<integer>(
			VERIFIER_HERE("Eval-range-First"),only_cardinalities&iterates,Load(Frames,Range->FirstReg),Truth(TargetUnificand),Step0,
			[=](const integer& First,const step& Step1)->current_step {
				return WhenCastStep<integer>(
					VERIFIER_HERE("Eval-range-Last"),only_cardinalities&iterates,Load(Frames,Range->LastReg),Truth(TargetUnificand),Step1,
					[=](const integer& Last,const step& Step2)->current_step {
						auto Difference=Last-First;
						if(auto N=Difference.Cast<nat>())
							return ForForkStep(Locus,!IsImplying()? Truth(effects): False,0,N.Coerce()+1,Step2,[=](nat i,const step& Step3)->current_step {
								return UnifyStep("Eval-range-Value",Locus,TargetUnificand,unificand(First+i),Step3);
							});
						else if(Difference<0)
							return FailTargetStep("Eval-range-Empty",locus{},TargetUnificand,Step2);
						else
							VERSE_ERR("op::range: Too large range isn't currently supported");
					},
					false,true
				);
			},
			false,true
		);
	}
	if(auto OpPrint=Op.Cast<op::print>()) {
		string S;
		for(auto T:OpPrint->Things)
			if(auto RO=T.Cast<reg>())
				S+=ToString(RO.Coerce(),"=",Load(Frames,RO.Coerce()));
			else
				S+=ToString(T);
		Print("Eval-print ",S);
		//if constexpr(!Runs<unificand>) // Somehow causes loop.
		//	DiagnoseEquation(Value);
		return Step0;
	}
	if(auto Test=Op.Cast<op::test>()) {
		return WhenFxStep(Here(Locus,"Eval-test-WhenFx"_VS),only_rejects,succeeds&converges&computes&no_unifies,Step0,[=](const step& Step1)->current_step {
			Print(ToString(Test->Locus),Runs<unificand>? ": Running test...": ": Verifying test...");
			auto TestResult=Eval<unificand>(Locus,"test"_VS,[=](const step& Step0)->current_step {
				auto TestFrames = SetContextFrames(Frames+array{frame_type{FrameSigil,Test->TestRegs,top_allows}});
				return EvalStep(Locus,TestFrames,Test->Body,Step0);
			},Test->ExpectedErrorCode);
			if(!TestResult && TestResult.GetError().ErrorCode!=Test->ExpectedErrorCode)
				return ErrStep(TestResult.GetError());
			if(TestResult && Test->ExpectedErrorCode!="S00" && (Runs<unificand> || Test->ExpectedErrorCode.Slice(0,1)!="R"))
				return ErrStep(T01(Locus));
			return Step1;
		});
	}
	if(auto Stuck=Op.Cast<op::stuck>()) {
		Verse::Stuck(Stuck->Fx,Stuck->StuckError);
		return Step0;
	}
	VERSE_UNEXPECTED;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Op inference refinement.

static void TraceVisibility(const pin<context>& C) {
	C->Run([&]{
		if(auto VI=C.Cast<verifier_iterate>(); VI && !VI->IsCommitted)
			for(auto[E,_]:VI->Equations) 
				if(E->Trace) {
					Print("Trace: ",E);
					Print("    HeadFx=",E->EquationHeadFx,", DeepFx=",E->EquationDeepFx);
					for(auto[U,UL]:E->Vertices)
						if(auto UM=U.CastVertex<vertex_managed>())
							for(auto[V,__]:UM->VertexPredecessors) {
								auto UStrength=AssumedFxHere(UL->EquateVertex.Context());
								Print("    ",V," -> ",UL.ReadValue(),UStrength<=succeeds? " S": UStrength<=decides? " D": "");
							}
				}
		auto SusStep=C->ContextResumeStep;
		while(auto Sus=Cast<suspension_managed>(SusStep)) {
			if(auto D=Sus.Coerce().Cast<context>())
				TraceVisibility(D.Coerce());
			SusStep=Sus->NextResumeStep;
		}
	});
}
static void RefineSuspensions(const context& C,bool& Narrowed,bool RefineSuspended) {
	auto CVI  = C.Cast<verifier_iterate>();
	auto CVCS = Coerce<box<verifier_context_state>>(C);
	if(CVI && CVI->Stopped())
		return;
	if(CVI && !CVI->IsCommitted) {
		CVI->ContextJoinStageFx=False; // Make all <succeeds>.
		for(auto[E,_]:CVI->Equations)
			E->PropagateEquality(Narrowed);
		bool NarrowedFx;
		do
			for(NarrowedFx=false; auto[E,_]:CVI->Equations)
				E->PropagateEquationFx(NarrowedFx);
		while(NarrowedFx);
		for(auto Strength=strength_succeeds; Strength<strength_resolves; Strength=StrengthSuccessor(Strength)) {
			bool More;
			for(auto[E,_]:CVI->Equations)
				E->ResetDominators(Strength);
			do
				for(More=false; auto[E,_]:CVI->Equations)
					E->ExpandDominators(More,Strength);
			while(More);
			do
				for(More=false; auto[E,_]:CVI->Equations)
					E->NarrowDominators(More,Strength);
			while(More);
			for(auto[E,_]:CVI->Equations)
				E->DominatorElim(Narrowed,Strength);
		}
	}
	auto CompletedChildAbstractingFx = only_succeeds;
	C->LocalPendingFx                = C->StuckFx;
	auto SusStep                     = C->ContextResumeStep;
	while(auto Sus=Cast<suspension_managed>(SusStep)) {
		if(auto NewSuspendedFx=Sus->OnRefineFx(Narrowed); NewSuspendedFx!=Sus->SuspendedFx)
			Narrowed=true, Sus->SuspendedFx=NewSuspendedFx;
		C->LocalPendingFx  = SequenceFx(C->LocalPendingFx,Sus->SuspendedFx);
		if(auto SusVCS=Cast<verifier_context_state>(Sus.Coerce()))
			CompletedChildAbstractingFx = SequenceFx(CompletedChildAbstractingFx,SusVCS->AbstractingFx);
		else if(auto SusAbstracting=Cast<abstracting_suspension>(Sus.Coerce()))
			CompletedChildAbstractingFx = SequenceFx(CompletedChildAbstractingFx,SusAbstracting->AbstractingFx());
		SusStep = Sus->NextResumeStep;
	}
	if(C->LocalPendingFx!=CVCS->CompletedChildFx || CompletedChildAbstractingFx!=CVCS->CompletedChildAbstractingFx) {
		Narrowed=Narrowed || RefineSuspended;
		CVCS->CompletedChildFx            = C->LocalPendingFx;
		CVCS->CompletedChildAbstractingFx = CompletedChildAbstractingFx;
	}
	if(CVI && !CVI->IsCommitted) { // Rules (FxvUp), (VerifyUp).

		// If this visibility contains nested verifies, update its ContextStageFx from nested ContextJoinStageFx.
		for(auto[StageSite,StageFx]:CVI->ContextJoinStageFx)
			if(auto Old=CVI->ContextStageFx.Get(StageSite).Else(effects),New=StageFx.ReadValue()&Old; Old!=New)
				//Print("StageFx ",CVI->Depth,": ",StageSite," -> ",StageFx.ReadValue()),
				//VERSE_ENSURE(New<Old), // Enable this when monotonic.
				Narrowed=true, CVI->ContextStageFx.Set(StageSite,New);
		
		// If this visibility is nested in an outer visibility, add our ContextStageFx to outer ContextJoinStageFx.
		//!! We need to go further and add <effects> if any peer stage-visibility has opv and we don't,
		// by maintaining a delimited list or span of implication-forked visibility-iterates, in case macro
		// resolution delays eval reaching the stage in a given fork.
		if(CVI->IterateVisibility!=visibility::VerifyTop)
			for(auto[StageSite,StageFx]:CVI->ContextStageFx) {
				auto OuterVisibility        = IterateStart(CVI->Context());
				//Print("Propagate ",CVI->Depth,"/",OuterVisibility->Depth,": ",StageSite," -> ",StageFx.ReadValue());
				auto OuterJoinStageStrength = OuterVisibility->ContextJoinStageFx.Get(StageSite).Else(only_succeeds);
				OuterVisibility->ContextJoinStageFx.Set(StageSite,OuterJoinStageStrength+StageFx.ReadValue());
			}

	}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Specifier verification.

template<class callback> current_step VerifySpecs(const redex& Redex,bool AllowFunctionSpecifiers,fx AllowFx,const array<future<box<syntax>>>& Specifiers,const step& Step0,const callback& Callback,nat i=0,fx KeepFx=effects,fx AddFx=no_effects,option<function_specifiers> FunctionSpecifiers=False,array<vertex> SpecifierVertices=False) {
	if(nat n=Length(Specifiers); i<n) {
		if(!SpecifierVertices) {
			auto SpecifierRegs=For(n,[&](nat i) {
				return Redex.FreshReg("Specifier"_VS);
			});
			return Redex.ReduceStep(Step0,box<op::sequence>(
				box<op::conditional>(true,false,false,
					box<op::sequence>(For(n,[&](nat i) {
						return box<op::scope>(only_succeeds, //!! Need to wrap in paraconsistent op::check demanding only_succeeds.
							box<op::reduce>(Redex,1,CaptureString("VerifySpecs-Reduce"),[=](const redex& Redex1,const step& Step1)->current_step {
								auto SpecifierRedex=Redex1.FreshRedex(Redex1.FreshReg("Specifier"_VS),SpecifierRegs[i],Specifiers[i]);
								return Redex1.ReduceStep(Step1,box<op::sequence>(
									box<op::exists>(SpecifierRedex),
									SpecifierRedex.Op
								));
							})
						);
					}))
				),
				box<op::reduce>(Redex,0,CaptureString("VerifySpecs"),[=](const redex& Redex1,const step& Step1)->current_step {
					return VerifySpecs(Redex1,
						AllowFunctionSpecifiers,AllowFx,Specifiers,Step1,
						Callback,i,KeepFx,AddFx,FunctionSpecifiers,
						SpecifierRegs.For([&](const reg& SpecifierReg) {
							return Load(Redex.Scopes,SpecifierReg);
						})
					);
				})
			));
		}

		// Must say what we do with specifiers whose evaluation fails, as they have no runtime semantics.
		auto SpecifierVertex = SpecifierVertices[i];
		return WhenCastStep<any>(
			[=]{return X30(Redex.Locus,"VerifySpecs.WhenCastStep"_VS);},
			effects,SpecifierVertex,option<vertex>(False),Step0,
			[=](const any& A,const step& Step1)->current_step {
				if(auto CastSpecifierFx=A.Cast<fx>()) {
					fx SpecifierFx = CastSpecifierFx.Coerce();
					return VerifySpecs(Redex,AllowFunctionSpecifiers,AllowFx,
						Specifiers,Step1,Callback,i+1,
						(KeepDefaultFx  (SpecifierFx)&KeepFx),
						(KeepSpecifierFx(SpecifierFx)&SpecifierFx)+AddFx,
						FunctionSpecifiers,
						SpecifierVertices
					);
				}
				else if(auto FS=A.Cast<function_specifiers>()) {
					if(!AllowFunctionSpecifiers)
						return Stuck(effects,X31(Redex.Locus,"VerifySpecs"_VS)), Step1;
					auto NewFS=FunctionSpecifiers.Else(function_specifiers::None)+FS.Coerce();
					if(function_specifiers::Closed<=NewFS && function_specifiers::Open<=NewFS)
						return Stuck(effects,X32(Redex.Locus)), Step1;
					return VerifySpecs(Redex,AllowFunctionSpecifiers,AllowFx,
						Specifiers,Step1,Callback,i+1,
						KeepFx,
						AddFx,
						Truth(NewFS),
						SpecifierVertices
					);
				}
				else return Stuck(effects,X30(Redex.Locus,"VerifySpecs.Stall"_VS)), Step1;
			},
			false,true
		);
	}
	if(!(AddFx<=AllowFx))
		return Stuck(effects,X31(Redex.Locus,"VerifySpecs"_VS)), Step0;
	return Callback(Redex,KeepFx,AddFx,FunctionSpecifiers,Step0);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier macros.

// A macro's entry point.
using invoke_step = current_step(*)(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0);

// Macro for verifier.
struct macro_vertex: vertex_managed {
	invoke_step Invoke;
	macro_vertex(invoke_step Invoke0): vertex_managed(true), Invoke(Invoke0) {} // Should we provide a shape?
	friend string ExposeToString(const macro_vertex& V) {
		return ToString(static_cast<const vertex_managed&>(V),"_macro"_VS);
	}
};
expose_mutable ExposeUnique(const macro_vertex&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Op makers.

op::reduce::reduce(const redex& ReduceRedex0,nat DepthChange0,const function<string()>& Describe0,const function<current_step(const redex&,const step&)>& OnVerifyStep0):
	Locus(ReduceRedex0.Locus), DepthChange(DepthChange0),
	FutureOp(TopContext->Run([&]{return future<box<op>>();})),
	Describe(Describe0), ReduceRedex(ReduceRedex0), OnVerifyStep(OnVerifyStep0) {
	ReduceRedex.Op = box(*this);
	if(DepthChange==0)
		ReduceRedex.Introduce();
}
tuple<array<reg>,array<reg>,box<op>> redex::FreshArrayRegs(reg SpanReg,nat n,const string& S) const {
	if(n==0)
		return{False,False,box<op::unify>(SpanReg,reg())};
	auto ArrayReg = reg(Length(Scopes)-1,TopContext->Run([]{return future<nat>();}),0,n,S);
	auto SpanOp   = box<op>(box<op::span>(SpanReg,ArrayReg,n));
	return{array{ArrayReg},ArrayReg.ArrayElementRegs,SpanOp};
}
box<op> MakeIdentifierOp(const redex& Redex,const string& Identifier,bool Defined) {
	auto Result=box<op::reduce>(Redex,0,CaptureString("MakeIdentifierOp"),[=](const redex& Redex1,const step& Step1)->current_step {
		auto NotFailedCount = var<nat>(Length(Redex1.Scopes)), SucceedsCount=var<nat>(0U);
		auto StallError     = [=]{
			if(*SucceedsCount!=1)
				return N00(Redex1.Locus,Identifier);
			else
				return N02(Redex1.Locus,Identifier);
		};
		return ForStep(Redex1.Scopes,Step1,[=](const verify_scope& Scope,const step& Step2)->current_step {
			return WhenResolveStep(StallError,abstracts,Scope->ScopeSymbols->FutureGet(Identifier),Step2,[=](const option<reg>& Reg,const step& Step3)->current_step {
				if(Reg) {
					// Search in FrameSymbols completed and found Identifier mapping to Reg.
					if(++SucceedsCount==1) {
						if(Defined)
							return Step3;
						else
							return Redex1.ReduceStep(Step3,Reg.Coerce(),false);
					}
					return Stuck(succeeds&converges&computes&no_unifies,(Defined? N04: N03)(Redex1.Locus,Identifier)), Step3;
				}
				else {
					// Search in FrameSymbols completed without finding identifier.
					if(--NotFailedCount>0)
						return Step3;
					return Stuck(succeeds&converges&computes&no_unifies,N00(Redex1.Locus,Identifier)), Step3;
				}
			});
		});
	});
	Redex.Scopes.End(0)->ScopeSymbols->EliminateBatch();
	return Result;
}
box<op> MakeNop() {
	return box<op::sequence>();
}
bool FarInput(const array<verify_scope>& Scopes,const reg& InputReg,const reg& OutputReg) {
	return FlexibleStart(LoadContext(Scopes,InputReg))!=FlexibleStart(LoadContext(Scopes,OutputReg));
}
bool redex::FarInput() const {
	return ::FarInput(Scopes,InputReg,OutputReg);
}
box<op> MakeInputIdentityOp(const array<verify_scope>& Scopes,const reg& InputReg,const reg& OutputReg) {
	if(FarInput(Scopes,InputReg,OutputReg))
		return box<op::enter>(InputReg,box<op::unify>(InputReg,OutputReg));
	return box<op::unify>(OutputReg,InputReg);
}
box<op> redex::MakeInputIdentityOp() const {
	return ::MakeInputIdentityOp(Scopes,InputReg,OutputReg);
}
redex redex::WithOp(const reg& SourceReg) const {
	return WithOp(box<op::sequence>(
		MakeInputIdentityOp(),
		box<op::unify>(OutputReg,SourceReg)
	));
}
redex redex::WithOp(tuple<>) const {
	return WithOp(box<op::sequence>(
		box<op::fail>(InputReg),
		box<op::fail>(OutputReg)
	));
}
template<class s,class f> box<op> MakeIterateOp(const char* What,const redex& Redex,const reg& InitialReg,const clause& DomainClause,const s& S,const f& F) {
	return box<op::iterate>(InitialReg,Redex.InputReg,
		box<op::reduce>(Redex,1,CaptureString(What,"_domain",DomainClause),[=](const redex& DomainRedex,const step& Step0)->current_step {
			auto DomainOutputReg = DomainRedex.FixedReg(0);
			auto DomainInputReg  = DomainRedex.FreshReg("DomainInput"_VS);
			auto DomainRedex1    = DomainRedex.FreshRedex(DomainInputReg,DomainOutputReg,DomainClause);
			return DomainRedex.ReduceStep(Step0,box<op::sequence>{
				box<op::exists>(DomainInputReg),
				DomainRedex1.Op
			});
		}),
		box<op::reduce>(Redex,3,CaptureString(What,"_succeeds"),[=](const redex& SucceedsRedex,const step& Step0)->current_step {
			auto DomainOutputReg = SucceedsRedex.WithEndSliceScopes(2).FixedReg(0); // Aligned with DomainRedex above.
			auto CurrentValueReg = SucceedsRedex.FixedReg(0);
			auto NextValuesReg   = SucceedsRedex.FixedReg(1);
			return S(SucceedsRedex,DomainOutputReg,CurrentValueReg,NextValuesReg,[=]()->current_step {
				SucceedsRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
				return ResumeStep(Step0);
			});
		}),
		box<op::reduce>(Redex,1,CaptureString(What,"_fails"),[=](const redex& FailsRedex,const step& Step0)->current_step {
			auto CurrentValueReg = FailsRedex.FixedReg(0);
			return F(FailsRedex,CurrentValueReg,Step0);
		})
	);
}
box<op> MakeConditionalOp(const char* What,const redex& Redex,const clause& DomainClause,const option<clause>& ThenClause,bool ThenFail,const option<clause>& ElseClause) {
	return MakeIterateOp(What,Redex,reg(),DomainClause,
		[=](const redex& SucceedsRedex,const reg& DomainOutput,const reg& CurrentValueReg,const reg& NextValuesReg,const step& Step0)->current_step {
			return SucceedsRedex.ReduceStep(Step0,box<op::sequence>(
				box<op::unify>(NextValuesReg,reg()),
				ThenClause? SucceedsRedex.NewAlias(ThenClause.Coerce()).Op:
				ThenFail?   SucceedsRedex.NewAlias(False).Op:
							SucceedsRedex.NewAlias(DomainOutput).Op
			));
		},
		[=](const redex& FailsRedex,const reg& CurrentValueReg,const step& Step0)->current_step {
			return FailsRedex.ReduceStep(Step0,
				ElseClause?
					FailsRedex.NewAlias(ElseClause.Coerce()).Op:
					FailsRedex.NewAlias(False).Op
			);
		}
	);
}

// Reduce a redex's op.
current_step redex::ReduceStep(const step& Step0,const box<op>& LocalOp,bool Introduces) const {
	auto ReduceFutureOp = Op.Coerce<op::reduce>()->FutureOp;
	ResolveOpBatch(Locus,ReduceFutureOp,LocalOp);
	ReduceFutureOp.Coerce()->OnAllocate(Scopes);
	if(Introduces)
		Scopes.End(0)->ScopeSymbols->EliminateBatch();
	return ResumeStep([=,*this]()->current_step {
		return EvalStep(Locus,Scopes,LocalOp,Step0);
	});
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Syntax desugaring.

// Desugaring regular syntax to ops.
redex redex::WithOp(const future<box<syntax>>& SyntaxFuture) const {
	// When we have user macros, we'll have lenient syntax and require the op::reduce and WhenResolveStep combination here.
	// In the meantime, we keep the op::reduce for debug info.
	auto Syntax = SyntaxFuture.Coerce(); 
	return WithOp(box<op::reduce>(WithLocus(Syntax->Locus),0,CaptureString(Syntax),[=,*this](const redex& Redex,const step& Step0)->current_step {
		if(auto Atom=Syntax.Cast<syntax::atom>()) // Rule (AtomSyntax).
			return Redex.ReduceStep(Step0,box<op::sequence>(
				Redex.MakeInputIdentityOp(),
				box<op::atom>(Redex.OutputReg,Atom->AtomValue)
			));
		else if(auto Identifier=Syntax.Cast<syntax::identifier>(); Identifier && !Identifier->Qualifier) {
			if(Identifier->Name=="_") // Rule (UnderscoreSyntax).
				return Redex.ReduceStep(Step0,
					//!!Translate to :any so abstraction works correctly
					Redex.MakeInputIdentityOp()
				);
			else // Rule (IdentSyntax).
				return Redex.ReduceStep(Step0,
					MakeIdentifierOp(Redex,Identifier->Name,false)
				);
		}
		else if(auto Call=Syntax.Cast<syntax::call>(); Call && (Call->CallMode==mode::Open || Call->CallMode==mode::Closed)) {
			auto FunctionRedex  = Redex.FreshRedex("CallFunction"_VS,Call->FunctionSyntax);
			auto ParameterRedex = Redex.FreshRedex("CallParameter"_VS,Call->ParameterSyntax);
			auto AllowFx        = FrameAllowFx(Redex.Scopes);
			return Redex.ReduceStep(Step0,box<op::sequence>(
				box<op::exists>(FunctionRedex,ParameterRedex),
				FunctionRedex.Op,
				ParameterRedex.Op,
				Call->CallMode==mode::Closed?
					// Rule (CallClosedSyntax).
					box<op>(box<op::sequence>(
						Redex.MakeInputIdentityOp(),
						box<op::call>(Redex.OutputReg,FunctionRedex.OutputReg,ParameterRedex.OutputReg,False)
					)):
				    // Rule (CallOpenSyntax).
					box<op::check>(succeeds&AllowFx,Truth(P00(Locus)),Redex.OutputReg,
						box<op::reduce>(Redex,1,CaptureString("Call-Of-reduce"),[=](const redex& Redex1,const step& Step1)->current_step {
							auto CheckOutputReg    = Redex1.FixedReg(0);
							auto CheckFunctionReg  = Redex1.FreshReg("OfFunction"_VS);
							auto CheckParameterReg = Redex1.FreshReg("OfParameter"_VS);
							return Redex1.ReduceStep(Step1,box<op::sequence>(
								box<op::exists>(CheckFunctionReg,CheckParameterReg),
								::MakeInputIdentityOp(Redex1.Scopes,Redex1.InputReg,CheckOutputReg),
								box<op::unify>(CheckFunctionReg,FunctionRedex.OutputReg),
								box<op::unify>(CheckParameterReg,ParameterRedex.OutputReg),
								box<op::call>(CheckOutputReg,CheckFunctionReg,CheckParameterReg,False)
							));
						})
					)
			));
		}
		else if(Call && Call->CallMode==mode::With) {
			// Rule (FunctionSpecSyntax):
			// Turn t<fx> into runtime t, verifier type_fx_abstraction[t,fx].
			auto TypeRedex      = Redex.FreshRedex("WithFunction"_VS,Call->FunctionSyntax);
			auto FxRedex        = Redex.FreshRedex("WithParameter"_VS,Call->ParameterSyntax);
			auto AbstractionReg = Redex.FreshReg("WithResult"_VS);
			auto LocalNativeReg = Redex.FreshReg("LocalNative"_VS);
			auto ParametersReg  = Redex.FreshReg("AbsParameters"_VS);
			auto [AbsExistsRegs,AbsRegs,AbsOp] = Redex.FreshArrayRegs(ParametersReg,2,"AbsParameters"_VS);
			return Redex.ReduceStep(Step0,box<op::sequence>(
				box<op::exists>(TypeRedex,FxRedex,AbstractionReg,ParametersReg,AbsExistsRegs,LocalNativeReg),
				TypeRedex.Op,
				box<op::conditional>(false,false,true,box<op::unify>(AbstractionReg,TypeRedex.OutputReg)),
				box<op::conditional>(true,true,false,box<op::sequence>(
					FxRedex.Op, //!! wrap in non-paraconsistent verify demanding only_succeeds like other specifiers
					AbsOp,
					box<op::unify>(LocalNativeReg,NativeReg(Redex.Scopes[0],"type_fx_abstraction")),
					box<op::unify>(AbsRegs[0],TypeRedex.OutputReg),
					box<op::unify>(AbsRegs[1],FxRedex.OutputReg),
					box<op::call>(AbstractionReg,LocalNativeReg,ParametersReg,False)
				)),
				Redex.MakeInputIdentityOp(),
				box<op::unify>(Redex.OutputReg,AbstractionReg)
			));
		}
		else if(auto Invoke=Syntax.Cast<syntax::invoke>()) {
			// Rule (InvokeSyntax1), (InvokeSyntax2).
			auto MacroRedex = Redex.FreshRedex("InvokeMacro"_VS,Invoke->InvokeMacro);
			return Redex.ReduceStep(Step0,box<op::sequence>(
				box<op::exists>(MacroRedex),
				box<op::conditional>(true,false,false,MacroRedex.Op),
				box<op::reduce>(Redex,0,CaptureString("Invoke-Reduce"),[=](const redex& Redex1,const step& Step1)->current_step {
					auto MacroVertex  = MacroRedex.LoadOutput();
					return WhenCastStep<any>(
						[=]{return M01(Redex.Locus,"Invoke-WhenCastStep"_VS);},
						effects,MacroVertex,option<vertex>(False),Step1,
						[=](const any& Value,const step& Step2)->current_step {
							if(auto Macro=Value.Cast<macro_vertex>())
								return Macro->Invoke(Redex1,Invoke->Clause,Invoke->DoClause,Invoke->PostClause,Step2);
							//if(auto AllowFx=Value.Cast<fx>(); AllowFx && (!DoClause && !PostClause))
							//	OLD: fx{expr} was like check<fx>{expr}.
							// TODO: support function_vertex, passing archetype.
							return Stuck(effects,M00(Redex.Locus,"InvokeMacroStep-Stall"_VS)), Step2;
						},
						false,true
					);
				})
			));
		}
		else if(auto Escape=Syntax.Cast<syntax::escape>())
			VERSE_ERR("syntax.escape in literal code {..} can't be executed");
		VERSE_UNEXPECTED;
	}));
}

// Desugar macro clause.
redex redex::WithOp(const clause& Clause) const {
	// Rules (SequenceSyntax), (TupleSyntax).
	auto Redex  = WithLocus(Locus.Else(Clause.ClauseLocus));
	auto n      = Length(Clause.Body);
	option<box<op>> Op1;
	if(n==0 || Clause.Form==form::Commas) {
		auto [InputExistsRegs ,InputRegs ,InputOp ] = Redex.WithInputScopes().FreshArrayRegs(Redex.InputReg,n,*InputReg.RegName);
		auto [OutputExistsRegs,OutputRegs,OutputOp] = Redex                  .FreshArrayRegs(Redex.OutputReg,n,*OutputReg.RegName);
		return WithOp(box<op::sequence>(
			array<box<op>>{
				box<op::enter>(Redex.InputReg,box<op::sequence>(
					box<op::exists>(InputExistsRegs),
					InputOp
				)),
				box<op::exists>(OutputExistsRegs),
				OutputOp
			} +
			For(n,[&,&InputRegs=InputRegs,&OutputRegs=OutputRegs](nat i) {
				return Redex.FreshRedex(InputRegs[i],OutputRegs[i],Clause.Body[i]).Op;
			})
		));
	}
	else return WithOp(box<op::sequence>(For(n,[&](nat i) {
		if(i<n-1) {
			auto SequenceRedex=Redex.FreshRedex(ToString("Sequence_",i),Clause.Body[i]);
			return box<op>(box<op::sequence>(
				box<op::exists>(SequenceRedex),
				SequenceRedex.Op
			));
		}
		else return Redex.NewAlias(Clause.Body[i]).Op;
	})));
}

// Desugaring definitions.
box<op> MakeDefineIdentifierOpBatch(const redex& Redex,const reg& SymbolReg,const string& Identifier) {
	if(Identifier=="_")
		return box<op::sequence>();
	if(Redex.Scopes.End(0)->ScopeSymbols->SetBatch(Identifier,SymbolReg))
		return Stuck(effects,N04(Redex.Locus,Identifier)), box<op::sequence>();
	else {
		SymbolReg.RegName = Identifier;
		auto Redex1       = Redex.FreshRedex("CheckIdentifier"_VS,box<op::sequence>());
		return box<op::conditional>(true,false,false,box<op::sequence>(
			box<op::exists>(Redex1),
			MakeIdentifierOp(Redex1,Identifier,true)
		));
	}
}
string FreshHygenicIdentifier(const string& S) {
	static nat i=0;
	return ToString("_",S,i++);//!!todo: real hygiene
}
future<box<syntax>> UnwrapColons(const locus& Locus,const future<box<syntax>>& s2,const future<box<syntax>>& p) {
	return WhenResolve(Here(Locus,"UnwrapColons"_VS),effects,s2,[=](const box<syntax>& s3)->box<syntax> {
		if(auto RightInvoke=s3.Cast<syntax::invoke>(); RightInvoke && IsNative(RightInvoke->InvokeMacro,u8"prefix':'") && Length(RightInvoke->Clause.Body)==1 && !RightInvoke->Clause.Specifiers && !RightInvoke->DoClause && !RightInvoke->PostClause) {
			auto r=UnwrapColons(Locus,RightInvoke->Clause.Body[0],p);
			return box<syntax::invoke>(Locus,MakeNativeSyntax("prefix':'"),
				clause{locus{},False,array{r}}
			);
		}
		return box<syntax::call>(locus{},mode::Closed,s3,p);
	});
}
current_step ReduceDefinitionStep(const redex& Redex,const future<box<syntax>>& LeftFuture,const array<future<box<syntax>>>& Specifiers,const clause& RightClause,const step& Step0) {
	return WhenResolveStep(Here(Redex.Locus,"operator':='"_VS),effects,LeftFuture,Step0,[=](const box<syntax>& Left,const step& Step1)->current_step {
		if(RightClause.Specifiers)
			return Stuck(effects,D01(Left->Locus.Else(Redex.Locus),"ReduceDefinitionStep"_VS)), Step1;

		// Handle Ident:=s1.
		if(auto Identifier=Left.Cast<syntax::identifier>(); Identifier && !Identifier->Qualifier && !Specifiers) {
			// Rules (IdentDefine) and (UnderscoreDefine).
			if(Identifier->Name!="_") {
				Redex.OutputReg.RegName=Identifier->Name;
				Redex.InputReg.RegName=Identifier->Name+"_i";
			}
			auto DefineOp = MakeDefineIdentifierOpBatch(Redex.WithLocus(Identifier->Locus),Redex.OutputReg,Identifier->Name);
			return Redex.ReduceStep(Step1,box<op::sequence>(
				DefineOp,
				Redex.NewAlias(RightClause).Op
			));
		}

		// Rules of the form (*DefineSpec) handle s0<s1>:=s2:
		if(auto Call=Left.Cast<syntax::call>(); Call && Call->CallMode==mode::With)
			return ReduceDefinitionStep(Redex,Call->FunctionSyntax,Specifiers+array{Call->ParameterSyntax},RightClause,Step1);

		// Rule (CallOpenDefine): s0(s1)Specs:=s2.
		// Make clause because syntax.call stores parameters as future<syntax> rather than future<clause>.
		// Grammar itself passes in a clause; consider refactoring along with macros and unquote-splicing.
		if(auto Call=Left.Cast<syntax::call>(); Call && Call->CallMode==mode::Open) {
			auto Function=box<syntax::invoke>(Redex.Locus,MakeNativeSyntax(Call->CallMode==mode::Open? "function": "map"),
				clause{locus{},False,array{Call->ParameterSyntax}},
				Truth(clause{RightClause.ClauseLocus,Specifiers,RightClause.Body,RightClause.Form})
			);
			return ReduceDefinitionStep(Redex,Call->FunctionSyntax,False,clause{locus{},False,array{Function}},Step1);
		}

		// Rules (MulipleDefine): (a&b)Spec:=expr.
		// TODO: prefix'..'. TODO: handle n-ary.
		if(auto Macro=Left.Cast<syntax::invoke>(); Macro && !Macro->DoClause && !Macro->PostClause)
			if(IsNative(Macro->InvokeMacro,u8"operator'&'"))
				if(Length(Macro->Clause.Specifiers)==0 && Length(Macro->Clause.Body)==2) {
					auto ClauseLocus = Macro->Clause.ClauseLocus.Else(Redex.Locus);
					auto Copies      = Macro->Clause.Body.For([&](const future<box<syntax>>& B) {
						return box<syntax::invoke>(Redex.Locus,MakeNativeSyntax("operator':='"),clause{ClauseLocus,Specifiers,array{B}},Truth(RightClause));
					});
					return Redex.ReduceStep(Step1,box<syntax::invoke>(Redex.Locus,MakeNativeSyntax("array"),clause{locus{},False,Copies}));
				}

		// Rules (ArrowInDefine): (a->b)Spec:expr.
		// Supports nesting i->j->k::xss meaning x:=xss[i:any][j:any].
		if(auto LeftInvoke=Left.Cast<syntax::invoke>(); LeftInvoke && IsNative(LeftInvoke->InvokeMacro,u8"operator'->'") && Length(LeftInvoke->Clause.Specifiers)==0 && Length(LeftInvoke->Clause.Body)==2 && !LeftInvoke->DoClause && !LeftInvoke->PostClause)
			if(!RightClause.Specifiers && Length(RightClause.Body)==1)
				return WhenResolveStep(Here(Redex.Locus,"operator'->'"_VS),effects,RightClause.Body[0],Step1,[=](const box<syntax>& Right,const step& Step2)->current_step {
					if(auto RightInvoke=Right.Cast<syntax::invoke>(); RightInvoke && IsNative(RightInvoke->InvokeMacro,u8"prefix':'") && Length(RightInvoke->Clause.Body)==1 && !RightInvoke->Clause.Specifiers && !RightInvoke->DoClause && !RightInvoke->PostClause) {
						// S(c,i,x,s0->s1:...s2) ---> scope j. R(c,j,x,s1:=...s2[s0:=i])
						string is=FreshHygenicIdentifier("i"_VS);
						auto   s0=LeftInvoke->Clause.Body[0], s1=LeftInvoke->Clause.Body[1], s2=RightInvoke->Clause.Body[0];
						auto   i=Redex.InputReg, x=Redex.OutputReg, j=Redex.FreshReg("NewInput"_VS);
						//!!handling specifiers adequately?
						return Redex.ReduceStep(Step2,box<op::sequence>(
							box<op::exists>(j),
							MakeDefineIdentifierOpBatch(Redex,i,is),
							Redex.FreshRedex(j,x,
								MakeDefineSyntax(
									locus{},
									s1,
									UnwrapColons(Redex.Locus,s2,MakeDefineSyntax(locus{},s0,box<syntax::identifier>(locus{},False,is)))
								)
							).Op
						));
					}
					else if(RightInvoke && IsNative(RightInvoke->InvokeMacro,u8"operator'where'") && Length(RightInvoke->Clause.Body)==1 && !RightInvoke->Clause.Specifiers && RightInvoke->DoClause && !RightInvoke->PostClause) {
						if(auto RightLeftInvoke=RightInvoke->Clause.Body[0].Cast<syntax::invoke>(); RightLeftInvoke && IsNative(RightLeftInvoke->InvokeMacro,u8"prefix':'") && Length(RightLeftInvoke->Clause.Body)==1 && !RightLeftInvoke->Clause.Specifiers && !RightLeftInvoke->DoClause && !RightLeftInvoke->PostClause) {
							// (WhereInSyntax) translates Left:=(:a where b) -> Left:=:(a where b)
							auto NewWhereSyntax = box<syntax::invoke>(locus{},RightInvoke->InvokeMacro,clause{locus{},False,array{RightLeftInvoke->Clause.Body[0]}},RightInvoke->DoClause);
							auto NewInSyntax    = box<syntax::invoke>(locus{},RightLeftInvoke->InvokeMacro,clause{locus{},False,array{NewWhereSyntax}});
							return ReduceDefinitionStep(Redex,Left,Specifiers,clause{locus{},False,array{NewInSyntax}},Step0);
						}
					}
					return Stuck(effects,D01(Left->Locus.Else(Redex.Locus),"ReduceDefinitionStep"_VS)), Step1;
				});

		// Transform s0:s1:=s2 into macro form.
		// TODO: Following spec and parser update, plumb :t:v, :t:v=w differently.
		if(auto Macro=Left.Cast<syntax::invoke>(); Macro && IsNative(Macro->InvokeMacro,u8"prefix':'") && Length(Macro->Clause.Body)==1 && !Macro->Clause.Specifiers && !Macro->DoClause && !Macro->PostClause)
			return Redex.ReduceStep(Step1,box<syntax::invoke>(Redex.Locus,MakeNativeSyntax("prefix':'"),
				clause{locus{},Specifiers,Macro->Clause.Body},
				Truth(RightClause)
			));

		// No match.
		return Stuck(effects,D01(Left->Locus.Else(Redex.Locus),"ReduceDefinitionStep"_VS)), Step1;
	});
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Native language macros.

current_step ArrayMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rules (ArrayMacro), (ArrayListMacro).
	if(!DoClause && !PostClause) {
		auto Clause1 = Clause;
		Clause1.Form = form::Commas;
		return Redex.ReduceStep(Step0,
			Redex.NewAlias(Clause1).Op
		);
	}
	return Stuck(effects,M02(Redex.Locus,"array")), Step0;
}
current_step DeoptionMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (DeoptionSyntax).
	if(!Clause.Specifiers && !DoClause && !PostClause) {
		auto Option       = Redex.FreshRedex("DeoptionOption"_VS,Clause);
		auto ParameterReg = Redex.FreshReg("DeoptionInput"_VS);
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(Option,ParameterReg),
			Option.Op,
			Redex.MakeInputIdentityOp(),
			box<op::call>(Redex.OutputReg,Option.OutputReg,ParameterReg,False)
		));
	}
	return Stuck(effects,M02(Redex.Locus,"operator'?'")), Step0;
}
current_step OperatorDefineMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rules (*Define).
	if(!Clause.Specifiers && Length(Clause.Body)==1 && DoClause && !PostClause)
		return ReduceDefinitionStep(Redex,Clause.Body[0],False,DoClause.Coerce(),Step0);
	return Stuck(effects,M02(Redex.Locus,"operator':='")), Step0;
}
current_step UnifyMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (UnifySyntax).
	if(!DoClause && !PostClause) {
		return Redex.ReduceStep(Step0,box<op::sequence>(For(Length(Clause.Body),[&](nat i) {
			auto Redex1=Redex.WithLocus(Clause.ClauseLocus);
			// Must create new variable, else we're continually updating the existing one.
			// But it would be better if we had redex::RedexOutputName so we could keep our input and output name
			// while ensuring new arrays and lambdas created by our clauses receive the new redex name.
			//if(*Redex1.InputReg .RegName) static_cast<future<>&>(Redex1.InputReg .RegName)=var<string>(ToString(*Redex1.InputReg .RegName,"_",i));
			//if(*Redex1.OutputReg.RegName) static_cast<future<>&>(Redex1.OutputReg.RegName)=var<string>(ToString(*Redex1.OutputReg.RegName,"_",i));
			return Redex1.NewAlias(Clause.Body[i]).Op;
		})));
	}
	return Stuck(effects,M02(Redex.Locus,"operator'='")), Step0;
}
current_step OperatorChoiceMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (ChoiceSyntax).
	if(Length(Clause.Body)==2 && !DoClause && !PostClause) {
		auto Ops=For(Length(Clause.Body),[&](nat i) {
			auto ChoiceSyntax=Clause.Body[i];
			return box<op::scope>(effects,
				box<op::reduce>(Redex,1,CaptureString("choice{",ChoiceSyntax,"}"),[=](const redex& Redex1,const step& Step1)->current_step {
					return Redex1.ReduceStep(Step1,
						Redex1.NewAlias(ChoiceSyntax).Op
					);
				})
			);
		});
		return Redex.ReduceStep(Step0,
			box<op::choice>(Ops[0],Ops[1])
		);
	}
	return Stuck(effects,M02(Redex.Locus,"operator'|'")), Step0;
}
current_step PrefixInMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rules (InSyntax), (StageSyntax).
	auto Abstraction   = Redex.FreshRedex("InAbstraction"_VS,Clause);
	auto NativeFuncReg = Redex.FreshReg("InLocalNative"_VS);
	auto FunctionReg   = Redex.FreshReg("InFunction"_VS);
	if(!Clause.Specifiers && !DoClause && !PostClause && !Redex.FarInput()) {
		// Optimization where (InSolveElim) is the only possibility.
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(Abstraction,FunctionReg,NativeFuncReg),
			Abstraction.Op,
			box<op::unify>(NativeFuncReg,NativeReg(Redex.Scopes[0],"abstraction_type")),
			box<op::call>(FunctionReg,NativeFuncReg,Abstraction.OutputReg,False),
			box<op::call>(Redex.OutputReg,FunctionReg,Redex.InputReg,False)
		));
	}
	if(!Clause.Specifiers && (!DoClause || !DoClause->Specifiers) && !PostClause) {
		auto EnterOutputReg   = Redex.WithInputScopes().FreshReg("EnterOutput"_VS);
		auto EnterFunctionReg = Redex.WithInputScopes().FreshReg("EnterFunction"_VS);
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(Abstraction,FunctionReg,NativeFuncReg),
			Abstraction.Op,
			box<op::unify>(NativeFuncReg,NativeReg(Redex.Scopes[0],"abstraction_type")),
			box<op::call>(FunctionReg,NativeFuncReg,Abstraction.OutputReg,False),
			DoClause?
				box<op>(box<op::stage>(FreshStageSite(),Redex.InputReg,Redex.OutputReg,Abstraction.OutputReg,
					box<op::reduce>(Redex,2,CaptureString("StageAbstraction",Clause),[=](const redex& AbstractionRedex,const step& Step2)->current_step {
						// We run this both in Redex.InputReg context (to produce abstraction) and in stage verify context (to check type).
						//!! in stage verify: must make a new ComposeContext
						AbstractionRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
						auto AbstractionInputReg    = AbstractionRedex.FixedReg(0);
						auto AbstractionOutputReg   = AbstractionRedex.FixedReg(1);
						auto AbstractionFunctionReg = AbstractionRedex.FixedReg(2);
						return AbstractionRedex.ReduceStep(Step2,box<op::sequence>(
							box<op::unify>(AbstractionFunctionReg,FunctionReg),
							box<op::call>(AbstractionOutputReg,AbstractionFunctionReg,AbstractionInputReg,False)
						));
					}),
					box<op::reduce>(Redex,2,CaptureString("StageVerify"),[=](const redex& CheckRedex,const step& Step3)->current_step {
						CheckRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
						auto CheckInputReg          = CheckRedex.FixedReg(0);
						auto DoInputReg             = CheckRedex.WithEndSliceScopes(1).FixedReg(0);
						auto DoRedex                = CheckRedex.FreshRedex(DoInputReg,CheckInputReg,DoClause.Coerce());
						return CheckRedex.ReduceStep(Step3,box<op::sequence>(
							//box<op::enter>(DoInputReg,box<op::unify>(DoInputReg,Redex.InputReg)),
							DoRedex.Op
						));
					})
				)):
				box<op::sequence>(
					box<op::unify>(Redex.OutputReg,EnterOutputReg),
					box<op::enter>(Redex.InputReg,box<op::sequence>(
						box<op::exists>(EnterOutputReg,EnterFunctionReg),
						box<op::unify>(EnterFunctionReg,FunctionReg),
						box<op::hold>(Redex.InputReg,EnterOutputReg,Abstraction.OutputReg),
						box<op::call>(EnterOutputReg,EnterFunctionReg,Redex.InputReg,False)
					))
				)
		));
	}
	return Stuck(effects,M02(Redex.Locus,"prefix':'")), Step0;
}
template<fx DefaultAllowFx> current_step FunctionMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rules (FunctionMacro), (LambdaArrowMacro).
	if(!PostClause && (!DoClause || !Clause.Specifiers))
		return VerifySpecs(Redex,true,function_allows,DoClause? DoClause->Specifiers: Clause.Specifiers,Step0,[=](const redex& Redex,fx KeepFx,fx AddFx,option<function_specifiers> FunctionSpecifiers,const step& Step1)->current_step {
			auto LambdaInputReg  = Redex.FreshReg("InputLambda"_VS);
			auto LambdaAllowFx   = (DefaultAllowFx&KeepFx)+AddFx;
			return Redex.ReduceStep(Step1,box<op::sequence>(
				box<op::exists>(LambdaInputReg),
				box<op::lambda>(LambdaAllowFx,Redex.OutputReg,Redex.InputReg,LambdaInputReg,
					function_specifiers::Closed<=FunctionSpecifiers.Else(function_specifiers::Open),
					box<op::reduce>(Redex,2,CaptureString("function_domain",Clause),[=](const redex& DomainRedex,const step& Step2)->current_step {
						DomainRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
						auto DomainInputReg    = DomainRedex.WithEndSliceScopes(1).FixedReg(0);
						auto DomainOutputReg   = DomainRedex.FixedReg(0);
						return DomainRedex.ReduceStep(Step2,
							DomainRedex.FreshRedex(DomainInputReg,DomainOutputReg,Clause).Op
						);
					}),
					box<op::reduce>(Redex,4,DoClause? CaptureString("function_range",DoClause.Coerce()): CaptureString("function_range"),[=](const redex& BetaRedex,const step& Step2)->current_step {
						BetaRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
						auto DomainOutputReg  = BetaRedex.WithEndSliceScopes(2).FixedReg(0);
						auto ComposeLambdaReg = BetaRedex.WithEndSliceScopes(1).FreshReg("ComposeLambda"_VS);
						auto ComposeInputReg  = BetaRedex.WithEndSliceScopes(1).FreshReg("ComposeInput"_VS);
						auto ComposeOutputReg = BetaRedex.WithEndSliceScopes(1).FreshReg("ComposeOutput"_VS);
						auto RangeOutputReg   = BetaRedex.FixedReg(0);
						auto Range            = DoClause?
							BetaRedex.FreshRedex(ComposeOutputReg,RangeOutputReg,DoClause.Coerce()):
							BetaRedex.FreshRedex(ComposeOutputReg,RangeOutputReg,DomainOutputReg);
						return BetaRedex.ReduceStep(Step2,
							box<op::beta>(
								ComposeOutputReg,
								box<op::sequence>(
									box<op::exists>(ComposeOutputReg,ComposeLambdaReg,ComposeInputReg),
									box<op::unify>(ComposeLambdaReg,Redex.InputReg),
									box<op::unify>(ComposeInputReg,DomainOutputReg),
									box<op::call>(ComposeOutputReg,ComposeLambdaReg,ComposeInputReg,Truth(LambdaInputReg))
								),
								Range.Op
							)
						);
					})
				))
			);
		});
	return Stuck(effects,M02(Redex.Locus,"function")), Step0;
}
current_step TypeMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rules (TypeMacro).
	if(!DoClause && !PostClause)
		return FunctionMacro<type_defaults>(Redex,Clause,False,False,Step0);
	return Stuck(effects,M02(Redex.Locus,"type")), Step0;
}
current_step LetMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (LetMacro).
	if(!Clause.Specifiers && DoClause && !PostClause) {
		return Redex.ReduceStep(Step0,
			box<op::scope>(effects,
				box<op::reduce>(Redex,1,CaptureString("LetDomain"),[=](const redex& Redex1,const step& Step1)->current_step {
					auto DomainRedex = Redex1.FreshRedex("LetDomain"_VS,Clause);
					return Redex1.ReduceStep(Step1,box<op::sequence>(
						box<op::exists>(DomainRedex),
						DomainRedex.Op,
						box<op::scope>(effects,
							box<op::reduce>(Redex1,1,CaptureString("LetRange"),[=](const redex& Redex2,const step& Step2)->current_step {
								auto RangeRedex = Redex2.NewAlias(DoClause.Coerce());
								return Redex2.ReduceStep(Step2,
									RangeRedex.Op
								);
							})
						)
					));
				})
			)
		);
	}
	return Stuck(effects,M02(Redex.Locus,"let")), Step0;
}
current_step AssumingMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (AssumingMacro).
	if(!Clause.Specifiers && DoClause && !PostClause) {
		auto Left  = Redex.NewAlias(Clause);
		auto Right = Redex.FreshRedex("AssumingRight"_VS,DoClause.Coerce());
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(Right),
			Left.Op,
			Right.Op
		));
	}
	return Stuck(effects,M02(Redex.Locus,"where")), Step0;
}
current_step WhereMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (WhereMacro).
	if(!Clause.Specifiers && DoClause && !PostClause) {
		auto Left  = Redex.NewAlias(Clause);
		auto Right = Redex.FreshRedex("WhereRight"_VS,DoClause.Coerce());
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(Right),
			Left.Op,
			Right.Op
		));
	}
	return Stuck(effects,M02(Redex.Locus,"where")), Step0;
}
current_step IfMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (IfThenElseMacro), (IfThenMacro), (IfMacro), (IfElseMacro).
	// Supported forms: if{a} [else ..], if(a){b} [else ..], if(a) then b [else ..]
	if(!Clause.Specifiers)
		return Redex.ReduceStep(Step0,
			MakeConditionalOp("if",Redex,Clause,DoClause,false,Truth(PostClause.Else(clause{})))
		);
	return Stuck(effects,M02(Redex.Locus,"if")), Step0;
}
current_step FirstMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (FirstDoMacro), (FirstMacro).
	if(!Clause.Specifiers && !PostClause)
		return Redex.ReduceStep(Step0,
			MakeConditionalOp("first",Redex,Clause,DoClause,false,False)
		);
	return Stuck(effects,M02(Redex.Locus,"first")), Step0;
}
current_step OperatorOrMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (OrMacro): a or b ---> if{a}else{b}, making one of a but multiple of b.
	// Could be a or b ---> first{a|b}, but that is non-uniform as we lack 1-ary or.
	if(Length(Clause.Body)==2 && !Clause.Specifiers && !DoClause && !PostClause)
		return Redex.ReduceStep(Step0,
			MakeConditionalOp("or",Redex,clause{locus{},False,array{Clause.Body[0]}},False,false,Truth(clause{locus{},False,array{Clause.Body[1]}}))
		);
	return Stuck(effects,M02(Redex.Locus,"operator'or'")), Step0;
}
current_step OperatorAndMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (AndMacro): a and b ---> let(a){b} with no cross-visible symbols
	if(Length(Clause.Body)==2 && !Clause.Specifiers && !DoClause && !PostClause)
		VERSE_ERR("TODO");
		//return Redex.ReduceStep(Step0,
		//	MakeConditionalOp(Redex,clause{locus{},False,array{Clause.Body[0]},form::List},False,false,Truth(clause{locus{},False,array{Clause.Body[1]}}))
		//);
	return Stuck(effects,M02(Redex.Locus,"operator'and'")), Step0;
}
current_step PrefixNotMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (NotMacro).
	if(!DoClause && !PostClause)
		return Redex.ReduceStep(Step0,
			MakeConditionalOp("not",Redex,Clause,False,true,Truth(clause{}))
		);
	return Stuck(effects,M02(Redex.Locus,"not")), Step0;
}
current_step OperatorRangeMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (RangeSyntax).
	if(Length(Clause.Body)==2 && !Clause.Specifiers && !DoClause && !PostClause) {
		auto FirstTarget = Redex.FreshRedex("RangeStart"_VS,Clause.Body[0]);
		auto LastTarget  = Redex.FreshRedex("RangeStop"_VS,Clause.Body[1]);
		auto ValueReg    = Redex.FreshReg("Value"_VS);
		return Redex.ReduceStep(Step0,box<op::sequence>{
			box<op::exists>(FirstTarget,LastTarget,ValueReg),
			FirstTarget.Op,
			LastTarget.Op,
			Redex.MakeInputIdentityOp(),
			box<op::unify>(Redex.OutputReg,ValueReg),
			box<op::range>(ValueReg,FirstTarget.OutputReg,LastTarget.OutputReg)
		});
	}
	return Stuck(effects,M02(Redex.Locus,"operator'and'")), Step0;
}
current_step ForMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (ForDoMacro), (ForMacro).
	if(!Clause.Specifiers && !PostClause) {
		auto ZeroReg = Redex.FreshReg("Zero"_VS);
		return Redex.ReduceStep(Step0,box<op::sequence>(
			box<op::exists>(ZeroReg),
			box<op::unify>(ZeroReg,reg(0)),
			MakeIterateOp("for",Redex,ZeroReg,
				Clause,
				[=](const redex& SucceedsRedex,const reg& DomainOutputReg,const reg& CurrentValueReg,const reg& NextValueReg,const step& Step1)->current_step {
					auto ElementInputReg      = SucceedsRedex.WithEndSliceScopes(1).FreshReg("ElementInput"_VS);
					auto ElementOutputReg     = SucceedsRedex.                      FreshReg("ElementOutput"_VS);
					auto Element              = DoClause?
						SucceedsRedex.FreshRedex(ElementInputReg,ElementOutputReg,DoClause.Coerce()):
						SucceedsRedex.FreshRedex(ElementInputReg,ElementOutputReg,DomainOutputReg);
					auto EnterCurrentValueReg = SucceedsRedex.WithEndSliceScopes(1).FreshReg("EnterCurrentValue"_VS);
					auto AddFunReg            = SucceedsRedex.FreshReg("LenFun"_VS);
					auto AddParameterReg      = SucceedsRedex.FreshReg("AddParameter"_VS);
					auto [AddParameterExistsRegs,AddParameterRegs,AddParameterOp] = SucceedsRedex.FreshArrayRegs(AddParameterReg,2,"Add"_VS);
					auto [NextExistsRegs        ,NextRegs        ,NextOp        ] = SucceedsRedex.FreshArrayRegs(NextValueReg,1,"Val"_VS);
					return SucceedsRedex.ReduceStep(Step1,box<op::sequence>(
						box<op::exists>(Element.OutputReg,AddFunReg,AddParameterReg,AddParameterExistsRegs,NextExistsRegs),

						// Evaluate element.
						box<op::enter>(Redex.InputReg,box<op::sequence>(
							box<op::exists>(Element.InputReg,EnterCurrentValueReg),
							box<op::unify>(EnterCurrentValueReg,CurrentValueReg),
							box<op::call>(Element.InputReg,Redex.InputReg,EnterCurrentValueReg,False)
						)),
						box<op::call>(Element.OutputReg,Redex.OutputReg,CurrentValueReg,False),
						Element.Op,

						// Evaluate next index index.
						AddParameterOp,
						box<op::unify>(AddFunReg,NativeReg(Redex.Scopes[0],"operator'+'")),
						box<op::unify>(AddParameterRegs[0],CurrentValueReg),
						box<op::unify>(AddParameterRegs[1],reg(1)),
						box<op::call>(NextRegs[0],AddFunReg,AddParameterReg,False),
						NextOp // Triggers next iteration.
					));
				},
				[=](const redex& FailsRedex,const reg& CurrentValueReg,const step& Step1)->current_step {
					auto EnterCurrentValueReg = FailsRedex.WithInputScopes().FreshReg("EnterCurrentValue"_VS);
					return FailsRedex.ReduceStep(Step1,box<op::sequence>(
						box<op::enter>(Redex.InputReg,box<op::sequence>(
							box<op::exists>(EnterCurrentValueReg),
							box<op::unify>(EnterCurrentValueReg,CurrentValueReg),
							box<op::length>(Redex.InputReg,EnterCurrentValueReg)
						)),
						box<op::length>(Redex.OutputReg,CurrentValueReg)
					));
				}
			)
		));
	}
	// TODO: support "until" PostClause, maybe continue & break.
	return Stuck(effects,M02(Redex.Locus,"for")), Step0;
}
current_step OperatorNotEqualMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (NotEqualMacro): x:=a<>e means x:=a where !x=e
	return Stuck(effects,M02(Redex.Locus,"operator'<>'")), Step0;
}
current_step ForAllMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	if(!Clause.Specifiers && !PostClause)
		// Consider adding op::forall-otherwise-fails with new special case for only-one-non-fails-domain-iteration,
		// where we can release range with strength modulated by domain.
		return Redex.ReduceStep(Step0,MakeIterateOp("forall",Redex,Redex.InputReg,
			Clause,
			[=](const redex& SucceedsRedex,const reg& DomainOutputReg,const reg& CurrentValueReg,const reg& NextValuesReg,const step& Step1)->current_step {
				auto Element             = DoClause?
					SucceedsRedex.NewAlias(DoClause.Coerce()):
					SucceedsRedex.NewAlias(DomainOutputReg);
				auto [ValExistsRegs,ValRegs,ValOp] = SucceedsRedex.FreshArrayRegs(NextValuesReg,1,"Next"_VS);
				return SucceedsRedex.ReduceStep(Step1,box<op::sequence>(
					box<op::exists>(ValExistsRegs),
					Element.Op,
					box<op::unify>(ValRegs[0],Redex.OutputReg),
					ValOp
				));
			},
			[=](const redex& FailsRedex,const reg& CurrentValueReg,const step& Step1)->current_step {
				return FailsRedex.ReduceStep(Step1,
					box<op::unify>(Redex.OutputReg,CurrentValueReg) // If all fails, unify input&output, else trivial. !!will be inconsistent
				);
			}
		));
	// TODO: support "otherwise" PostClause for empty-domain case.
	return Stuck(effects,M02(Redex.Locus,"forall")), Step0;
}
current_step OperatorDotMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (DotIdentSyntax).
	if(Length(Clause.Body)==2 && !DoClause && !PostClause) {
		auto ClauseLocus = Clause.ClauseLocus.Else(Redex.Locus);
		if(auto Id=Clause.Body[1].Cast<syntax::identifier>(); Id && !Id->Qualifier) {
			auto Argument = Redex.WithLocus(ClauseLocus).FreshRedex("DotArgument"_VS,Clause.Body[0]);
			return Redex.ReduceStep(Step0,box<op::sequence>(
				box<op::exists>(Argument),
				Argument.Op,
				box<op::reduce>(Redex,0,CaptureString("OperatorDotMacroVerify"),[=](const redex& Redex1,const step& Step1)->current_step {
					auto Describe       = [=]{return N05(ClauseLocus.Else(Id->Locus),Id->Name,"OperatorDotMacro-WhenCastStep"_VS);};
					auto ArgumentVertex = Argument.LoadOutput();
					return Redex1.ReduceStep([=]()->current_step {
						return WhenCastStep<falsity>(
							Describe,abstracts+reads,ArgumentVertex,Truth(Redex.LoadOutput()),Step1,
							[=](const falsity& Falsity,const step& Step2)->current_step {
								return Step2;
							},
							false,true
						);
					});
				})
			));
		}
	}
	return Stuck(effects,M02(Redex.Locus,"operator'|'")), Step0;
}
current_step CheckMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (CheckMacro).
	if(!DoClause && !PostClause)
		return VerifySpecs(Redex,false,effects,Clause.Specifiers,Step0,[=](const redex& Redex,fx KeepFx,fx AddFx,option<function_specifiers> FunctionSpecifiers,const step& Step1)->current_step {
			return Redex.ReduceStep(Step1,box<op::sequence>(
				Redex.MakeInputIdentityOp(),
				box<op::check>((succeeds&KeepFx)+AddFx,False,Redex.OutputReg,
					box<op::reduce>(Redex,1,CaptureString("MakeFreshOpIn-Type",Clause),[=](const redex& Redex1,const step& Step1)->current_step {
						auto CheckInputReg  = Redex1.FreshReg("CheckInput"_VS);
						auto CheckOutputReg = Redex1.FixedReg(0);
						auto ClauseRedex    = Redex1.FreshRedex(CheckInputReg,CheckOutputReg,Clause);
						return Redex1.ReduceStep(Step1,box<op::sequence>(
							box<op::exists>(CheckInputReg),
							ClauseRedex.Op
						));
					})
				)
			));
		});
	return Stuck(effects,M02(Redex.Locus,"check")), Step0;
}
current_step AssertMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (AssertMacro).
	if(!DoClause && !PostClause)
		return Redex.ReduceStep(Step0,
			box<syntax::invoke>(
				Redex.Locus,MakeNativeSyntax("if"),Clause,Truth(clause{}),
				Truth(clause{Redex.Locus,False,array{box<syntax::call>(Redex.Locus,mode::Open,MakeNativeSyntax("Err"),MakeStringSyntax("Assertion Failed"_VS))}})
			)
		);
	return Stuck(effects,M02(Redex.Locus,"assert")), Step0;
}
current_step RejectMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (RejectMacro).
	if(!Clause.Specifiers && !DoClause && !PostClause) {
		return Redex.ReduceStep(Step0,
			box<op::scope>(effects,
				box<op::reduce>(Redex,1,CaptureString("RejectMacro-Argument"),[=](const redex& Redex1,const step& Step1)->current_step {
					auto Argument = Redex1.FreshRedex("RejectArgument"_VS,Clause);
					return Redex1.ReduceStep(Step1,box<op::sequence>(
						box<op::exists>(Argument),
						Argument.Op,
						box<op::reduce>(Redex1,0,CaptureString("RejectMacro-Check"),[=](const redex& Redex2,const step& Step2)->current_step {
							auto Describe      = [=]{return V02(Redex.Locus);};
							auto MessageVertex = Argument.LoadOutput();
							auto RedexVertex   = Redex.LoadOutput();
							return Redex2.ReduceStep([=]()->current_step {
								// DivergeTargetStep can't be right here as it's circular.
								return DivergeTargetStep("RejectMacroDiverge",Redex.Locus,RedexVertex,[=]()->current_step {
									return WhenCastStep<string>(
										Describe,succeeds&converges&computes&no_unifies,MessageVertex,option<vertex>(False),Step2,
										[=](const string& String,const step& Step3)->current_step {
											return Stuck(succeeds&converges&computes&no_unifies,V02(Redex.Locus,String)), Step3;
										},
										false,true
									);
								});
							});
						})
					));
				})
			)
		);
	}
	return Stuck(effects,M02(Redex.Locus,"reject")), Step0;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Native testing macros.

current_step AssumeMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (AssumeMacro).
	if(!DoClause && !PostClause) {
		return VerifySpecs(Redex,false,effects,Clause.Specifiers,Step0,[=](const redex& Redex,fx KeepFx,fx AddFx,option<function_specifiers> FunctionSpecifiers,const step& Step1)->current_step {
			// This testing construct is unsafe: it assumes something has effects which it may not have.
			return Redex.ReduceStep(Step1,box<op::sequence>(
				Redex.MakeInputIdentityOp(),
				box<op::assume>((succeeds&KeepFx)+AddFx+only_rejects,Redex.OutputReg,
					box<op::reduce>(Redex,2,CaptureString("AssumeMacro",Clause),[=](const redex& AssumeRedex,const step& Step2)->current_step {
						AssumeRedex.Scopes.End(1)->ScopeSymbols->EliminateBatch();
						auto AssumeInputReg  = AssumeRedex.WithEndSliceScopes(1).FixedReg(0);
						auto AssumeOutputReg = AssumeRedex.FixedReg(0);
						auto ClauseRedex     = AssumeRedex.FreshRedex(AssumeInputReg,AssumeOutputReg,Clause);
						return AssumeRedex.ReduceStep(Step2,box<op::sequence>(
							box<op::exists>(AssumeInputReg),
							ClauseRedex.Op
						));
					})
				)
			));
		});
	}
	return Stuck(effects,M02(Redex.Locus,"assume")), Step0;
}
current_step ExpectMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (ExpectMacro).
	if(!DoClause && !PostClause) {
		auto AllowFx=FrameAllowFx(Redex.Scopes);
		return VerifySpecs(Redex,false,effects,Clause.Specifiers,Step0,[=](const redex& Redex,fx KeepFx,fx AddFx,option<function_specifiers> FunctionSpecifiers,const step& Step1)->current_step {
			return Redex.ReduceStep(Step1,
				box<op::scope>(AllowFx&(KeepFx+AddFx),
					box<op::reduce>(Redex,1,CaptureString("ExpectMacro-Reduce"),[=](const redex& Redex2,const step& Step2)->current_step {
						return Redex2.ReduceStep(Step2,Clause);
					})
				)
			);
		});
	}
	return Stuck(effects,M02(Redex.Locus,"expect")), Step0;
}
current_step TestMacro(const redex& Redex,const clause& Clause,const option<clause>& DoClause,const option<clause>& PostClause,const step& Step0) {
	// Rule (TestMacro).
	if(!Clause.Specifiers && Length(Clause.Body)==1 && DoClause && !PostClause)
		if(auto Identifier=Clause.Body[0].Cast<syntax::identifier>(); Identifier && !Identifier->Qualifier) {
			string Code         = Identifier->Name;
			bool   Run          = Code=="S00" || Code.Slice(0,1)=="R";
			static nat Counter  = 100000;
			return Redex.ReduceStep(Step0,box<op::sequence>(
				Redex.NewAlias(reg(Counter++)).Op,
				box<op::conditional>(true,Run,Run,
					box<op::test>(Redex.Locus,Code,
						box<op::reduce>(Redex,1,CaptureString("TestCheck"),[=](const redex& Redex1,const step& Step1)->current_step {
							auto TestRedex = Redex1.FreshRedex("TestCheck"_VS,DoClause.Coerce());
							return Redex1.ReduceStep(Step1,box<op::sequence>(
								box<op::exists>(TestRedex),
								TestRedex.Op
							));
						})
					)
				)
			));
		}
	return Stuck(effects,M02(Redex.Locus,"test")), Step0;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Natives.

template<class unificand> current_step TraceStep(const unificand& R,const unificand& P,const step& Step0) {
	if constexpr(!Runs<unificand>)
		Equate(P)->Equation->Trace=true, Print("Tracing: ",P);
	return UnifyStep("Trace",locus{},R,unificand(False),Step0);
}
template<class r,class... ps> auto ExposeCallable(r(*F)(ps...)) {
	return [=](const ps&... PS) {
		return F(PS...);
	};
}
struct runtime_macro {}; // A runtime incomparable head-normal value.
template<class unificand> auto MakeMacro(const invoke_step& Invoke) {
	if constexpr(Runs<unificand>)
		return future<>(runtime_macro{});
	else
		return box<macro_vertex>(Invoke);
}
template<class frame_type,class unificand=unificand_type<array<frame_type>>> frame_type MakeNatives() {
	auto NativeContext = MakeContext<unificand>(HereContext(locus{},"Natives"_VS),top_allows,
		true,GetAssumedFxConst(succeeds),DefaultGetSuspendedFx);
	return NativeContext->Run([&]{
		auto NativeRegs  = regs(2);
		auto NativeFrame = frame_type(FrameSigil,NativeRegs,succeeds);
		SetContextFrames(array{NativeFrame});
		auto AddNative   = [&](const string& Identifier,const unificand& U) {
			nat  i = NativeFrame->FrameUnificands.Add(U);
			auto r = reg(0,i,0,0,Identifier);
			NativeRegs.Coerce()+=array{r};
			if constexpr(!Runs<unificand>) {
				U.SetVertexName(r.RegName);
				NativeFrame->ScopeSymbols->SetBatch(Identifier,r);
			}
		};
		auto GetNative=[&](const char* S) {
			return unificand(NativeFrame->FrameUnificands.Elements[NativeIndex(NativeFrame,S)]);
		};

		// Types.
		AddNative("comparable"_VS     ,MakeCastFunction<unificand,comparable>(resolves&computes&converges&accepts&no_unifies)); // First because we look it up a lot.
		AddNative("false"_VS          ,Runs<unificand>? unificand(False): box<array_vertex>(0));
		AddNative("any"_VS            ,MakeStepFunction([](const unificand& R,const unificand& P,const step& Step0)->current_step {return UnifyStep("any",locus{},R,P,Step0);}));
		AddNative("void"_VS           ,MakeStepFunction([](const unificand& R,const unificand& P,const step& Step0)->current_step {return UnifyStep("void",locus{},R,unificand(False),Step0);}));
		AddNative("rational"_VS       ,MakeCastFunction<unificand,rational  >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable")}));
		AddNative("int"_VS            ,MakeCastFunction<unificand,integer   >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable"),GetNative("rational")}));
		AddNative("nat"_VS            ,MakeCastFunction<unificand,natural   >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable"),GetNative("rational"),GetNative("int")}));
		AddNative("char8"_VS          ,MakeCastFunction<unificand,char8     >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable")}));
		AddNative("char32"_VS         ,MakeCastFunction<unificand,char32    >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable")}));
		AddNative("char"_VS           ,GetNative("char8"));
		AddNative("float32"_VS        ,MakeCastFunction<unificand,float32   >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable")}));
		AddNative("float64"_VS        ,MakeCastFunction<unificand,float64   >(resolves&computes&converges&accepts&no_unifies,array{GetNative("comparable")}));
		AddNative("float"_VS          ,GetNative("float64"));
		AddNative("known"_VS,MakeStepFunction([](const unificand& R,const unificand& P,const step& Step0)->current_step {
			// Same as MakeCastFunction on comparable for now, but will bifurcate when we have constraints.
			return UnifyStep("known",locus{},R,P,[=]()->current_step {
				return WhenCastStep<comparable>(
					VERIFIER_HERE("WhenCastStep-known"),resolves&computes&converges&accepts&no_unifies,P,Truth(R),Step0,
					[=](const comparable& T,const step& Step1)->current_step {
						return Step1;
					},
					false,true
				);
			});
		}));
		if constexpr(Runs<unificand>) {
			AddNative("type_fx_abstraction"_VS,unificand());
			AddNative("abstraction_type"_VS,GetNative("any"));
		}
		else {
			AddNative("type_fx_abstraction"_VS,MakeStrictFunction<unificand>("in_type_fx_abstraction",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const box<abstraction_vertex>& A0,fx SpecifierFx)->current_step {
				auto A1=box<abstraction_vertex>(
					A0->AbstractionFunctionVertex, // Should take vertex, do WhenCastStep, put the actual parameter here.
					(KeepDefaultFx  (SpecifierFx)&A0->AbstractionKeepFx),
					(KeepSpecifierFx(SpecifierFx)&SpecifierFx          )+A0->AbstractionAddFx
				);
				return UnifyStep("in_type_fx_abstraction",locus{},R,unificand(A1),Step0);
			}));
			AddNative("abstraction_type"_VS,MakeStrictFunction<unificand>("abstraction_type",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const box<abstraction_vertex>& AV)->current_step {
				//Print("a_t ",AV->AbstractionFunctionVertex);
				return FindEquation(R)->Vertices.Has(AV->AbstractionFunctionVertex)? current_step(Step0): //!! Hack because for{x:(1,2)} gets confused on 5 abstraction_vertex, unsure why.
					UnifyStep("abstraction_type",locus{},R,AV->AbstractionFunctionVertex,Step0);
			}));
		}

		// Functions.
		AddNative("Length"_VS         ,MakeStrictFunction<unificand>("Length"     ,resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const array<unificand>& A          )->current_step {return UnifyStep("Length",locus{},R,unificand(Length(A)),Step0);}));
		AddNative("prefix'-'"_VS      ,MakeStrictFunction<unificand>("prefix'-'"  ,resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A                  )->current_step {return UnifyStep("prefix'-'"  ,locus{},R,unificand(-A ),Step0);}));
		AddNative("prefix'+'"_VS      ,MakeStrictFunction<unificand>("prefix'+'"  ,resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A                  )->current_step {return UnifyStep("prefix'+'"  ,locus{},R,unificand(+A ),Step0);}));
		AddNative("operator'+'"_VS    ,MakeStrictFunction<unificand>("operator'+'",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A,const rational& B)->current_step {return UnifyStep("operator'+'",locus{},R,unificand(A+B),Step0);}));
		AddNative("operator'-'"_VS    ,MakeStrictFunction<unificand>("operator'-'",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A,const rational& B)->current_step {return UnifyStep("operator'-'",locus{},R,unificand(A-B),Step0);}));
		AddNative("operator'*'"_VS    ,MakeStrictFunction<unificand>("operator'*'",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A,const rational& B)->current_step {return UnifyStep("operator'*'",locus{},R,unificand(A*B),Step0);}));
		AddNative("operator'/'"_VS    ,MakeStrictFunction<unificand>("operator'/'",resolves&computes&converges&accepts,[](const step& Step0,const unificand& R,const rational& A,const rational& B)->current_step {return B!=0? UnifyStep("operator'/'",locus{},R,unificand(A/B),Step0): FailTargetStep("operator'/'",locus{},R,Step0);}));
		AddNative("Throw"_VS          ,MakeStrictFunction<unificand>("Throw"      ,resolves&throws&converges&accepts,[](const step& Step0,const unificand& R,const unificand& A)->current_step {
			if constexpr(Runs<unificand>)
				return ThrowStep(locus{},A,Step0);
			else // Actually must wait until all prior execution has completed so we know exception is total - since it's not comparable if it contains functions.
				return UnifyStep("Throw",locus{},R,unificand(False),[=]()->current_step {
					return WhenFxStep(VERIFIER_HERE("ThrowStep"),only_imperatives&throws,contradicts&throws&converges&accepts&no_unifies,Step0,[=](const step& Step1)->current_step {
						VERSE_ERR("Unimplemented: verifier throw"); //!! Verifier needs support for abstract interpretation of throw.
					});
				});
		}));
		AddNative("Err"_VS,MakeStrictFunction<unificand>("Err",resolves&computes&accepts,[](const step& Step0,const unificand& R,const string& S)->current_step {
			if constexpr(Runs<unificand>)
				return ErrStep(R01(locus{},S));
			else // Need WhenCastStep to provide frame with ImplyFx so we can imply under it with appropriate weakening.
				return DivergeTargetStep("Err",locus{},R,Step0);
		}));
		AddNative("Concatenate"_VS,MakeStrictFunction<unificand>("Concatenate",resolves&computes&converges&accepts&no_unifies,[](const step& Step0,const unificand& R,const array<array<unificand>>& XSS)->current_step {
			auto XS = XSS.Concatenate();
			auto YS = MakeArrayUnificand<unificand>(Length(XS));
			return UnifyStep("Concatenate-Result",locus{},R,YS,[=]()->current_step {
				return ForStep(Length(XS),Step0,[=](nat i,const step& Step1)->current_step {
					return UnifyStep("Concatenate-Elements",locus{},GetArrayElement(YS,i),XS[i],Step1);
				});
			});
		}));
		AddNative("Print"_VS,MakeStepFunction([](const unificand& R,const unificand& P,const step& Step0)->current_step {
			return UnifyStep("Print",locus{},R,unificand(False),[=]()->current_step {
				// Eventually need maximally asynchronous implementation comporting with ToString,
				// properly sequenced verify-time version, etc.
				return WhenCastStep<future<>>(
					VERIFIER_HERE("Print-parameter"),succeeds&converges&no_transacts&interacts&no_unifies&accepts,P,Truth(R),Step0,
					[=](const future<>& B,const step& Step1)->current_step {
						// Breaks because runtime ParentPendingFx are bogus in: f()<interacts>:=Print(456); f[]
						return WhenFxStep(VERIFIER_HERE("Print-fx"),only_imperatives&interacts,succeeds&converges&no_transacts&interacts&no_unifies&accepts,Step1,[=](const step& Step2)->current_step {
							Print("Print: ",P);
							return Step2;
						});
					},
					false,true
				);
			});
		}));
		AddNative("operator'..'"_VS,MakeMacro<unificand>(OperatorRangeMacro));

		// Native functions for testing the verifier.
		AddNative("Trace"_VS,MakeStepFunction(ExposeCallable(TraceStep<unificand>)));

		// Effects. See KeepDefaultFx,KeepSpecifierFx for interactions.
		AddNative("converges"_VS      ,unificand(converges));
		AddNative("diverges"_VS       ,unificand(contradicts));
		AddNative("contradicts"_VS    ,unificand(contradicts));
		AddNative("fails"_VS          ,unificand(fails));
		AddNative("succeeds"_VS       ,unificand(succeeds));
		AddNative("decides"_VS        ,unificand(decides));
		AddNative("resolves"_VS       ,unificand(resolves));
		AddNative("abstracts"_VS      ,unificand(abstracts));
		AddNative("iterates"_VS       ,unificand(iterates));
		AddNative("computes"_VS       ,unificand(computes));
		AddNative("allocates"_VS      ,unificand(allocates));
		AddNative("reads"_VS          ,unificand(reads));
		AddNative("writes"_VS         ,unificand(writes));
		AddNative("transacts"_VS      ,unificand(transacts));
		AddNative("suspends"_VS       ,unificand(suspends));
		AddNative("interacts"_VS      ,unificand(interacts));
		AddNative("throws_any"_VS     ,unificand(throws)); // Temporary for testing, will become "throws" taking parameter.

		// Function specifiers.
		AddNative("closed"_VS         ,unificand(function_specifiers::Closed));
		AddNative("open"_VS           ,unificand(function_specifiers::Open));

		// Macros.
		AddNative("array"_VS          ,MakeMacro<unificand>(ArrayMacro));
		AddNative("let"_VS            ,MakeMacro<unificand>(LetMacro));
		AddNative("operator'where'"_VS,MakeMacro<unificand>(WhereMacro));
		AddNative("if"_VS             ,MakeMacro<unificand>(IfMacro));
		AddNative("first"_VS          ,MakeMacro<unificand>(FirstMacro));
		AddNative("for"_VS            ,MakeMacro<unificand>(ForMacro));
		AddNative("forall"_VS         ,MakeMacro<unificand>(ForAllMacro));
		AddNative("case"_VS           ,MakeMacro<unificand>(FunctionMacro<function_defaults>));
		AddNative("function"_VS       ,MakeMacro<unificand>(FunctionMacro<function_defaults>));
		AddNative("prefix'not'"_VS    ,MakeMacro<unificand>(PrefixNotMacro));
		AddNative("type"_VS           ,MakeMacro<unificand>(TypeMacro));
		//AddNative("abstraction"_VS  ,MakeMacro<unificand>(AbstractionMacro)); // Like type, but captures and propagates abstraction effects.
		AddNative("assert"_VS         ,MakeMacro<unificand>(AssertMacro));
		AddNative("prefix':'"_VS      ,MakeMacro<unificand>(PrefixInMacro));
		AddNative("operator'or'"_VS   ,MakeMacro<unificand>(OperatorOrMacro));
		AddNative("operator'and'"_VS  ,MakeMacro<unificand>(OperatorAndMacro));
		AddNative("operator':='"_VS   ,MakeMacro<unificand>(OperatorDefineMacro));
		AddNative("operator'|'"_VS    ,MakeMacro<unificand>(OperatorChoiceMacro));
		AddNative("operator'<>'"_VS   ,MakeMacro<unificand>(OperatorNotEqualMacro));
		AddNative("operator'.'"_VS    ,MakeMacro<unificand>(OperatorDotMacro));
		AddNative("operator'=>'"_VS   ,MakeMacro<unificand>(FunctionMacro<function_defaults>));//!!
		AddNative("operator'='"_VS    ,MakeMacro<unificand>(UnifyMacro));
		AddNative("operator'?'"_VS    ,MakeMacro<unificand>(DeoptionMacro));

		// Macros for testing the verifier. Not safe in user code.
		AddNative("test"_VS           ,MakeMacro<unificand>(TestMacro));
		AddNative("assume"_VS         ,MakeMacro<unificand>(AssumeMacro));
		AddNative("check"_VS          ,MakeMacro<unificand>(CheckMacro));
		AddNative("expect"_VS         ,MakeMacro<unificand>(ExpectMacro));
		AddNative("reject"_VS         ,MakeMacro<unificand>(RejectMacro));

		return NativeFrame;
	});
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Verifier errors.

error verifier_context_state::OnDescribeSuspensionHelper(const suspension& Sus,bool& Recurse) const {
	auto Fx      = Sus->SuspendedFx;
	auto AllowFx = Sus.Context()->AllowFx;
	auto Error   = Sus->OnDescribe();
	if(auto SusContext=Sus.Cast<context>(); SusContext && !IsVerify(SusContext.Coerce()) && !ContextReportBetas)
		Recurse=false;
	if(!IsVerify() || Fx<=accepts || Error.ErrorCode=="R00") {
		// Refine error based on dynamically recalculated effects.
		if(Fx<=fails && !(Fx<=contradicts) && !(fails<=contradicts+AllowFx))
			Error=F00(Error.Locus,Error.Internal);
		else if(!((contradicts&Fx)<=AllowFx))
			Error=X00(Error.Locus,Error.Internal,Fx,AllowFx);
		else if(!(Fx<=resolves+iterates) && !(abstracts<=contradicts+AllowFx))
			Error=A00(Error.Locus,Error.Internal);
		else if(Fx<=iterates && !(Fx<=decides) && !(iterates<=contradicts+AllowFx))
			Error=I00(Error.Locus,Error.Internal);
		else if(Fx<=resolves && !(Fx<=decides) && !(resolves<=contradicts+AllowFx))
			Error=U00(Error.Locus,Error.Internal);
		else if(Fx<=decides && !(Fx<=succeeds) && !(Fx<=fails) && AllowFx<=decides)
			Error=D00(Error.Locus,Error.Internal);
		else if(Fx<=succeeds && !(Fx<=contradicts) && AllowFx<=fails) // Only makes sense at allow-boundary, not granularly.
			Error=S01(Error.Locus,Error.Internal);
		else
			Error=S00(Error.Locus,Error.Internal);
		if(Sus.Cast<verifier_iterate>())
			Error.Priority+=10;
	}
	return Error;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Runtime and verifier.

// Rules (ProgramIntro) and (ProgramElim).
template<class unificand> result<tuple<>,error> Eval(const locus& Locus,const string& What,const function<current_step(const step&)>& Body,const string& ExpectFirstError) {
	using base  = if_type<IsEqual<unificand,future<>>,iterate_managed,verifier_iterate_managed>;
	auto  State = [&]{
		if constexpr(IsEqual<unificand,future<>>)
			return False;
		else
			return verifier_iterate_state{GetAssumedFxConst(effects),visibility::VerifyTop,DefaultGetSuspendedFx};
	}();
	result<tuple<>,error> Result   = False;
	option<step>          CheckStep;
	CheckStep=Truth(step([&]()->current_step {
		auto VI = Thread.Cast<verifier_iterate>();
		if(VI)
			if(VI->RefineFx(); Thread->IsReady)
				return ResumeStep(CheckStep.Coerce());
		if(!(Thread->LocalPendingFx<=Thread->AllowFx) && !(VI&&VI->SilenceCount))
			return ErrStep(ContextError(Locus));
		return LeaveIterateStep;
	}));
	Iterate<base>(Here(Locus,What),State,top_allows,effects,
		[=] {
			return Body(CheckStep.Coerce());
		},
		[&] {},
		[&] {VERSE_UNEXPECTED;},
		[&](const future<>&) {VERSE_UNEXPECTED;},
		[&](const iterate& I,const error& E) {
			if(E.ErrorCode!=ExpectFirstError)
				I->Run([&]{return ContextReport(VerboseContextReport);});
			Result=E;
		}
	);
	return Result;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Testing.

void TestScript() {
	//TestParsing();
	nat c0=Clock();
	Print(), Print("--- testing compiler ---");

	// Parse.
	Print("Parsing...");
	auto Filename = "Tests.verse"_VS;
	auto Locus    = locus{Filename};
	auto Syntax   = CoerceResult(ParseVerseSyntax(LoadTextFile(Filename),Locus));

	// Verify.
	Print("Verifying...");
	VERSE_ENSURE(!Thread->HasSuspensions());
	future<box<op>> OpFuture;
	CoerceResult(Eval<vertex>(Locus,"Verify"_VS,[=](const step& Step0)->current_step {
		auto NativeScope = MakeNatives<verify_scope>();
		return NativeScope.Context()->RunStep(Step0,[=](const step& Step1)->current_step {
			auto RootOutputReg = reg(0,0ULL);
			auto RootInputReg  = reg(0,1ULL);
			auto RootRedex     = redex{Locus,array{NativeScope},box<op::sequence>(),RootOutputReg,RootInputReg};
			IterateStart()->Run([&]{SetContextFrames(array{NativeScope});}); // For GetVerifierNative.
			return EvalStep(Locus,RootRedex.Scopes,
				// This op::reduce allocates op::check regs, and its op::check creates a solve context.
				box<op::reduce>(RootRedex,0,CaptureString("Program"),[=](const redex& VerifyRedex,const step& Step2)->current_step {
					VerifyRedex.Scopes.End(0)->ScopeSymbols->EliminateBatch();
					return VerifyRedex.ReduceStep(Step2,
						box<op::check>(top_allows,False,RootOutputReg,
							// This op::reduce resolves Op to the new TestRedex.
							box<op::reduce>(RootRedex,1,CaptureString("Program"),[=](const redex& ReduceRedex1,const step& Step3)->current_step {
								//Print("Op:\n",ReduceRedex1.Op.Coerce(),"\n");
								OpFuture.ResolveBatch(ReduceRedex1.Op,false);
								auto TestRedex = ReduceRedex1.FreshRedex("TestRedex"_VS,Syntax);
								return ReduceRedex1.ReduceStep(Step3,box<op::sequence>(
									box<op::exists>(TestRedex),
									TestRedex.Op
								));
							})
						)
					);
				}),
				Step1);
		});
	}));
	VERSE_ENSURE(!Thread->HasSuspensions());
	auto Op=OpFuture.Coerce();

	// Run.
	Print("Running...");
	auto NativeFrame = MakeNatives<run_frame>();
	CoerceResult(NativeFrame.Context()->Run([&]{
		return Eval<future<>>(Locus,"Run"_VS,[=](const step& Step0) {
			auto ProgramFrames = array{NativeFrame,run_frame{FrameSigil,regs(0)}};
			return EvalStep(Locus,ProgramFrames,Op,Step0);
		});
	}));

	Print("Time Elapsed ",(Clock()-c0)/1000/1000/1000,"Gcyc");
}
string Verse::ExposeToString(const box<syntax>& A) {
	if(auto Result=ToStringSyntax(encoding(),A))
		return Result;
	return "()"_VS;
}
string Verse::ToCode(const box<syntax>& A) {
	return "code{"_VS + ToStringSyntax(encoding(),A) + "}"_VS;
}
