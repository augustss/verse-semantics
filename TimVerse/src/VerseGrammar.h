// Copyright Epic Games, Inc. All Rights Reserved.
// Dependency-free allocation-free single-header Verse grammar library.
//--------------------------------------------------------------------------------------------------------------------------------------------------------------

#pragma once

#if !TIM
#include <utility>
#endif

namespace Verse {
namespace Grammar {

#ifndef VERSE_MAX_EXPR_DEPTH
#define VERSE_MAX_EXPR_DEPTH 100
#endif

#ifndef VERSE_MAX_INDCMT_DEPTH
#define VERSE_MAX_INDCMT_DEPTH 3
#endif

// Macros.
#if /*NDEBUG*/false
#define GRAMMAR_ASSERT(c)    (void)(0)
#else
#define GRAMMAR_ASSERT(c)    ((c)? (void)0: Verse::Grammar::Err())
#endif
#define GRAMMAR_RUN(e)       {auto GrammarTemp=(e); if(!GrammarTemp) return GrammarTemp.GetError();}
#define GRAMMAR_SET(r,e)     {auto GrammarTemp=(e); if(!GrammarTemp) return GrammarTemp.GetError(); r=*GrammarTemp;}
#define GRAMMAR_LET(r,e)     auto r##Let=(e); if(!r##Let) return r##Let.GetError(); auto r=*r##Let;

// Natural numbers and characters.
using int64  = long long;
using nat8   = unsigned char;
using nat16  = unsigned short;
using nat32  = unsigned int;
using nat64  = unsigned long long;
using nat    = unsigned long long;
using char32 = char32_t;

#if defined(__cpp_char8_t)
	using char8 = char8_t;
#else
	// `char8_t` is natively defined since C++20 unless disabled.
	// If not available, use `char` as a replacement - while `char8_t` is supposed to be unsigned, `unsigned char` is not compatible with u8"" literals.
	// Though all greater / less than comparisons need to ensure that an unsigned 8-bit number is used.
	// [The alternative is to have all string literals wrapped in a cast (or macro of a cast) and use `unsigned char`.]
	// See:
	//   char8_t: A type for UTF-8 characters and strings (Revision 6) - https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0482r6.html
	//   char8_t backward compatibility remediation - https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1423r3.html
	using char8 = char;
#endif

// Basic functions.
template<class t,nat n> constexpr nat ArraySize(t(&)[n]) {return n;}

// Error.
[[noreturn]] inline void Err() //-V1082
{ 
#if defined(_MSC_VER)
	while(true) __debugbreak();
#else
	while(true) __builtin_trap();
#endif
}

// Trivial type.
struct nothing {};

// Precedence.
enum class prec: nat8 {Never,List,Commas,Expr,Fun,Def,Or,And,Not,Eq,NotEq,Less,Greater,Choice,To,Add,Mul,Prefix,Call,Base,Nothing};

// Associativity.
enum class assoc: nat8 {None,Postfix,InfixLeft,InfixRight};

// Block form.
enum class form: nat8 {List, Commas};

// Block punctuation.
enum class punctuation: nat8 {None,Braces,Parens,Brackets,AngleBrackets,Qualifier,Dot,Colon,Ind};

// Places specializing capture_t generation.
enum class place: nat16 {UTF8,Printable,BlockCmt,LineCmt,IndCmt,Space,String,Content};

// Modes for calling: none (error if instantiated), of (failure disallowed), at (failure allowed), with (macro).
enum class mode {None,Open,Closed,With};

// Sets a variable on construction, and restores its previous value on destruction.
template<typename t> struct scoped_guard {
    scoped_guard(t& _guard_variable, const t& new_value)
        : guard_variable(&_guard_variable)
        , old_value(_guard_variable)
    {
        *guard_variable = new_value;
    }
    ~scoped_guard()
    {
        if (guard_variable)
            *guard_variable = old_value;
    }
private:
    t* guard_variable;
    t old_value;
};

// Text spans passed around by the parser.
struct text {
	const char8 *Start, *Stop;
	constexpr text(): Start(nullptr), Stop(nullptr) {}
	constexpr text(const char8* Start0,const char8* Stop0): Start(Start0), Stop(Stop0) {GRAMMAR_ASSERT(Stop>=Start);}
	constexpr text(const char8* Start0): text(Start0,Start0) {while(*Stop) ++Stop;}
	#if defined(__cpp_char8_t)
	text(const char* Start0) : text(reinterpret_cast<const char8*>(Start0), (const char8*)Start0) { while (*Stop) ++Stop; }
	#endif
	constexpr char8 operator[](nat i) const {GRAMMAR_ASSERT(Start+i<Stop); return Start[i];}
	explicit operator bool() const {return Start!=Stop;}
};
constexpr nat Length(const text& Text) {
	return Text.Stop-Text.Start;
}
inline bool operator==(const text& as,const text& bs) {
	if(Length(as)!=Length(bs))
		return 0;
	for(nat i=0; i<Length(as); i++)
		if(as.Start[i]!=bs.Start[i])
			return 0;
	return 1;
}
inline bool operator!=(const text& as,const text& bs) {
	return !(as==bs);
}

// A snippet of text describing its location.
struct snippet {
	text Text;
	nat  StartLine,   StopLine;
	nat  StartColumn, StopColumn;
	snippet(): Text(nullptr,nullptr), StartLine(0), StopLine(0), StartColumn(0), StopColumn(0) {}
	explicit operator bool() const {
		return bool(Text);
	}
private:
	// Private to ensure all non-empty snippets are within the string passed to the parser.
	friend struct parser_base;
	snippet(const char8* Start0,const char8* End0,nat StartLine0,nat StopLine0,nat StartColumn0,nat StopColumn0):
		Text(Start0,End0), StartLine(StartLine0), StopLine(StopLine0), StartColumn(StartColumn0), StopColumn(StopColumn0) {}
};

// Verse blocks.
template<class syntaxes_t,class capture_t> struct block {
	block(const snippet& BlockSnippet0=snippet{},const syntaxes_t& Elements0=syntaxes_t(),form Form0=form::List):
		BlockSnippet(BlockSnippet0), Punctuation(punctuation::None), Form(Form0), Elements(Elements0) {}
	snippet     BlockSnippet;        // Snippet of the whole block.
	syntaxes_t  Specifiers;          // Specifiers.
	capture_t   TokenLeading;        // If Token, the Scan before it.
	text        Token;               // Token preceding opening punctuation.
	capture_t   PunctuationLeading;  // After token, before opening punctuation; present only if Punctuation.
	punctuation Punctuation;         // Punctuation wrapping the list.
	capture_t   PunctuationTrailing; // If Punctuation, this holds Space & NewLine trailing it.
	form        Form;                // Commas or List.
	syntaxes_t  Elements;            // Elements.
	capture_t   ElementsTrailing;    // Scan between elements and closing punctuation or end.
};

// Results consisting of either a value or an error.
template<class value_t,class error_t> struct result {
	template<class u,class=decltype(value_t(*(u*)nullptr))> result(const u& Value0): Value(Value0), Success(true) {}
#if !TIM
    template<class u, class = decltype(value_t(*(u*)nullptr))> result(u&& Value0) : Value(std::move(Value0)), Success(true) {}
#endif
	template<class t0=error_t,class=decltype(t0())> result(): Error(), Success(false) {}
	result(const error_t& Error0): Error(Error0), Success(false) {}
	result(const result& Other): Success(Other.Success) {
		if(Other.Success)
			new(&Value)value_t(Other.Value);
		else
			new(&Error)error_t(Other.Error);
	}
	~result() {if(Success) {Value.~value_t();} else {Error.~error_t();}}
	operator bool() const {return Success;}
	result& operator=(const result& R) {if(this!=&R) {this->~result(); new(this)result(R);} return *this;}
	const value_t& operator*() const {GRAMMAR_ASSERT(Success); return Value;}
	value_t* operator->() {GRAMMAR_ASSERT(Success); return &Value;}
	const error_t& GetError() const {GRAMMAR_ASSERT(!Success); return Error;}
private:
	union {value_t Value; error_t Error;};
	bool  Success;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Low-level character classification.

// Verse grammar character classification functions.
constexpr bool IsSpace   (char8 c) {return c==' ' || c=='\t';}
constexpr bool IsNewLine (char8 c) {return c==0x0D || c==0x0A;}
constexpr bool IsEnding  (char8 c) {return c==0 || c==0x0D || c==0x0A;}
constexpr bool IsAlpha   (char8 c) {return (c>='A'&&c<='Z') || (c>='a'&&c<='z') || c=='_';}  // Parentheses `()` required to make static analysis happy
constexpr bool IsDigit   (char8 c) {return c>='0' && c<='9';}
constexpr bool IsAlnum   (char8 c) {return IsAlpha(c) || IsDigit(c);}
constexpr bool IsHex     (char8 c) {return (c>='0'&&c<='9') || (c>='A'&&c<='F') || (c>='a'&&c<='f');}
constexpr nat8 DigitValue(char8 c) {return (c>='0'&&c<='9')? c-'0': (c>='A'&&c<='F')? c-'A'+10: (c>='a'&&c<='f')? c-'a'+10: 0;}
constexpr bool IsIdentifierQuotable(char8 c0,char8 c1) {return nat8(c0)>=0x20 && nat8(c0)<=0x7E && c0!='{' && c0!='}' && c0!='"' && c0!='\'' && c0!='\\' && (c0!='<'||c1!='#') && (c0!='#'||c1!='>');}
constexpr bool IsStringBackslashLiteral(char8 c0,char8 c1) {return c0=='r' || c0=='n' || c0=='t' || c0=='\\' || c0=='"' || c0=='\'' || (c0=='<'&&c1!='#') || c0=='>' || (c0=='#'&&c1!='>') || c0=='&' || c0=='~' || c0=='{' || c0=='}';}

// Convert valid UTF-8 sequence with valid length to its Unicode Code Point.
inline char32 EncodedChar32(const char8* s,nat Count) {
	switch(Count) {  // Extra `nat8` casts for when `char8` is signed in certain circumstances
	case 1: return char32(       nat8(s[0])                                                                                                            );
	case 2: return char32((nat32(nat8(s[0]))*0x40    + nat32(nat8(s[1])&0x3F)                                                              ) & 0x7FF   );
	case 3: return char32((nat32(nat8(s[0]))*0x1000  + nat32(nat8(s[1])&0x3F)*0x40   + nat32(nat8(s[2])&0x3F)                              ) & 0xFFFF  );
	case 4: return char32((nat32(nat8(s[0]))*0x40000 + nat32(nat8(s[1])&0x3F)*0x1000 + nat32(nat8(s[2])&0x3F)*0x40 + nat32(nat8(s[3])&0x3F)) & 0x1FFFFF);
	default: Err();
	}
}

// Get length of internal lexical unit recognized for Place.
// U8        := 0o80..0oBF
// UTF8      :=                                      0o00..0o7F
//           |                                       0oC2..0oDF U8
//           |  !(0oE0 0o80..0o9F | 0oED 0oA0..0oBF) 0oE0..0oEF U8 U8
//           |  !(0oF0 0o80..0o8F | 0oF4 0o90..0oBF) 0oF0..0oF4 U8 U8 U8
// Printable := 0o09 | !("<#" | "#>" | 0o0..0o1F | 0o7F | 0oC2 0o80..0o9F | 0oE2 0o80 0oA8..0oA9 ) UTF8 | ..
// Special   := '\'|'{'|'}'|'#'|'<'|'>'|'&'|'~'
// String    := .. !('\'|'{'|'}'|'"') Text ..
// Content   := .. !Special Text ..
template<place Place> nat EncodedLength(const char8* s) {
	switch(nat8(s[0])) {
	case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
	case 0x08:            case 0x0A: case 0x0B: case 0x0C: case 0x0D: case 0x0E: case 0x0F: 
	case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15: case 0x16: case 0x17: 
	case 0x18: case 0x19: case 0x1A: case 0x1B: case 0x1C: case 0x1D: case 0x1E: case 0x1F:
	case 0x7F:
		return Place==place::UTF8? 1: 0;
	case 0x09: case 0x20:
		return 1;
	case '"':
		return Place!=place::Space && Place!=place::String? 1: 0;
	case '<':
		return Place==place::UTF8 || (s[1]!='#' && Place!=place::Space && Place!=place::Content)? 1: 0;
	case '#':
		return Place==place::UTF8 || (s[1]!='>' && Place!=place::Space)? 1: 0;
	case '\\': case '{': case '}':
		return Place!=place::Space && Place!=place::String && Place!=place::Content? 1: 0;
	case '>': case '&': case '~':
		return Place!=place::Space && Place!=place::Content? 1: 0;
	case '!': case '$': case '%': case '\'':case '(': case ')': case '*': case '+': case ',': case '-': case '.': case '/':
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
	case ':': case ';': case '=': case '?': case '@':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
	case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '[': case ']': case '^': case '_': case '`':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
	case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case '|':
		return Place!=place::Space? 1: 0;
	case 0x80: case 0x81: case 0x82: case 0x83: case 0x84: case 0x85: case 0x86: case 0x87:
	case 0x88: case 0x89: case 0x8A: case 0x8B: case 0x8C: case 0x8D: case 0x8E: case 0x8F:
	case 0x90: case 0x91: case 0x92: case 0x93: case 0x94: case 0x95: case 0x96: case 0x97:
	case 0x98: case 0x99: case 0x9A: case 0x9B: case 0x9C: case 0x9D: case 0x9E: case 0x9F:
	case 0xA0: case 0xA1: case 0xA2: case 0xA3: case 0xA4: case 0xA5: case 0xA6: case 0xA7:
	case 0xA8: case 0xA9: case 0xAA: case 0xAB: case 0xAC: case 0xAD: case 0xAE: case 0xAF:
	case 0xB0: case 0xB1: case 0xB2: case 0xB3: case 0xB4: case 0xB5: case 0xB6: case 0xB7:
	case 0xB8: case 0xB9: case 0xBA: case 0xBB: case 0xBC: case 0xBD: case 0xBE: case 0xBF:
	case 0xC0: case 0xC1:
	case 0xF5: case 0xF6: case 0xF7:
	case 0xF8: case 0xF9: case 0xFA: case 0xFB: case 0xFC: case 0xFD: case 0xFE: case 0xFF:
		return 0;
	case 0xC2:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0xBF && (Place==place::UTF8 || nat8(s[1])>=0xA0)? 2: 0;
	case 0xC3: case 0xC4: case 0xC5: case 0xC6: case 0xC7:
	case 0xC8: case 0xC9: case 0xCA: case 0xCB: case 0xCC: case 0xCD: case 0xCE: case 0xCF:
	case 0xD0: case 0xD1: case 0xD2: case 0xD3: case 0xD4: case 0xD5: case 0xD6: case 0xD7: 
	case 0xD8: case 0xD9: case 0xDA: case 0xDB: case 0xDC: case 0xDD: case 0xDE: case 0xDF:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0xBF? 2: 0;
	case 0xE0:
		return Place!=place::Space && nat8(s[1])>=0xA0&&nat8(s[1])<=0xBF && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF? 3: 0;
	case 0xE2:
		if constexpr(Place!=place::Space)
			if ((nat8(s[1])>=0x80)
				&& (nat8(s[1])<=0xBF)
				&& (nat8(s[2])>=0x80)
				&& (nat8(s[2])<=0xBF)
				&& ((nat8(s[1])!=0x80)
					|| ((nat8(s[2])!=0xA8)
						&& (nat8(s[2])!=0xA9))))
				return 3;
		return 0;
	case 0xE1: case 0xE3: case 0xE4: case 0xE5: case 0xE6: case 0xE7:
	case 0xE8: case 0xE9: case 0xEA: case 0xEB: case 0xEC:            case 0xEE: case 0xEF:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0xBF && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF? 3: 0;
	case 0xED:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0x9F && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF? 3: 0;
	case 0xF0:
		return Place!=place::Space && nat8(s[1])>=0x90&&nat8(s[1])<=0xBF && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF && nat8(s[3])>=0x80&&nat8(s[3])<=0xBF? 4: 0;
	case 0xF1: case 0xF2: case 0xF3:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0xBF && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF && nat8(s[3])>=0x80&&nat8(s[3])<=0xBF? 4: 0;
	case 0xF4:
		return Place!=place::Space && nat8(s[1])>=0x80&&nat8(s[1])<=0x8F && nat8(s[2])>=0x80&&nat8(s[2])<=0xBF && nat8(s[3])>=0x80&&nat8(s[3])<=0xBF? 4: 0;
	default:
		return 0;
	};
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Grammar output.
// This is not ready for use, but will later provide a Verse and VSON output library and pretty-printing API.

// Grammar output encoding.
struct encoding {
	prec Prec;
	bool AllowIn,FollowingIn;
	encoding(prec Prec0=prec::List,bool AllowIn0=false,bool FollowingIn0=false):
		Prec(Prec0), AllowIn(AllowIn0), FollowingIn(FollowingIn0) {}
	encoding Fresh(prec Prec1,bool AllowIn1=false,bool FollowingIn0=false) const {
		return encoding(Prec1,AllowIn1 || Prec1<=prec::Def,FollowingIn0);
	}
};
inline bool ParenthesizePrefix(const encoding& Encoding,prec StringPrec) {
	return StringPrec<Encoding.Prec;
}
inline bool ParenthesizePostfix(const encoding& Encoding,prec StringPrec) {
	return StringPrec<Encoding.Prec || ((Encoding.Prec==prec::Less)&&(StringPrec==prec::Greater));
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Token table.

// Forward declared tokens.
extern const struct token_set AllTokens,AllowLess,AllowNotEq;

// Token information.
struct token_info {
	const char8*     Symbol;
	prec             PrefixPrec;
	mode             PrefixMode;
	prec             PostfixTokenPrec;
	prec             PostfixPrec;
	assoc            PostfixAssoc;
	mode             PostfixMode;
	const token_set& PostfixAllowMask;
	encoding PostfixLeftEncoding(const encoding& Encoding,bool Parens) const {
		GRAMMAR_ASSERT(PostfixAssoc==assoc::Postfix || PostfixAssoc==assoc::InfixLeft || PostfixAssoc==assoc::InfixRight);
		bool AllowIn = Encoding.AllowIn || Encoding.Prec<=prec::Def || Parens;
		if(PostfixAssoc==assoc::Postfix || PostfixAssoc==assoc::InfixLeft)
			return Encoding.Fresh(PostfixPrec,AllowIn);
		else 
			return Encoding.Fresh(prec(nat(PostfixPrec)+1),AllowIn);
	}
	encoding PostfixRightEncoding(const encoding& Encoding,bool Parens) const {
		GRAMMAR_ASSERT(PostfixAssoc==assoc::InfixLeft || PostfixAssoc==assoc::InfixRight);
		if(PostfixAssoc==assoc::InfixRight)
			return Encoding.Fresh(PostfixPrec,false,Encoding.FollowingIn && !Parens);
		else
			return Encoding.Fresh(prec(nat(PostfixPrec)+1),false,Encoding.FollowingIn && !Parens);
	}
	prec PostfixRightPrec() const {
		GRAMMAR_ASSERT(PostfixAssoc==assoc::InfixLeft || PostfixAssoc==assoc::InfixRight);
		return PostfixAssoc==assoc::InfixRight? PostfixPrec: prec(nat(PostfixPrec)+1);
	}
};
constexpr token_info Tokens[]={
	//Symbol     , PrefixPrec  , PrefixMode   ,PostfixTokenPrec,PostfixPrec  ,PostfixAssoc     ,PostfixMode , PostfixAllowMask
	{u8""        , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens }, // unknown
	{u8""        , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens }, // end
	{u8""        , prec::Never , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::None  , AllTokens }, // NewLine
	{u8""        , prec::Base  , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens }, // Alpha
	{u8""        , prec::Base  , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens }, // Digit
	{u8"alias"   , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"and"     , prec::Base  , mode::None   ,prec::And       ,prec::And    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"at"      , prec::Base  , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::Closed, AllTokens },
	{u8"break"   , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"catch"   , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"const"   , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"continue", prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"do"      , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"else"    , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"if"      , prec::Base  , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"in"      , prec::Def   , mode::With   ,prec::Def       ,prec::Choice ,assoc::None      ,mode::None  , AllTokens },
	{u8"is"      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::None      ,mode::None  , AllTokens },
	{u8"live"    , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"mutable" , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"next"    , prec::Base  , mode::None   ,prec::Fun       ,prec::Fun    ,assoc::InfixRight,mode::None  , AllTokens },
	{u8"not"     , prec::Not   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"of"      , prec::Base  , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::Open  , AllTokens },
	{u8"or"      , prec::Base  , mode::None   ,prec::Or        ,prec::Or     ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"over"    , prec::Base  , mode::None   ,prec::Fun       ,prec::Fun    ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8"set"     , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"ref"     , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::With  , AllTokens },
	{u8"return"  , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"then"    , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"to"      , prec::Base  , mode::None   ,prec::To        ,prec::To     ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"until"   , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"when"    , prec::Base  , mode::None   ,prec::Fun       ,prec::Fun    ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8"where"   , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"while"   , prec::Base  , mode::None   ,prec::Fun       ,prec::Fun    ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8"with"    , prec::Never , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::None  , AllTokens },
	{u8"yield"   , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"var"     , prec::Def   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8","       , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8";"       , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"!"       , prec::Not   , mode::With   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"\""      , prec::Base  , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"&"       , prec::Def   , mode::None   ,prec::Mul       ,prec::Mul    ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8"'"       , prec::Base  , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"("       , prec::Base  , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::None  , AllTokens },
	{u8")"       , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"*"       , prec::Prefix, mode::Closed ,prec::Mul       ,prec::Mul    ,assoc::InfixLeft ,mode::Closed, AllTokens },
	{u8"*="      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"+"       , prec::Prefix, mode::Closed ,prec::Add       ,prec::Add    ,assoc::InfixLeft ,mode::Closed, AllTokens },
	{u8"+="      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"-"       , prec::Prefix, mode::Closed ,prec::Add       ,prec::Add    ,assoc::InfixLeft ,mode::Closed, AllTokens },
	{u8"-="      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"->"      , prec::Never , mode::None   ,prec::To        ,prec::To     ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"."       , prec::Never , mode::None   ,prec::Call      ,prec::Call   ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8".."      , prec::Def   , mode::With   ,prec::To        ,prec::To     ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"/"       , prec::Base  , mode::None   ,prec::Mul       ,prec::Mul    ,assoc::InfixLeft ,mode::Closed, AllTokens },
	{u8"/="      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8":"       , prec::Def   , mode::With   ,prec::Call      ,prec::Choice ,assoc::None      ,mode::None  , AllTokens },
	{u8":="      , prec::Never , mode::None   ,prec::Def       ,prec::Def    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8":)"      , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8":>"      , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"<"       , prec::Base  , mode::None   ,prec::Call      ,prec::Less   ,assoc::InfixRight,mode::Closed, AllowLess },
	{u8"<="      , prec::Never , mode::None   ,prec::Less      ,prec::Less   ,assoc::InfixRight,mode::Closed, AllowLess },
	{u8"<>"      , prec::Never , mode::None   ,prec::NotEq     ,prec::NotEq  ,assoc::InfixLeft ,mode::With  , AllowNotEq},
	{u8"="       , prec::Never , mode::None   ,prec::Eq        ,prec::Eq     ,assoc::InfixLeft ,mode::With  , AllTokens },
	{u8"=="      , prec::Never , mode::None   ,prec::Eq        ,prec::Never  ,assoc::None      ,mode::Closed, AllTokens },
	{u8"=>"      , prec::Never , mode::None   ,prec::Fun       ,prec::Fun    ,assoc::InfixRight,mode::With  , AllTokens },
	{u8">"       , prec::Never , mode::None   ,prec::Greater   ,prec::Greater,assoc::InfixRight,mode::Closed, AllTokens },
	{u8">="      , prec::Never , mode::None   ,prec::Greater   ,prec::Greater,assoc::InfixRight,mode::Closed, AllTokens },
	{u8"?"       , prec::Prefix, mode::Closed ,prec::Call      ,prec::Call   ,assoc::Postfix   ,mode::With  , AllTokens },
	{u8"@"       , prec::Expr  , mode::None   ,prec::Expr      ,prec::Expr   ,assoc::None      ,mode::None  , AllTokens },
	{u8"["       , prec::Prefix, mode::Closed ,prec::Call      ,prec::Prefix ,assoc::InfixRight,mode::Closed, AllTokens },
	{u8"]"       , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
	{u8"^"       , prec::Prefix, mode::Closed ,prec::Call      ,prec::Call   ,assoc::Postfix   ,mode::With  , AllTokens },
	{u8"{"       , prec::Never , mode::None   ,prec::Call      ,prec::Call   ,assoc::None      ,mode::None  , AllTokens },
	{u8"|"       , prec::Never , mode::None   ,prec::Choice    ,prec::Choice ,assoc::InfixRight,mode::With  , AllTokens },
	{u8"}"       , prec::Never , mode::None   ,prec::Never     ,prec::Never  ,assoc::None      ,mode::None  , AllTokens },
};

// Tokens.
struct token {
	nat8 Index;
	explicit constexpr token(nat8 Index0): Index(Index0) {}
	constexpr token(const char8* Op): Index(nat8(ArraySize(Tokens)-1)) {
		for(; Index>=nat(token::FirstParse()); Index--)
			for(nat j=0;; j++)
				if(Tokens[Index].Symbol[j]!=char8(Op[j]))
					break;
				else if(!Op[j])
					return;
		Index=0;
	}
	constexpr operator nat8() const {return Index;}
	static constexpr token None()       {return token(nat8(0));}
	static constexpr token End()        {return token(1);}
	static constexpr token NewLine()    {return token(2);}
	static constexpr token Alpha()      {return token(3);}
	static constexpr token Digit()      {return token(4);}
	static constexpr token FirstParse() {return token(5);}
	explicit operator bool() const {
		return Index!=0;
	}
	constexpr const token_info* operator->() const {
		return &Tokens[Index];
	}
};

// A set of tokens.
struct token_set {
	constexpr token_set(): Bits{0,0} {}
	template<class... ts> explicit constexpr token_set(token T,ts... TS): token_set(TS...) {
		Bits[nat8(T)/64]|=1LL<<(nat8(T)&63);
	}
	template<class... ts> explicit constexpr token_set(const char8* S,ts... TS): token_set(token(S),TS...) {}
	constexpr bool Has(token T) const {
		return Bits[nat8(T)/64]&(1LL<<(nat8(T)&63));
	}
	constexpr explicit operator bool() const {
		return Bits[0] || Bits[1];
	}
	constexpr token_set operator&(const token_set& Other) const {
		return token_set(Bits[0]&Other.Bits[0],Bits[1]&Other.Bits[1]);
	}
	constexpr token_set operator|(const token_set& Other) const {
		return token_set(Bits[0]|Other.Bits[0],Bits[1]|Other.Bits[1]);
	}
	constexpr token_set operator~() const {
		return token_set(~Bits[0],~Bits[1]);
	}
private:
	constexpr token_set(nat64 Bits0,nat64 Bits1): Bits{Bits0,Bits1} {}
	nat64 Bits[2];
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Token sets.

inline const token_set AllTokens          = ~token_set{};
inline const token_set AllowLess          = ~token_set{u8">",u8">="};
inline const token_set AllowNotEq         = ~token_set{u8">",u8">=",u8"<",u8"<="};
inline const token_set InPrefixes         = token_set{u8":",u8"in"};
inline const token_set StopList           = token_set{u8":)",u8")",u8"]",u8"}",token::NewLine(),token::End()};
inline const token_set StopExpr           = StopList | token_set{u8";",u8","};
inline const token_set StopFun            = StopExpr | token_set{u8"@"};
inline const token_set StopDef            = StopFun  | token_set{u8"=>",u8"next",u8"over",u8"when",u8"while"};
inline const token_set BracePostfixes     = token_set{u8"{"};
inline const token_set BlockPostfixes     = token_set{u8"{",u8".",u8":"};
inline const token_set ParenPostfixes     = token_set{u8"("};
inline const token_set WithPostfixes      = token_set{u8"with",u8"<"};
inline const token_set InvokePostfixes    = BlockPostfixes | ParenPostfixes | WithPostfixes | token_set{u8"in",token::NewLine()};
inline const token_set MarkupPostfixes    = token_set{u8",",u8";",u8">",u8":>"};
inline const token_set SetPostfixes       = token_set{u8"=",u8":=",u8"+=",u8"-=",u8"*=",u8"/="};
inline const token_set DefPostfixes       = token_set{u8"=",u8":="};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Parser.

// Generator-independent base class of parser.
struct parser_base {
private:
	template<class> friend struct parser;

	// A cursor tracks a parsing position in accordance with the Verse grammar,
	// and a snipping position that attributes NewLine to the Space preceding it.
	struct cursor {
		const char8* Pos;           // Pointer to current parse position.
		const char8* LineStart;     // Pointer to start of line.
		const char8* NextLineStart; // If >Pos, indicates we've snipped the NewLine at Pos.
		token        Token;         // Token here.
		nat          TokenSize;     // Length of token.
		nat          Line;          // Zero-based line number.
		char8 operator[](int64 Offset) const {
			return Pos[Offset];
		}
		bool SnippedNewLine() const {
			return NextLineStart>Pos;
		}
	};

	// A point for producing snippets.
	struct point {
		const char8* Pos;
		nat          Line,Column;
		point(const char8* Pos0,nat Line0,nat Column0): Pos(Pos0), Line(Line0), Column(Column0) {}
		point(const cursor& Cursor): 
			Pos   (Cursor.SnippedNewLine()? Cursor.NextLineStart: Cursor.Pos), 
			Line  (Cursor.SnippedNewLine()? Cursor.Line+1:        Cursor.Line), 
			Column(Cursor.SnippedNewLine()? 1:                    nat(Cursor.Pos-Cursor.LineStart+1)) {}
		static point Start(const snippet& Snippet) {return point{Snippet.Text.Start,Snippet.StartLine,Snippet.StartColumn};}
		static point Stop (const snippet& Snippet) {return point{Snippet.Text.Stop,Snippet.StopLine,Snippet.StopColumn};}
	};

	// Grammar context coinciding with "push" and "pop" in the specification.
	struct context {
		const char8* BlockInd;   // Start of the line that initiated our current indentation, or nullptr.
		const char8* TrimInd;    // BlockInd or a more indented block to specify further text trimming.
		bool         Nest;       // Whether we accept lines with equal indentation to BlockInd.
		bool         LinePrefix; // Whether subsequent ScanKey and Commas lines should be prefixed with '&'.
		context(): BlockInd{u8""}, TrimInd{u8""}, Nest(true), LinePrefix(true) {}
	};

	// Tokens.
	nat8 FirstToken [256              ]; // First candidate token per leading char8.
	nat8 NextToken  [ArraySize(Tokens)]; // Next candidate token per token.
	token ParseToken(const char8* Start,nat& Size) {
		if(Start[0]==0)
			return Size=0, token::End();
		for(nat8 i=FirstToken[nat8(Start[0])]; i; i=NextToken[i]) {
			if(i<token::FirstParse())
				return Size=0, token(i);
			auto Symbol = Tokens[i].Symbol;
			nat j;
			for(j=0; Symbol[j] && Start[j]==Symbol[j]; j++);
			if(Symbol[j] || (IsAlnum(Symbol[0])&&IsAlnum(Start[j])))
				continue;
			return Size=j, token(i);
		}
		return Size=0, token::None();
	}

	// State and constructor.
	cursor       Cursor;
	context      Context;
	nat32        ExprDepth{0};
	nat32        CommentDepth{0};
	const nat    InputLength;
	const char8* InputString;
	parser_base(nat InputLength0,const char8* InputString0,nat Line0=1):
		FirstToken{}, NextToken{},
		Cursor{InputString0,InputString0,InputString0,token::None(),0,Line0},
		InputLength(InputLength0), InputString(InputString0) {
		GRAMMAR_ASSERT(InputString[InputLength]==0);
		for(nat c=0u; c<128u; c++)
			FirstToken[c] = 
				IsNewLine(char8(c))? token::NewLine():
				IsEnding(char8(c))?  token::End():
				IsAlpha(char8(c))?   token::Alpha():
				IsDigit(char8(c))?   token::Digit():
									 token::None();
		for(auto Token=nat8(token::FirstParse()); Token<ArraySize(Tokens); Token++) {
			auto& First=FirstToken[nat(Tokens[Token].Symbol[0])];
			if(First)
				NextToken[Token]=First;
			First=token(Token);
		}
	}

	// Consumption.
	void Next(nat n) {
		while(n--)
			GRAMMAR_ASSERT(Cursor[0]!=0), Cursor.Pos++;
	}
	bool Eat(const char8* s) {
		nat n;
		for(n=0; s[n]; n++)
			if(Cursor[n]!=s[n])
				return false;
		return Cursor.Pos+=n, true;
	}
	void EatToken() {
		Cursor.Pos += Cursor.TokenSize;
	}

	// Snippets.
	static snippet Snip(const point& Start,const point& Stop) {
		return snippet{
			Start.Pos,    Stop.Pos,
			Start.Line,   Stop.Line, 
			Start.Column, Stop.Column
		};
	}
	snippet Snip(const point& Start) const {
		return Snip(Start,Cursor);
	}
	snippet Snip() const {
		return Snip(Cursor,Cursor);
	}
	text CursorQuote() {
		static const text Quote[2]={u8"",u8"\""};
		const nat8 Cur0 = nat8(Cursor[0]);
		return Quote[Cur0>0x20 && Cur0!='"' && Cur0<0x7F];
	}
	text CursorText() {
		const nat8 Cur0 = nat8(Cursor[0]);

		// Quoted.
		if((Cur0=='#'&&Cursor[1]=='>') || (Cur0=='<'&&Cursor[1]=='#'))
			return text(Cursor.Pos,Cursor.Pos+2);
		if(IsAlpha(Cur0)) {
			nat n=1;
			while(IsAlnum(Cursor[n]))
				n++;
			return text(Cursor.Pos,Cursor.Pos+n);
		}
		if(Cur0>0x20 && Cur0<=0x7E)
			return text(Cursor.Pos,Cursor.Pos+1);

		// Not quoted.
		if(Cur0=='"')
			return u8"'\"'";
		else if(Cur0>=128 && EncodedLength<place::Printable>(Cursor.Pos))
			return u8"unicode character";
		else if(Cur0>=128)
			return u8"non-unicode character sequence";
		else if(Cur0=='\r' || Cur0=='\n')
			return u8"end of line";
		else if(Cur0=='\t')
			return u8"tab";
		else if(Cur0==' ')
			return u8"space";
		else if(Cur0==0)
			return u8"end of file";
		else
			return u8"ASCII control character";
	}
};

// Generator-dependent parser.
template<class gen_t> struct parser: parser_base {
private:
	using syntax_t                   = typename gen_t::syntax_t;
	using syntaxes_t                 = typename gen_t::syntaxes_t;
	using error_t                    = typename gen_t::error_t;
	using capture_t                  = typename gen_t::capture_t;
	template<class t> using result_t = result<t,error_t>;

	// Constructor.
	const gen_t& Gen;
	parser(const gen_t& Gen0,nat n,const char8* Source0,nat StartLine=1):
        // Accounts for null `Source0` which often occurs with empty files / etc.
		parser_base(n,Source0?Source0:u8"", StartLine), Gen(Gen0) {}

	// Tracking trailing captures across expressions and their postfixes so we can
	// assign them to the lexically outermost generator.
	struct trailing {
		result<cursor,nothing> TrailingStart;
		capture_t              TrailingCapture;
		explicit operator bool() const {
			return bool(TrailingStart);
		}
		void MoveFrom(trailing& Source) {
			GRAMMAR_ASSERT(!TrailingStart);
			TrailingStart        = Source.TrailingStart;
			TrailingCapture      = Source.TrailingCapture;
			Source.TrailingStart = nothing{};
		}
	};

	// Our extended internal block structure tracking block's trailing captures.
	struct block_t: public block<syntaxes_t,capture_t> {
		using block<syntaxes_t,capture_t>::block;
		trailing BlockTrailing;
	};

	// We track a stack of expressions and postfixes at increasing precedence so that we can
	// insert multi-precedence postfix operators like '<' and stop subsequent parsing there.
	// An expr is in one of three states (except mid-update when these invariants don't hold):
	// - Uninitialized: no ExprSyntax, no Trailing, not Finished. 
	// - Initialized:   has ExprSyntax, has Trailing, not Finished.
	// - Finished:      has ExprSyntax, no Trailing, is Finished.
	struct expr {
		cursor                   Start;
		prec                     FinishPrec;
		result<cursor,nothing>   Finished;
		expr*                    OuterExpr;
		token_set                AllowPostfixes;
		result<syntax_t,nothing> ExprSyntax;
		capture_t                ExprLeading;
		trailing                 Trailing;
		result<cursor,nothing>   MarkupStart;
		bool                     MarkupFinished, ExprStop;
		struct expr*             OuterMarkup;
		text                     MarkupTag;
		expr*                    QualIdentTarget;
		expr(prec FinishPrec0,const cursor& Start0,expr* OuterExpr0,token_set AllowPostfixes0=token_set{},expr* QualIdentTarget0=nullptr):
			Start(Start0), FinishPrec(FinishPrec0),
			OuterExpr(OuterExpr0), AllowPostfixes(AllowPostfixes0),
			MarkupFinished(false), ExprStop(false), OuterMarkup(nullptr),
			QualIdentTarget(QualIdentTarget0) {}
		syntax_t operator*() const {
			return *ExprSyntax;
		}
		// Needs a virtual destructor since it has virtual methods or various static analysis will complain
		virtual ~expr() {}
		virtual result_t<nothing> OnFinish(parser& /*Parser*/) {
			GRAMMAR_ASSERT(!Finished);
			GRAMMAR_ASSERT(!OuterExpr || !OuterExpr->Finished);
			GRAMMAR_ASSERT(Trailing);
			Finished = *Trailing.TrailingStart;
			return nothing{};
		}
	};

	// Token management.
	void UpdateToken() {
		Cursor.Token=ParseToken(Cursor.Pos,Cursor.TokenSize);
		if(IsAlpha(Cursor.Token->Symbol[0])) {
			// Key := !Alnum Space !":="
			// When a reserved word is followed by a definition symbol, we demote it to an identifier
			// so that simple object notation supports all identifiers including reserved words.
			cursor    KeyStart     = Cursor;
			EatToken();
			auto      SpaceResult  = Space();
			auto      IsIdentifier = Cursor.Token==token(u8":=");
			Cursor                 = KeyStart;//backtrack but could cache
			if(SpaceResult && IsIdentifier)
				Cursor.Token=token::Alpha();
		}
	}
	bool CheckToken() {
		auto SavedToken=Cursor.Token;
		UpdateToken();
		return Cursor.Token==SavedToken;
	}

	// Errors.
	result_t<nothing> Require(const char8* Value,error_t(parser::*OnError)(text What)) {
		if(!Eat(Value))
			return (this->*OnError)(Value);
		return nothing{};
	}
	result_t<nothing> RequireClose(cursor Start,const char8* Open,const char8* Close,error_t(parser::*OnError)(text)) {
		if(Eat(Close))
			return nothing{};
		else if(!Ending())
			return (this->*OnError)(Close);
		else
			return Cursor=Start, S80(Open);
	}

	// Snippets.
	snippet SnipFinished(const cursor& Start,const expr& End) {
		return Snip(Start,*End.Finished);
	}
	snippet SnipFinished(const cursor& Start,const block_t& End) {
		return Snip(Start,*End.BlockTrailing.TrailingStart);
	}

	// Trailing capture and snippet management.
	result_t<nothing> SpaceTrailing(trailing& Trailing) {
		GRAMMAR_ASSERT(!Trailing);
		Trailing.TrailingStart=Cursor;
		GRAMMAR_RUN(Space(Trailing.TrailingCapture));
		return nothing{};
	}
	result_t<nothing> UpdateFrom(expr& Target,trailing& Source,const result_t<syntax_t>& SyntaxResult) {
		GRAMMAR_ASSERT(Source);
		GRAMMAR_ASSERT(!Target.Finished && !Target.Trailing);
		Target.Trailing.MoveFrom(Source);
		GRAMMAR_SET(Target.ExprSyntax,SyntaxResult);
		return nothing{};
	}
	result_t<nothing> UpdateSpaceTrailing(expr& Target,const result_t<syntax_t>& SyntaxResult) {
		GRAMMAR_ASSERT(!Target.Finished);
		GRAMMAR_ASSERT(!Target.Trailing);
		GRAMMAR_SET(Target.ExprSyntax,SyntaxResult);
		GRAMMAR_RUN(SpaceTrailing(Target.Trailing));
		return nothing{};
	}
	syntax_t ApplyTrailing(expr& Target,bool FinishingNow=false) {
		GRAMMAR_ASSERT(!Target.Finished || FinishingNow);
		GRAMMAR_ASSERT(Target.Trailing);
		Target.ExprSyntax = Gen.Trailing(*Target,Target.Trailing.TrailingCapture);
		Target.Trailing   = trailing{};
		return *Target;
	}
	void ApplyTrailing(block_t& Block0,const point& TrailingEnd) {
		if(Block0.Punctuation!=punctuation::None)
			Gen.CaptureAppend(Block0.PunctuationTrailing,Block0.BlockTrailing.TrailingCapture);
		else
			Gen.CaptureAppend(Block0.ElementsTrailing,Block0.BlockTrailing.TrailingCapture);
		Block0.BlockSnippet  = Snip(point::Start(Block0.BlockSnippet),TrailingEnd);
		Block0.BlockTrailing = trailing{};
	}

	// Character set and comment and errors:
	auto S01() {return Gen.Err(Snip(),"S01","Source must be ASCII or Unicode UTF-8 format");}
	auto S02() {return Gen.Err(Snip(),"S02","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," in block comment");}
	auto S03() {return Gen.Err(Snip(),"S03","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," in line comment");}
	auto S04() {return Gen.Err(Snip(),"S04","Block comment beginning at \"<#\" never ends");}
	auto S05() {return Gen.Err(Snip(),"S05","Ending \"#>\" is outside of block comment");}
	auto S06() {return Gen.Err(Snip(),"S06","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," in indented comment");}

	// Numeric and numbered character constant errors.
	auto S15() {return Gen.Err(Snip(),"S15","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," following number.");}
	auto S16() {return Gen.Err(Snip(),"S15","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," following character.");}
	auto S18() {return Gen.Err(Snip(),"S18","Character code unit octet must be 1-2 digits in the range 0o0 to 0oFF");}
	auto S19() {return Gen.Err(Snip(),"S19","Unicode code point must be 1-6 digits in the range 0u0 to 0u10FFFF");}

	// Identifier errors.
	auto S20(text What) {return Gen.Err(Snip(),"S20","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing identifier following \"",What,"\"");}
	auto S23(text What) {return Gen.Err(Snip(),"S23","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing \"",What,"\" in qualifier");}
	auto S24(text What) {return Gen.Err(Snip(),"S24","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing \"",What,"\" in quoted identifier");}
	auto S25(text What) {return Gen.Err(Snip(),"S25","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing \"",What,"\" in path literal");}
	auto S26(text What) {return Gen.Err(Snip(),"S26","Missing label in path following \"",What,"\"");}

	// Text errors.
	auto S30()          {return Gen.Err(Snip(),"S30","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," in character literal");}
	auto S31(text)      {return Gen.Err(Snip(),"S31","Missing \"'\" in character literal");}
	auto S32(text)      {return Gen.Err(Snip(),"S32","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing end quote in string literal");}
	auto S34()          {return Gen.Err(Snip(),"S34","Bad character escape \"\\\" followed by ",CursorQuote(),CursorText(),CursorQuote());}

	// Markup errors.
	auto S40()                 {return Gen.Err(Snip(),"S40","Missing markup tag preceding ",CursorQuote(),CursorText(),CursorQuote());}
	auto S41()                 {return Gen.Err(Snip(),"S41","Bad markup expression preceding ",CursorQuote(),CursorText(),CursorQuote());}
	auto S42()                 {return Gen.Err(Snip(),"S42","Unexpected markup end tag outside of markup");}
	auto S43(text Tag,text Id) {return Gen.Err(Snip(),"S43","Markup started with \"<",Tag,">\" tag but ended in mismatched \"</",Id,">\" tag");}
	auto S44(text What)        {return Gen.Err(Snip(),"S44","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing \"",What,"\" in markup end tag");}
	auto S46()                 {return Gen.Err(Snip(),"S46","Expected indented markup following \":>\" but got ",CursorQuote(),CursorText(),CursorQuote());}

	// Markup content errors.
	auto S51(text What) {return Gen.Err(Snip(),"S51","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing \"",What,"\" in markup");}
	auto S52(text)      {return Gen.Err(Snip(),"S52","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing markup end tag");}
	auto S54()          {return Gen.Err(Snip(),"S54","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," in indented markup");}
	auto S57()          {return Gen.Err(Snip(),"S57","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," or missing ending \";\" or newline following \"&\" markup escape expression");}
	auto S58()          {return Gen.Err(Snip(),"S58","Markup list separator \"~\" is only allowed in markup beginning with \"~\"; elsewhere escape it using \"\\~\"");}

	// Precedence errors.
	auto S60(text What,text Op) {return Gen.Err(Snip(),"S60","Precedence doesn't allow \"",Op,"\" following \"",What,"\"");}
	auto S61(text Op)           {return Gen.Err(Snip(),"S61","Precedence doesn't allow \"",Op,"\" here");}
	auto S62()                  {return Gen.Err(Snip(),"S62","Verse uses 'and', 'or', 'not' instead of '&&', '||', '!'.");};
	auto S64(text,text Op)      {return Gen.Err(Snip(),"S64","Precedence doesn't allow \"",Op,"\" in markup tag expression");}
	auto S65()                  {return Gen.Err(Snip(),"S65","Use a=b for comparison, not a==b");}
	auto S66(text Op)           {return Gen.Err(Snip(),"S66","Use 'set' before \"",Op,"\" to update variables");}
	auto S67()                  {return Gen.Err(Snip(),"S67","Prefix attribute must be followed by identifier declaration");}
	auto S68()                  {return Gen.Err(Snip(),"S68","Use # for line comment, not //");}
	
	// Bad or missing expression, block, keyword errors.
	auto S70(text)      {return Gen.Err(Snip(),"S70","Expected expression, got ",CursorQuote(),CursorText(),CursorQuote()," at top level of program");}
	auto S71(text What) {return Gen.Err(Snip(),"S71","Expected expression, got ",CursorQuote(),CursorText(),CursorQuote()," following \"",What,"\"");}
	auto S74(text)      {return Gen.Err(Snip(),"S74","Expected markup tag expression, got ",CursorQuote(),CursorText(),CursorQuote());}
	auto S76(text What) {return Gen.Err(Snip(),"S76","Expected block, got ",CursorQuote(),CursorText(),CursorQuote()," following \"",What,"\"");}
	auto S77()          {return Gen.Err(Snip(),"S77","Unexpected ",CursorQuote(),CursorText(),CursorQuote()," following expression");}
	auto S78()          {return Gen.Err(Snip(),"S78","Expected <specifier> following \"with\"");}
	auto S79()          {return Gen.Err(Snip(),"S79","Unexpected ",CursorQuote(),CursorText(),CursorQuote(),"or missing \">\" following specifier");}

	// Expression grouping errors:
	auto S80(text What) {return Gen.Err(Snip(),"S80","Block starting in \"",What,"\" never ends");}
	auto S81(text What) {return Gen.Err(Snip(),"S81","Expected expression or \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in parenthesis");}
	auto S82(text What) {return Gen.Err(Snip(),"S82","Expected expression or \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in parenthesized parameter list");}
	auto S83(text What) {return Gen.Err(Snip(),"S83","Expected expression or \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in bracketed parameters");}
	auto S84(text What) {return Gen.Err(Snip(),"S84","Expected expression or \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in braced block");}
	auto S85(text What) {return Gen.Err(Snip(),"S85","Expected \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in prefix brackets");}
	auto S86(text What) {return Gen.Err(Snip(),"S86","Expected expression or \"",What,"\", got ",CursorQuote(),CursorText(),CursorQuote()," in string interpolation");}
	auto S88(text)      {return Gen.Err(Snip(),"S88","Expected expression, got ",CursorQuote(),CursorText(),CursorQuote()," in indented block");}
	auto S88void()      {return Gen.Err(Snip(),"S88","Expected expression, got ",CursorQuote(),CursorText(),CursorQuote()," in indented block");}
	auto S89()          {return Gen.Err(Snip(),"S89","Indentation mismatch: expected ",Context.BlockInd[point(Cursor).Column]==' '? "space": "tab",", got ",CursorQuote(),CursorText(),CursorQuote());}

	// Parser limitations versus spec.
	auto S97()          {return Gen.Err(Snip(),"S97","Unexpected error");}
	auto S98()          {return Gen.Err(Snip(),"S98","Feature is not currently supported");}
    auto S99()          {return Gen.Err(Snip(),"S99","Exceeded maximum expression depth"); }

	// Blank space and indentation.
	void SnipNewLine(capture_t& Capture,place Place=place::Space) {
		// If a NewLine is ahead, incorporate it in Capture despite not consuming it per grammar spec.
		if(!Cursor.SnippedNewLine() && (Cursor[0]==0x0D || Cursor[0]==0x0A)) {
			auto Start           = Cursor;
			Cursor.NextLineStart = Cursor.Pos+1+(Cursor[0]==0x0D && Cursor[1]==0x0A);
			Gen.NewLine(Capture,Snip(Start),Place);
		}
	}
	bool NewLine(capture_t& Capture,place Place=place::Space) {
		// NewLine := 0o0D [0o0A] | 0o0A
		SnipNewLine(Capture,Place);
		if(Cursor.SnippedNewLine()) {
			Cursor.Pos       = Cursor.NextLineStart;
			Cursor.LineStart = Cursor.Pos;
			Cursor.Line++;
			return true;
		}
		return false;
	}
	bool Ending() {
		// Ending := &(NewLine | end)
		return Cursor.SnippedNewLine() || IsEnding(Cursor[0]);
	}
	result_t<nothing> Space(capture_t& Capture,place Place=place::Space,bool DoSnipNewLine=true) {
		// Space := {0o09 | 0o20 | Comment}
		GRAMMAR_RUN(Text<place::Space>(Capture,Place));
		if(DoSnipNewLine)
			SnipNewLine(Capture,Place);
		return UpdateToken(), nothing{};
	}
	result_t<capture_t> Space(place Place=place::Space) {
		capture_t Capture;
		GRAMMAR_RUN(Space(Capture,Place));
		return Capture;
	}
	result_t<context> Ind() {
		// Ind := Ending push; set Nest=false; set BlockInd=LineInd; set LinePrefix=""
		GRAMMAR_ASSERT(Ending());
		auto SavedContext   = Context;
		Context.BlockInd    = Cursor.LineStart;
		Context.TrimInd     = Cursor.LineStart;
		Context.Nest        = false;
		return SavedContext;
	}
	result_t<nothing> Ded(const context& SavedContext,error_t(parser::*OnError)()) {
		// Ded := Ending pop
		Context = SavedContext;
		if(!Ending())
			return (this->*OnError)();
		return UpdateToken(), nothing{};
	}
	result_t<bool> Line(capture_t& Capture,place Place) {
		// Line := NewLine; parse i:={0o09|0o20}; (Ending | !(0o09|0o20) Space
		//         if     (i>BlockInd | Nest and i=BlockInd) then set LineInd=ThisInd
		//         else if(not i<=BlockInd                 ) then error)
		auto SavedLineEnd = Cursor;
		if(!NewLine(Capture,Place))
			return false;
		auto SavedLineStart = Cursor;
		while(IsSpace(Cursor[0]) && Cursor[0]==Context.BlockInd[Cursor.Pos-SavedLineStart.Pos])
			Next(1);
		bool HasMoreSpace = IsSpace(Cursor[0]);
		if((HasMoreSpace || Context.Nest) && !IsSpace(Context.BlockInd[Cursor.Pos-SavedLineStart.Pos])) {
			// This line falls into current indented block, so consume any additional optional TrimIn
			// and note via Gen.Indent followed by potentially Place-significant Space.
			while(IsSpace(Cursor[0]) && Cursor[0]==Context.TrimInd[Cursor.Pos-SavedLineStart.Pos])
				Next(1);
			Gen.Indent(Capture,Snip(SavedLineStart),Place);
			GRAMMAR_RUN(Space(Capture,Place));
			return true;
		}
		else if(Ending()) {
			// Blank line whose indentation isn't related to leading.
			return Gen.BlankLine(Capture,Snip(SavedLineStart),Place), true;
		}
		else if(HasMoreSpace) {
			// Inconsistently indented nonblank line, so error at inconsistency.
			return S89();
		}
		else {
#if TIM
			return Cursor=SavedLineEnd, false; // Backtrack but could cache.
#else
			// NOTE: (yiliang.siew) For indented indcmts, such as:
			/*
			 *
			 * ```
			 * a<#>
			 *  b<#>
			 * c<#>
			 *  d<#>
			 * <#>
			 *  e<#>
			 * ```
			 *
			 * And so on, the parser will end up treating the entirety of the contents after `a<#>` as an indcmt,
			 * and recursively do so for the contents after `b<#>` and so on. This can lead to really slow parsing/stack overflow
			 * should a malicious actor craft Verse syntax to take advantage of this.
			 * We therefore only capture indented comments up to a certain point and give up; it's highly unlikely anyone would need
			 * that amount of indentation in their comments.
			 */
			const uint32_t NewCommentDepth = Place == place::IndCmt || Place == place::BlockCmt ? CommentDepth + 1 : CommentDepth;
			scoped_guard CommentDepthGuard(CommentDepth, NewCommentDepth);
			if (CommentDepth > VERSE_MAX_INDCMT_DEPTH) {
				return Cursor=SavedLineEnd, false; //backtrack but could cache
			}
			
			// Line that only contains whitespace or comments, but less indented than the current block.
			// If we have reached this point, it means that we might be at the start of a comment on the next line, but
			// the comment might not have any indentation at all.
			capture_t SpaceCapture = {};
			GRAMMAR_RUN(Space(SpaceCapture,Place));
			if(Cursor.SnippedNewLine()) {
				// We need to keep eating until we hit a token that is non-whitespace/comment and has different indentation. If so, backtrack.
				// If no backtracking, return `true` and append the capture here.
				// If this `Scan` fails to extend the current block because there is already a non-whitespace/comment token there,
				// it will already have backtracked to the previous line. But the capture would still be empty since `Scan` would
				// return a `nothing{}` value.
				capture_t ScanCapture = {};
				if (Scan(ScanCapture,Place) && Gen.CaptureLength(ScanCapture) == 0) {
					// NOTE: (yiliang.siew) Backtrack here so that the comment can be associated with the correct capture.
					return Cursor=SavedLineEnd, false;
				}
				else {
					Gen.CaptureAppend(Capture, SpaceCapture);
					return true;
				}
			}
			else {
				// Consistent nonblank line from an earlier indented block.
				return Cursor=SavedLineEnd, false;//backtrack but could cache
			}
#endif
		}
	}
	result_t<nothing> Scan(capture_t& Capture,place Place=place::Space) {
		// Scan := Space {Line}
		GRAMMAR_RUN(Space(Capture,Place,false));
		for(;;) {
			capture_t LineCapture;
			GRAMMAR_LET(GotLine,Line(LineCapture,Place));
			if(!GotLine)
				return UpdateToken(), nothing{};

			// In place::Content, trim trailing [NewLine Space &('~' | '</')].
			if(Place==place::Content && (Cursor[0]=='~' || (Cursor[0]=='<'&&Cursor[1]=='/')))
				Gen.MarkupTrim(LineCapture);
			Gen.CaptureAppend(Capture,LineCapture);
		}
	}
	result_t<token> ScanKey(capture_t& Capture,token_set TokenSet) {
		// This function implements the grammar for Brace and ScanKey [Token1 | Token2 | ..] &Key.
		// Brace   := Scan '{' List '}' Space
		// ScanKey := Space (&NewLine Scan LinePrefix Space | !NewLine)
		// Key     := !Alnum Space !":="
		auto ScanStart = Cursor;
		GRAMMAR_LET(More,Space());
		bool Multiline = Ending();
		GRAMMAR_RUN(Scan(More));
		if(Context.LinePrefix && Multiline && Cursor.Token!=token(u8"{")) {
			auto LinePrefixStart=Cursor;
			if(Eat(u8"&")) {
				Gen.LinePrefix(More,Snip(LinePrefixStart));
				GRAMMAR_RUN(Space(More));
				if(TokenSet.Has(Cursor.Token))
					return Gen.CaptureAppend(Capture,More), Cursor.Token;
			}
		}
		else if(TokenSet.Has(Cursor.Token))
			return Gen.CaptureAppend(Capture,More), Cursor.Token;
		return Cursor=ScanStart, token::None(); //backtrack but could cache
	}

	// Constants and base expressions.
	result_t<nat> ParseHex(nat MaxDigits,nat MaxValue,error_t(parser::*OnError)()) {
		nat i=0;
		while(IsHex(Cursor[0])) {
			if(MaxDigits-->0) {
				auto i0=i;
				i=i*16+DigitValue(Cursor[0]);
				if(i<=MaxValue && i/16==i0) {
					Next(1);
					continue;
				}
			}
			return (this->*OnError)();
		}
		return i;
	}
	result_t<nothing> DisallowDotAlnum() {
		bool GotDot=Cursor[0]=='.';
		if(IsAlnum(Cursor[GotDot]))
			return S15();
		return nothing{};
	}
	result_t<nothing> DisallowDotNum() {
		bool GotDot=Cursor[0]=='.';
		if(IsDigit(Cursor[GotDot]))
			return S15();
		return nothing{};
	}
	result_t<syntax_t> Num() {
		// Exp    := [('e'|'E') ['+'|'-'] Digits] !(('e'|'E') ('+'|'-'|Digit))
		// Units  := [Alpha {Alpha}] !Alpha
		// Num    := !(("0b"|"0o"|"0u"|"0x") Hex) Digits ['.' Digits] Exp Units) !('.' Digits)
        //        |  ("0x" Hex {Hex} !('.' Alnum)
		auto Start=Cursor;
		GRAMMAR_ASSERT(IsDigit(Cursor[0]));
		if(Cursor[0]=='0' && Cursor[1]=='x' && IsHex(Cursor[2])) {
			GRAMMAR_ASSERT(Cursor[0]=='0'&&Cursor[1]=='x'&&IsHex(Cursor[2]));
			Next(2);
			do {Next(1);} while(IsHex(Cursor[0]));
			// Could use `DisallowDotNum()` which would then permit extension function on hex literals - `0xff.ShiftRight()`
			// Can still wrap hex literals with parentheses - `(0xff).ShiftRight()`
			GRAMMAR_RUN(DisallowDotAlnum());
			return Gen.NumHex(Snip(Start),text(Start.Pos+2,Cursor.Pos));
		}
		while(IsDigit(Cursor[0]))
			Next(1);
		text Digits(Start.Pos,Cursor.Pos),FractionalDigits(Cursor.Pos+1,Cursor.Pos+1);
		if(Cursor[0]=='.' && IsDigit(Cursor[1])) {
			Next(2);
			while(IsDigit(Cursor[0]))
				Next(1);
			FractionalDigits.Stop=Cursor.Pos;
		}
		text ExponentSign,Exponent;
		if(Cursor[0]=='e' || Cursor[0]=='E') {
			int64 HasExponentSign = int64(Cursor[1]=='+' || Cursor[1]=='-');
			if(IsDigit(Cursor[1+HasExponentSign])) {
				ExponentSign=text(Cursor.Pos+1,Cursor.Pos+1+HasExponentSign);
				Next(1+HasExponentSign);
				Exponent.Start=Cursor.Pos;
				while(IsDigit(Cursor[0]))
					Next(1);
				Exponent.Stop=Cursor.Pos;
			}
		}
		GRAMMAR_LET(Result,Gen.Num(Snip(Start),Digits,FractionalDigits,ExponentSign,Exponent));
		if(IsAlpha(Cursor[0])) {
			auto Pos0=Cursor.Pos;
			do Next(1);
			while(IsAlnum(Cursor[0]));
			GRAMMAR_SET(Result,Gen.Units(Snip(Start),Result,text(Pos0,Cursor.Pos)));
		}
		// Disallow extra dot digit and allow dot alpha so extension functions ['.' Ident] can be called on number literals
		GRAMMAR_RUN(DisallowDotNum());
		return Result;
	}
	result_t<syntax_t> CharLit() {
		// Special := '\'|'{'|'}'|'#'|'<'|'>'|'&'|'~'
		// CharEsc := '\' ('r'|'n'|'t'|'''|'"'|Special)
		// CharLit := ''' Printable ''' !''' | ''' CharEsc '''
		GRAMMAR_ASSERT(Cursor[0]=='\'');
		auto Start=Cursor;
		Next(1);
		nat n=EncodedLength<place::Printable>(Cursor.Pos);
		if(!n)
			return S30();
		auto Char32    = EncodedChar32(Cursor.Pos,n);
		auto Backslash = Cursor[0]=='\\' && Cursor[1] && Cursor[2]=='\'';
		if(Backslash) {
			Next(1);
			if(IsStringBackslashLiteral(Cursor[0],Cursor[1])) {
				Char32=char32(Cursor[0]=='r'? '\r': Cursor[0]=='n'? '\n': Cursor[0]=='t'? '\t': Cursor[0]);
				Backslash=1;
				Next(n);
			}
			else return S34();
		}
		else Next(n);
		GRAMMAR_RUN(Require(u8"'",&parser::S31));
		return Gen.Char32(Snip(Start),Char32,false,Backslash);
	}
	result_t<char8> Char8() {
		// Char8 := "0o" (Hex) [Hex] !Alnum
		GRAMMAR_ASSERT(Cursor[0]=='0'&&Cursor[1]=='o'&&IsHex(Cursor[2]));
		Next(2);
		GRAMMAR_LET(n,ParseHex(2,0xFFULL,&parser::S18));
		if(IsAlnum(Cursor[0]))
			return S16();
		return char8(n);
	}
	result_t<char32> Char32() {
		// Char32 := "0u" ("10" | Hex) [Hex] [Hex] [Hex] [Hex]) !Alnum
		GRAMMAR_ASSERT(Cursor[0]=='0'&&Cursor[1]=='u'&&IsHex(Cursor[2]));
		Next(2);
		GRAMMAR_LET(n,ParseHex(6,0x10FFFFULL,&parser::S19));
		if(IsAlnum(Cursor[0]))
			return S16();
		return char32(n);
	}
	result_t<text> Ident() {
		// Ident := Alpha {Alnum} !Alnum ["'" {!('<#'|'#>'|'\'|'{'|'}'|'"'|''') 0o20-0o7E} "'"]
		GRAMMAR_ASSERT(IsAlpha(Cursor[0]));
		auto Pos0=Cursor.Pos;
		do Next(1);
		while(IsAlnum(Cursor[0]));
		if(!Eat(u8"'"))
			return text(Pos0,Cursor.Pos);
        // Ensure not reading past string and determine if quotable
		while((Cursor[0] != '\0') && IsIdentifierQuotable(Cursor[0], Cursor[1]))
			Next(1);
		GRAMMAR_RUN(Require(u8"'",&parser::S24));
		return text(Pos0,Cursor.Pos);
	}
	result_t<text> Path() {
		// Path := '/' Label ('@' Label | !'@')] {'/' ['(' Path ':)'] Ident} !'/'
		auto Start=Cursor;
		GRAMMAR_RUN(Require(u8"/",&parser::S25));
		if(Cursor[0]=='/' || (Cursor[0]==' ' && Cursor.Pos>InputString && Cursor[-1]=='/'))
			return S68();
		GRAMMAR_RUN(Label(u8"/"));
		if(Eat(u8"@"))
			GRAMMAR_RUN(Label(u8"@"));
		while(Eat(u8"/")) {
			text What=u8"/";
			if(Eat(u8"(")) {
				GRAMMAR_RUN(Path());
				GRAMMAR_RUN(Require(u8":)",&parser::S25));
				What=u8":)";
			}
			if(IsAlpha(Cursor[0])) {
				GRAMMAR_RUN(Ident());
				continue;
			}
			return S20(What);
		}
		if(Cursor[0]!='/')
			return text(Start.Pos,Cursor.Pos);
		return S25(u8"/");
	}
	result_t<text> Label(text What) {
		// Label := Alnum {Alnum|'-'|'.'} !(Alnum|'-'|'.')
		auto Pos0=Cursor.Pos;
		if(IsAlnum(Cursor[0])) {
			Next(1);
			while(IsAlnum(Cursor[0]) || Cursor[0]=='-' || Cursor[0]=='.')
				Next(1);
			return text(Pos0,Cursor.Pos);
		}
		return S26(What);
	}

	// Text processing.
	result_t<capture_t> LineCmt() {
		// LineCmt := '#' !'>' {Text} Ending
		GRAMMAR_ASSERT(Cursor[0]=='#');
		Next(1);
		capture_t Capture;
		GRAMMAR_RUN(Text<place::LineCmt>(Capture));
		if(Ending())
			return Capture;
		else
			return S03();
	}
	result_t<capture_t> BlockCmt() {
		// BlockCmt := "<#" !'>' {Text|NewLine} !'<' "#>"
		GRAMMAR_ASSERT(Cursor[0]=='<'&&Cursor[1]=='#'&&Cursor[2]!='>');
		auto Start=Cursor;
		Next(2);
		capture_t Capture;
		GRAMMAR_RUN(Text<place::BlockCmt>(Capture));
		if(Cursor[0]=='#' && Cursor[1]=='>')
			return Next(2), Capture;
		else if(Cursor[0]==0)
			return Cursor=Start, S04();
		else
			return S02();
	}
	result_t<capture_t> IndCmt() {
		// IndCmt := "<#>" {Text} Ind {Text|Line} Ded
		GRAMMAR_ASSERT(Cursor[0]=='<'&&Cursor[1]=='#'&&Cursor[2]=='>');
		Next(3);
		capture_t Capture;
		GRAMMAR_RUN(Text<place::LineCmt>(Capture));
		if(Ending()) {
			GRAMMAR_LET(SavedContext,Ind());
			GRAMMAR_RUN(Text<place::IndCmt>(Capture));
			GRAMMAR_RUN(Ded(SavedContext,&parser::S06));
			// NOTE: (yiliang.siew) We don't want to snip a newline here, because that means that an indcmt like:
			/*
			 * <#>indcmt
			 *     indcmt_frag
			 * stub{}
			 * 
			 */
			// Ends up getting an extra newline snipped as part of its comment string capture, which wouldn't make sense
			// since indcmts must always have a newline after anyway.
			// GRAMMAR_RUN(Space(Capture,place::IndCmt));
			return Capture;
		}
		else return S06();
	}
	template<place ParsePlace> result_t<nothing> Text(capture_t& Capture,place GenPlace=ParsePlace) {
		// Text      := Printable | BlockCmt | "<#>"
		// LineCmt   := '#' !'>' {Text} Ending
		// BlockCmt  := "<#" !'>' {Text|NewLine} !'<' "#>"
		// IndCmt    := "<#>" {Text} Ind {Text|Line} Ded
		// Space     := {0o09 | 0o20 | Comment}
		// String    := '"' {..                  | CharEsc |      !('\'|'{'|'}'|'"') Text} '"'
		// Content   :=     {.. | Comment | Line | CharEsc | .. | !Special           Text}
		// CharEsc   := '\' ('r'|'n'|'t'|'''|'"'|Special)
		for(;;) {
			auto Start=Cursor;
			for(nat n; (n=EncodedLength<ParsePlace>(Cursor.Pos))!=0;)
				Next(n);
			if(Cursor.Pos!=Start.Pos)
				Gen.Text(Capture,Snip(Start),GenPlace);
			auto SpecialStart=Cursor;
			switch(Cursor[0]) {
			case '\r': case '\n':
				if constexpr(ParsePlace==place::Content || ParsePlace==place::IndCmt) {
					GRAMMAR_RUN(Scan(Capture,GenPlace));
					if(Ending())
						return nothing{};
					continue;
				}
				else if constexpr(ParsePlace==place::BlockCmt) {
					NewLine(Capture,GenPlace);
					continue;
				}
				else return nothing{};
			case '#':
				if(Cursor[1]!='>') {
					GRAMMAR_LET(Commentary,LineCmt());
					Gen.LineCmt(Capture,Snip(SpecialStart),GenPlace,Commentary);
					continue;
				}
				else if constexpr(ParsePlace==place::BlockCmt)
					return nothing{};
				else
					return S05();
			case '<':
				if(Cursor[1]!='#') {
					return nothing{};
				}
				else if(Cursor[2]!='>') {
					GRAMMAR_LET(Commentary,BlockCmt());
					Gen.BlockCmt(Capture,Snip(SpecialStart),GenPlace,Commentary);
					continue;
				}
				else if constexpr(ParsePlace==place::Space || ParsePlace==place::Content || ParsePlace==place::IndCmt) {
					GRAMMAR_LET(Commentary,IndCmt());
					Gen.IndCmt(Capture,Snip(SpecialStart),GenPlace,Commentary);
					continue;
				}
				else {
					Next(3);
					Gen.Text(Capture,Snip(SpecialStart),GenPlace);
					continue;
				}
			case '\\':
				// Parse a constant escape.
				// Special  := '\'|'{'|'}'|'#'|'<'|'>'|'&'|'~'
				// CharEsc  := '\' ('r'|'n'|'t'|'''|'"'|Special)
				if constexpr(ParsePlace==place::String || ParsePlace==place::Content) {
					Next(1);
					if(Cursor[0] && IsStringBackslashLiteral(Cursor[0],Cursor[1])) {
						auto Backslashed = Cursor[0];
						Next(1);
						Gen.StringBackslash(Capture,Snip(SpecialStart),GenPlace,Backslashed);
						continue;
					}
					else return S34();
				}
			default:
				return nothing{};
			}
		}
	}
	result_t<block_t> Interp() {
		// Interp := '{' List '}'
		GRAMMAR_ASSERT(Cursor[0]=='{');
		auto Start=Cursor;
		Next(1);
		GRAMMAR_LET(Block0,List(u8"}",&parser::S86,Cursor,capture_t(),punctuation::None,Cursor));
		GRAMMAR_RUN(RequireClose(Start,u8"{",u8"}",&parser::S86));
		return Block0;
	}
	result_t<block_t> Ampersand() {
		// Ampersand := push; parse LinePrefix='&'; Space Def (';'|Ending); pop
		GRAMMAR_ASSERT(Cursor[0]=='&');
		Next(1);
		auto ExprStart          = Cursor;
		GRAMMAR_LET(Leading,Space());
		auto SavedContext       = Context;
		Context.LinePrefix      = true;
		GRAMMAR_LET(Block0,WhenExpr(u8"&",prec::Def,prec::Def,nullptr,Leading,[&](expr& Expr)->result_t<block_t> {
			ApplyTrailing(Expr,true);
			auto SemicolonStart = Cursor;
			bool Semicolon      = Eat(u8";");
			auto Block0         = SingletonBlock(ExprStart,Expr);
			if(!Ending() && !Semicolon)
				return S57();
			if(Semicolon)
				Gen.Semicolon(Block0.ElementsTrailing,Snip(SemicolonStart));
			ApplyTrailing(Block0,Cursor);
			return Block0;
		},AllTokens));
		Context                 = SavedContext;
		return Block0;
	}
	template<place Place> result_t<syntaxes_t> String(cursor TextStart,capture_t Leading=capture_t()) {
		// String  := '"' {Interp | CharEsc | !('\'|'{'|'}'|'"') Text} '"'
		// Content :=     {Interp | CharEsc | Markup | Ampersand | Comment | Line | !Special Text}
		syntaxes_t Splices;
		for(;;) {
			GRAMMAR_RUN(Text<Place>(Leading));
			if(Cursor.Pos!=TextStart.Pos) {
				GRAMMAR_LET(S,Gen.StringLiteral(Snip(TextStart),Leading));
				Gen.SyntaxesAppend(Splices,S);
			}
			auto SpecialStart=Cursor;
			switch(Cursor[0]) {
			case '{': {
				GRAMMAR_LET(Block0,Interp());
				GRAMMAR_LET(S,Gen.StringInterpolate(Snip(SpecialStart),Place,1,Block0));
				Gen.SyntaxesAppend(Splices,S);
				break;
			}
			case '&': {
				GRAMMAR_LET(Block0,Ampersand());
				GRAMMAR_LET(S,Gen.StringInterpolate(Snip(SpecialStart),Place,0,Block0));
				Gen.SyntaxesAppend(Splices,S);
				break;
			}
			case '<':
				// Markup := '<' Tags ..
				// Tags   := Space (!'/' ..) ..
				if(Cursor[1]!='/') {
					GRAMMAR_LET(e,Markup());
					Gen.SyntaxesAppend(Splices,e);
					break;
				}
				[[fallthrough]];
			default:
				return Splices;
			}
			TextStart=Cursor;
			Leading=capture_t();
		}
	}

	// Markup content.
	result_t<syntax_t> Contents(bool TrimLeading) {
		// Contents := Scan (Content | '~' Content {'~' Content})
		auto Start=Cursor;
		GRAMMAR_LET(Leading,Space(place::Content)); // If TrimLeading, trim leading [Space NewLine].
		if(TrimLeading && Ending())
			Gen.MarkupTrim(Leading);
		GRAMMAR_RUN(Scan(Leading,place::Content));
		if(Cursor[0]!='~') {
			GRAMMAR_LET(Splices,String<place::Content>(Start,Leading));
			if(Cursor[0]=='~')
				return S58();
			return Gen.Content(Snip(Start),Splices);
		}
		else {
			Next(1);
			Gen.MarkupTrim(Leading); // Trim everything before ~.
			syntaxes_t Results;
			do {
				auto ElementStart=Cursor;
				GRAMMAR_LET(Splices,String<place::Content>(Cursor));
				GRAMMAR_LET(S,Gen.Content(Snip(ElementStart),Splices));
				Gen.SyntaxesAppend(Results,S);
			}
			while(Eat(u8"~"));
			return Gen.Contents(Snip(Start),Leading,Results);
		}
	}
	result_t<syntax_t> Trimmed(bool TrimLeading) {
		// We push and set TrimInd to LineInd so markup can precisely trim according to LineStart.
		auto SavedContext = Context;
		Context.TrimInd   = Cursor.LineStart;
		Context.Nest      = true;
		GRAMMAR_LET(Result,Contents(TrimLeading));
		Context           = SavedContext;
		return Result;
	}

	// Blocks.
	block_t SingletonBlock(const snippet& Snippet,const syntax_t& Syntax,const capture_t& PunctuationLeading=capture_t(),punctuation Punctuation=punctuation::None) {
		block_t Block0(Snippet);
		Block0.PunctuationLeading  = PunctuationLeading;
		Block0.Punctuation         = Punctuation;
		Gen.SyntaxesAppend(Block0.Elements,Syntax);
		return Block0;
	}
	block_t SingletonBlock(const cursor& BlockStart,expr& Expr,const capture_t& PunctuationLeading=capture_t(),punctuation Punctuation=punctuation::None) {
		auto Block0=SingletonBlock(SnipFinished(BlockStart,Expr),*Expr,PunctuationLeading,Punctuation);
		Block0.BlockTrailing.MoveFrom(Expr.Trailing);
		return Block0;
	}
	result_t<block_t> IndList(cursor Start,const capture_t& PunctuationLeading,punctuation Punctuation,cursor LeadingStart,const capture_t& Leading=capture_t()) {
		// Ind List Ded
		GRAMMAR_LET(SavedContext,Ind());
		GRAMMAR_LET(Block0,List(u8"",&parser::S88,Start,PunctuationLeading,Punctuation,LeadingStart,Leading));
		GRAMMAR_RUN(Ded(SavedContext,&parser::S88void));
		GRAMMAR_RUN(SpaceTrailing(Block0.BlockTrailing));
		return Block0;
	}
	result_t<block_t> BlockHelper(text What,prec Prec,expr& OuterExpr,cursor BlockStart,capture_t PunctuationLeading,
		bool AllowOpen,bool AllowInd,bool AllowCommas,bool* Fails=nullptr) {
		// Brace     := Scan '{' List '}' Space
		// Block     := Brace | DotSpace Space Def Space | (DotSpace | ':') Space Ind List Ded
		// BraceInd  := Brace | Ind List Ded
		// DotSpace  := '.' (0o09 | 0o20 | Ending) Space
		switch(nat8(Cursor.Token)) {
		case token::NewLine(): case token::End(): {
			GRAMMAR_LET(ScanToken,ScanKey(PunctuationLeading,BracePostfixes));
			if(!ScanToken) {
				if(AllowInd)
					return IndList(BlockStart,PunctuationLeading,punctuation::Ind,Cursor);
				goto bad;
			}
			[[fallthrough]];
		}
		case token(u8"{"): {
			auto BraceStart=Cursor;
			EatToken();
			GRAMMAR_LET(Block0,List(u8"}",&parser::S84,Cursor,PunctuationLeading,punctuation::Braces,Cursor));
			GRAMMAR_RUN(RequireClose(BraceStart,u8"{",u8"}",&parser::S84));
			Block0.BlockSnippet=Snip(BlockStart);
			GRAMMAR_RUN(SpaceTrailing(Block0.BlockTrailing));
			return Block0;
		}
		case token(u8"."): {
			if(AllowOpen && (IsSpace(Cursor[1]) /*|| IsEnding(Cursor[1])*/)) {
				EatToken();
				//auto MiddleStart=Cursor;
				GRAMMAR_LET(Middle,Space());
				/*if(Ending())
					return IndList(BlockStart,PunctuationLeading,punctuation::Dot,MiddleStart,Middle);*/
				return WhenExpr(What,prec::Def,prec::Def,&OuterExpr,Middle,[&](expr& Right)->result_t<block_t> {
					return SingletonBlock(BlockStart,Right,PunctuationLeading,punctuation::Dot);
				});
			}
			goto bad;
		}
		case token(u8":"): {
			if(AllowOpen) {
				auto ColonStart=Cursor;
				EatToken();
				auto MiddleStart=Cursor;
				GRAMMAR_LET(Middle,Space());
				if(Ending())
					return IndList(BlockStart,PunctuationLeading,punctuation::Colon,MiddleStart,Middle);
				Cursor=ColonStart; //backtrack colon and space, then fall through.
			}
			[[fallthrough]];
		}
		default:
			if(Prec!=prec::Nothing) {
				if(AllowCommas)
					return Commas(What,Prec,BlockStart,PunctuationLeading,&parser::S71);
				else
					return WhenExpr(What,Prec,Prec,&OuterExpr,PunctuationLeading,[&](expr& Right)->result_t<block_t> {
						return SingletonBlock(BlockStart,Right);
					});
			}
		bad:
			if(!Fails)
				return S71(What);
			else
				return *Fails=true, block_t{};
		}
	}
	result_t<block_t> Block(text What,expr& OuterExpr,cursor BlockStart,const capture_t& PunctuationLeading,bool& Fails) {
		// Block := Brace | DotSpace Space Def Space | (DotSpace | ':') Space Ind List Ded
		return BlockHelper(What,prec::Nothing,OuterExpr,BlockStart,PunctuationLeading,true,false,false,&Fails);
	}
	result_t<block_t> BraceInd(text What,prec Prec,expr& OuterExpr) {
		// BraceInd := Brace | Ind List Ded
		auto BlockStart=Cursor;
		GRAMMAR_LET(PunctuationLeading,Space());
		return BlockHelper(What,Prec,OuterExpr,BlockStart,PunctuationLeading,false,true,false);
	}
	result_t<block_t> KeyBlock(prec Prec,expr& OuterExpr,cursor BlockStart,const capture_t& TokenLeading,text Token,const capture_t& PunctuationLeading) {
		// KeyBlock := Block
		GRAMMAR_LET(Block0,BlockHelper(Token,Prec,OuterExpr,BlockStart,PunctuationLeading,true,false,false));
		Block0.Token        = Token;
		Block0.TokenLeading = TokenLeading;
		return Block0;
	}
	result_t<block_t> KeyBlockDefs(expr& OuterExpr,cursor BlockStart,const capture_t& TokenLeading,text Token) {
		// Defs := Def {Space ',' Scan Def}
		GRAMMAR_LET(PunctuationLeading,Space());
		GRAMMAR_LET(Block0,BlockHelper(Token,prec::Def,OuterExpr,BlockStart,PunctuationLeading,true,false,true));
		Block0.Token        = Token;
		Block0.TokenLeading = TokenLeading;
		return Block0;
	}
	template<class f> result_t<nothing> WhenBraceCall(const char8* What,prec Prec,expr& OuterExpr,const f& F) {
		// Brace  := Scan '{' List '}' Space
		// Prefix := Call | .. Space (Brace | Prefix)
		// Takes a callback because things like +a<b preemptively invoke OnFinish.
		auto BlockStart=Cursor;
		GRAMMAR_LET(PunctuationLeading,Space());
		if(Cursor.Token==token(u8"{") || Cursor.Token==token::NewLine()) {
			GRAMMAR_LET(RightBlock,BlockHelper(What,Prec,OuterExpr,BlockStart,PunctuationLeading,false,false,false));
			return F(RightBlock);
		}
		else return WhenExpr(What,Prec,Prec,&OuterExpr,PunctuationLeading,[&](expr& RightExpr)->result_t<nothing> {
			auto RightBlock=SingletonBlock(BlockStart,RightExpr);
			return F(RightBlock);
		});
	}

	// Qualified identifiers.
	result_t<syntax_t> QualIdentQualified(expr& Target,const cursor& Start,block_t& Block0) {
		GRAMMAR_RUN(Space(Block0.PunctuationTrailing));
		Block0.BlockSnippet = Snip(Start);
		Block0.Punctuation  = punctuation::Qualifier;
		if(IsAlpha(Cursor[0])) {
			GRAMMAR_LET(Id,Ident());
			Target.MarkupTag=Id;
			return Gen.QualIdent(Snip(Start),Block0,Id);
		}
		else return S23(u8":)");
	}
	result_t<syntax_t> QualIdent(text What,expr& Target,bool AllowParenthesis) {
		// QualIdent := ['(' List ':)' Space] Ident
		auto Start=Cursor;
		if(IsAlpha(Cursor[0])) {
			GRAMMAR_LET(Id,Ident());
			Target.MarkupTag=Id;
			return Gen.Ident(Snip(Start),Id,u8"",u8"");
		}
		else if(Cursor[0]=='(') {
			EatToken();
			auto Block0=block_t{};
#if 1 // NEW: Reflects grammar spec change to Base and Qualident to only accept Commas, not List.
			capture_t CommasLeading;
			GRAMMAR_RUN(Scan(CommasLeading));
			if(!StopList.Has(Cursor.Token)) {
				GRAMMAR_SET(Block0,Commas(u8")",prec::Expr,Cursor,CommasLeading,&parser::S81));
				Block0.ElementsTrailing = Block0.BlockTrailing.TrailingCapture;
				Block0.BlockTrailing    = trailing{};
			}
			else Block0.ElementsTrailing = CommasLeading;
			Block0.Punctuation           = punctuation::Parens;
#else // OLD
			GRAMMAR_SET(Block0,List(u8")",&parser::S81,Cursor,capture_t(),punctuation::Parens,Cursor));
#endif
			if(Eat(u8":)"))
				return QualIdentQualified(Target,Start,Block0);
			else if(AllowParenthesis) {
				GRAMMAR_RUN(RequireClose(Start,u8"(",u8")",&parser::S81));
				Block0.BlockSnippet = Snip(Start);
				return Gen.Parenthesis(Block0);
			}
			else return S23(u8":)");
		}
		return S20(What);
	}

	// Macro invocations and constructs that lead with same syntax like '(', '<', 'with'.
	struct call {
		text      CallWhat;
		cursor    CallTrailingStop;
		mode      CallMode;
		block_t&  CallParameter;
		call*     OuterCall = nullptr;  // Initialized to keep static analysis happy
	};
	struct invoke: expr {
		text            What;
		token           StartToken;
		token_set       InTokens, PostTokens;
		call            *FirstCall, *LastCall;
		call*           Of;
		block_t*        Clauses[3];
		block_t*        PriorClause;
		invoke(text What0,expr& OuterExpr0,cursor Start0,token StartToken0,token_set InTokens0,token_set PostTokens0,call* FirstCall0=nullptr,call* LastCall0=nullptr):
			expr(prec::Base,Start0,&OuterExpr0,OuterExpr0.MarkupStart? InvokePostfixes|MarkupPostfixes: InvokePostfixes),
			What(What0), StartToken(StartToken0),
			InTokens(InTokens0), PostTokens(PostTokens0),
			FirstCall(FirstCall0), LastCall(LastCall0),
			Of(nullptr), Clauses{nullptr,nullptr,nullptr}, PriorClause(nullptr) {}
		void UpdateLastCall(call* NewCall) {
			if(LastCall)
				LastCall->OuterCall = NewCall;
			else
				FirstCall           = NewCall;
			LastCall                = NewCall;
		}
		result_t<nothing> OnFinish(parser& Parser) override {
			Parser.CheckToken();
			this->Trailing = trailing{
				LastCall
					? *LastCall->CallParameter.BlockTrailing.TrailingStart
					: PriorClause
						? *PriorClause->BlockTrailing.TrailingStart:
						Parser.Cursor,
				capture_t()};
			GRAMMAR_RUN(expr::OnFinish(Parser));
			if(Clauses[0]) {

				// Generate this macro invocation.
				GRAMMAR_RUN(Parser.UpdateFrom(*this->OuterExpr,PriorClause->BlockTrailing,Parser.Gen.Invoke(
					Parser.SnipFinished(this->Start,*PriorClause),
					Parser.ApplyTrailing(*this->OuterExpr),
					*Clauses[0],Clauses[1],Clauses[2])));

				// Handle remaining calls on the stack now with another Invoke.
				if(!FirstCall) // Disable this to check soundness of logic below.
					return nothing{};
				invoke NewTarget{u8"nested macro invocation",*this->OuterExpr,this->Start,token::None(),
					token_set{u8"do"},token_set{u8"until",u8"catch"},FirstCall,LastCall};
				if(!this->ExprStop)
					return Parser.Invoke(NewTarget,Parser.Cursor,capture_t());
				else
					return NewTarget.OnFinish(Parser);
			}
			else if(!StartToken) {
				// Not a macro, and a macro isn't required, so flush accumulated call and specifiers
				// to the nearest outer prec::Call, needed for if{a}else if{b}<c> associating as (if{a}else if{b})<c>.
				if (!this->OuterExpr) { return nothing{}; }  // This should never occur - though without it some C++ semantic analysis checkers get upset when passing to FinishExpr() below.
				GRAMMAR_LET(InsertCall,Parser.FinishExpr(token::None(),prec::Call,*this->OuterExpr));
				if(!InsertCall)
					return Parser.S61(FirstCall? FirstCall->CallWhat: u8"macro end");
				for(auto Call=FirstCall; Call; Call=Call->OuterCall) {
					Call->CallParameter.BlockSnippet=Snip(point::Start(Call->CallParameter.BlockSnippet),*Call->CallParameter.BlockTrailing.TrailingStart);
					GRAMMAR_RUN(Parser.UpdateFrom(*InsertCall,Call->CallParameter.BlockTrailing,Parser.Gen.Call(
						Snip(InsertCall->Start,point::Stop(Call->CallParameter.BlockSnippet)),Call->CallMode,
						Parser.ApplyTrailing(*InsertCall),Call->CallParameter)));
				}
				return nothing{};
			}
			else return Parser.S76(What); // Error for reserved word not followed by macro.
		}
	};
	result_t<nothing> InvokeClause(invoke& Target,nat WhichClause,cursor BlockStart,block_t& Block0,cursor NextBlockStart,const capture_t& NextTokenLeading=capture_t()) {
		// We've committed to producing a macro invocation, so accumulate specifiers m<a> and handle any prior m(a).catch up to clauses from call m(c).
		auto           Specifiers     = syntaxes_t{};
		const snippet* FirstSpecifier = nullptr;
		while(auto Call=Target.FirstCall) {
			Target.FirstCall=Target.FirstCall->OuterCall;
			ApplyTrailing(Call->CallParameter,Call->CallTrailingStop);
			if(Call->CallMode==mode::Open) {
				GRAMMAR_ASSERT(!Target.Clauses[0] && !Target.Clauses[1] && !Target.Clauses[2]);
				if(FirstSpecifier)
					Call->CallParameter.BlockSnippet = Snip(point::Start(*FirstSpecifier),point::Stop(Call->CallParameter.BlockSnippet));
				Call->CallParameter.Specifiers       = Specifiers;
				Target.Clauses[0]                    = &Call->CallParameter;
				Target.Of                            = nullptr;
				return InvokeClause(Target,WhichClause,BlockStart,Block0,NextBlockStart,NextTokenLeading);
			}
			else if(Call->CallMode==mode::With) {
				if(!Gen.SyntaxesLength(Specifiers))
					FirstSpecifier = &Call->CallParameter.BlockSnippet;
				GRAMMAR_LET(E,Gen.Parenthesis(Call->CallParameter));
				Gen.SyntaxesAppend(Specifiers,E);
			}
			else Err();
		}
		if(Target.PriorClause)
			ApplyTrailing(*Target.PriorClause,FirstSpecifier? point::Start(*FirstSpecifier): BlockStart);
		if(FirstSpecifier)
			Block0.BlockSnippet     = Snip(point::Start(*FirstSpecifier),point::Stop(Block0.BlockSnippet));
		Block0.Specifiers           = Specifiers;
		Target.LastCall             = nullptr; // Catch up so subsequent accumulation works.
		Target.Clauses[WhichClause] = &Block0;
		Target.PriorClause          = Block0.BlockSnippet? &Block0: Target.PriorClause;
		if(!Target.ExprStop)
			return Invoke(Target,NextBlockStart,NextTokenLeading);
		else
			return Target.OnFinish(*this);
	};
	result_t<nothing> Invoke(invoke& Target,cursor BlockStart,capture_t TokenLeading=capture_t()) {
		// Markup  := '<' Scan Tags Scan ":>" Space Ind Contents Ded 
		//         |  '<' Scan Tags Scan ';'  Scan      Contents '>'
		//         |  '<' Scan Tags Scan '>'  Scan      Contents '</' Ident Space {'/' Ident Space} '>'
		// Tags    := Space (!'/' Call ScanKey '.' | !Reserved) QualIdent Space {Invoke} [',' Scan Tags]
		// Postfix := .. | !Invoke (Paren | Specs) | ..
		// Invoke  :=          [Specs] (Paren [Specs] (Block | Do  ) | Block [[Specs] Do  ]) (Until | !Until)
		// If      := "if" Key [Specs] (Paren         (Block | Then) | Block [        Then]) (Else  | !Else )
		GRAMMAR_ASSERT(CheckToken());
		auto PostfixStart = Cursor;
		auto PostfixToken = PostfixStart.Token;
		if(!Target.AllowPostfixes.Has(PostfixToken))
			return Target.OnFinish(*this);

		// Definitely starting a new potential clause.
		switch(nat8(PostfixToken)) {
		case token(u8"("): {
			// Paren := '(' List ')' Space
			EatToken();
			GRAMMAR_LET(Block0,List(u8")",&parser::S82,Cursor,capture_t(),punctuation::Parens,Cursor));
			if(Eat(u8":)")) {
				// If we're in an attribute like @a (b:)c, move the QualIdent to the Base handler for '@'.
				GRAMMAR_LET(InsertExpr,FinishExpr(token::None(),prec::Prefix,Target));
				if(!InsertExpr || !InsertExpr->QualIdentTarget)
					return parser::S82(u8":)");
				InsertExpr->QualIdentTarget->Start=PostfixStart;
				GRAMMAR_LET(Id,QualIdentQualified(*InsertExpr,PostfixStart,Block0));
				GRAMMAR_RUN(UpdateSpaceTrailing(*InsertExpr->QualIdentTarget,Id));
				return nothing{};
			}
			GRAMMAR_RUN(RequireClose(PostfixStart,u8"(",u8")",&parser::S82));
			Block0.BlockSnippet      = Snip(BlockStart);
			auto NewCall             = call{u8"(",Cursor,mode::Open,Block0};
			GRAMMAR_RUN(SpaceTrailing(NewCall.CallParameter.BlockTrailing));
			NewCall.CallTrailingStop = Cursor;
			Target.UpdateLastCall(&NewCall);
			Target.Of                = &NewCall;
			Target.AllowPostfixes    = (Target.AllowPostfixes & ~ParenPostfixes) | Target.InTokens;
			if(Target.StartToken==token(u8"if")) // Disallow if(a)<b>{c} to enable future if(a) b.
				Target.AllowPostfixes = Target.AllowPostfixes & ~WithPostfixes;
			return Invoke(Target,Cursor);
		}
		case token(u8"<"): case token(u8"with"): {
			// Specs := [ScanKey "with" Key] '<' Scan Choice Space '>' Space (Specs | !Specs)
			text CallToken;
			capture_t PunctuationLeading;
			if(PostfixToken==token(u8"with")) {
				EatToken();
				CallToken=u8"with";
				GRAMMAR_RUN(Space(PunctuationLeading));
				if(Cursor.Token!=token(u8"<"))
					return S78();
			}
			EatToken();
			GRAMMAR_LET(Leading,Space());
			// We parse specifier at prec::Choice, but FinishExpr at prec::Less to right-associate nested '<'.
			// LessExpr receives TrailingCapture so specifiers can handle it and less-than can propagate it.
			// If we parsed a<b<c at just prec::Choice, the inner FinishExpr forces the finishes prec::Choice,
			// whose FinishExpr forces the outer prec::Choice, so the outer Postfix incorrectly parses first.
			// This is as simple as it can be; other approaches add bloat.
			bool GotLess    = false;
			auto LessExpr   = when_expr(prec::Less,&Target,AllowLess,Cursor,Leading,[&](expr& LessExpr)->result_t<nothing> {
				// We get here only if we parse a Less expression a<b, not if we parse a specifier.
				GRAMMAR_ASSERT(GotLess);
				auto& InsertExpr = *LessExpr.OuterExpr; // Dynamic, not necessarily Target.
				return UpdateFrom(InsertExpr,LessExpr.Trailing,Gen.InfixToken(
					SnipFinished(InsertExpr.Start,LessExpr),PostfixToken->PostfixMode,
					ApplyTrailing(InsertExpr),PostfixToken->Symbol,*LessExpr
				));
			});
			return WhenExpr(u8"<",prec::Choice,prec::Less,&LessExpr,capture_t(),[&](expr& RightExpr)->result_t<nothing> {
				GRAMMAR_RUN(UpdateFrom(LessExpr,RightExpr.Trailing,*RightExpr));
				if(Eat(u8">")) {
					// Parsed a specifier. Abandon LessExpr.
					auto RightSyntax            = Gen.Leading(Leading,ApplyTrailing(LessExpr));
					auto SpecifierBlock         = SingletonBlock(Snip(BlockStart,Cursor),RightSyntax,PunctuationLeading,punctuation::AngleBrackets);
					SpecifierBlock.Token        = CallToken;
					SpecifierBlock.TokenLeading = TokenLeading;
					auto NewCall                = call{u8"<",Cursor,mode::With,SpecifierBlock};
					GRAMMAR_RUN(SpaceTrailing(NewCall.CallParameter.BlockTrailing));
					NewCall.CallTrailingStop    = Cursor;
					Target.UpdateLastCall(&NewCall);
					return Invoke(Target,Cursor);
				}
				else if(PostfixToken!=token(u8"with")) {
					// We parsed a Less expression a<b so figure out where it lands and finish parsing it.
					GotLess=true;
					GRAMMAR_SET(LessExpr.OuterExpr,FinishExpr(token(u8"<"),prec::Less,Target));
					if(!LessExpr.OuterExpr)
						return S61(u8"<");
					return Postfix(u8"<",prec::Less,LessExpr); // Trigger's LessExpr's when_expr.
				}
				else return S79();
			});
		}
		case token(u8"{"): case token(u8"."): case token(u8":"): case token(u8"in"): {
			// Block := Brace | DotSpace Space Def Space | (DotSpace | ':') Space Ind List Ded
			bool Fails=false;
			GRAMMAR_LET(Block0,Block(u8"macro invocation",Target,BlockStart,TokenLeading,Fails));
			if(!Fails) {
				Target.AllowPostfixes = (Target.AllowPostfixes & ~ParenPostfixes & ~BlockPostfixes) | Target.InTokens | Target.PostTokens;
				if(Target.Of)
					Target.AllowPostfixes = Target.AllowPostfixes & ~Target.InTokens;
				if(Target.StartToken==token(u8"if")) // Disallow if{a}<b>.. so else-if never finishes before last InvokedClause.
					Target.AllowPostfixes = Target.AllowPostfixes & ~WithPostfixes;
				return InvokeClause(Target,Target.Of!=0,BlockStart,Block0,Cursor);
			}
			return Target.OnFinish(*this); // For In, '.' QualIdent.
		}
		case token(u8"do"): case token(u8"then"): {
			// Do   := ScanKey "do"    Key (KeyBlock | Def)
			// Then := ScanKey "then"  Key (KeyBlock | Def)
			EatToken();
			GRAMMAR_LET(PunctuationLeading,Space());
			GRAMMAR_LET(Block0,KeyBlock(prec::Def,Target,BlockStart,TokenLeading,PostfixToken->Symbol,PunctuationLeading));
			Target.AllowPostfixes = (Target.AllowPostfixes & ~Target.InTokens) | Target.PostTokens;
			return InvokeClause(Target,1,BlockStart,Block0,Cursor);
		}
		case token(u8"until"): {
			// Until := ScanKey "until" Key (KeyBlock | Def) | ..
			EatToken();
			GRAMMAR_LET(PunctuationLeading,Space());
			GRAMMAR_LET(Block0,KeyBlock(prec::Def,Target,BlockStart,TokenLeading,PostfixToken->Symbol,PunctuationLeading));
			Target.AllowPostfixes = token_set{};
			return InvokeClause(Target,2,BlockStart,Block0,Cursor);
		}
		case token(u8"catch"): {
			// Until := .. | ScanKey "catch" Key Invoke
			// Chain more catches only if !Target.FirstCall. Update AllowTokens to reenable catch.
			EatToken();
			auto CatchExpr = when_expr(prec::Base,&Target,AllTokens,BlockStart,TokenLeading,[&](expr& CatchExpr)->result_t<nothing> {
				auto Block0=SingletonBlock(BlockStart,CatchExpr);
				return InvokeClause(Target,2,BlockStart,Block0,*CatchExpr.Finished);
			});
			GRAMMAR_RUN(UpdateSpaceTrailing(CatchExpr,Gen.Native(Snip(BlockStart),u8"catch")));
			invoke CatchTarget{u8"catch",CatchExpr,BlockStart,token(u8"catch"),token_set{u8"do"},token_set{u8"until",u8"catch"}};
			GRAMMAR_RUN(Invoke(CatchTarget,Cursor));
			if(!CatchExpr.Finished)
				CatchExpr.OnFinish(*this);
			return nothing();
		}
		case token(u8"else"): {
			// Else := ScanKey "else" Key (ScanKey If | !(ScanKey If) (KeyBlock | Def))
			EatToken();
			GRAMMAR_LET(PunctuationLeading,Space());
			Target.AllowPostfixes = token_set{};
			if(Cursor.Token==token(u8"if")) {
				// Grammar makes "else if" a special case so "if(a){b}else if(c){d}+1"
				// is equivalent to "(if(a){b}else if(c){d})+1", not "if(a){b}else (if(c){d}+1)".
				return WhenExpr(u8"else if",prec::Base,prec::Base,&Target,PunctuationLeading,[&](expr& ElseExpr)->result_t<nothing> {
					Target.ExprStop         = ElseExpr.ExprStop;
					auto ElseBlock          = SingletonBlock(BlockStart,ElseExpr);
					ElseBlock.Token         = PostfixToken->Symbol;
					ElseBlock.TokenLeading  = TokenLeading;
					return InvokeClause(Target,2,BlockStart,ElseBlock,Cursor);
				});
			}
			else {
				GRAMMAR_LET(ElseBlock,KeyBlock(prec::Def,Target,BlockStart,TokenLeading,PostfixToken->Symbol,PunctuationLeading));
				return InvokeClause(Target,2,BlockStart,ElseBlock,Cursor);
			}
		}
		case token(u8","): case token(u8";"): case token(u8">"): case token(u8":>"): {
			if(!Target.Clauses[0] || Target.FirstCall) {
				// If we have <m;c>, <m(a);c>, <m<a>;c>, <m{a}<b>;c>, introduce a new block and recurse back.
				block_t Block0{Snip()};
				return InvokeClause(Target,Target.FirstCall!=nullptr,BlockStart,Block0,BlockStart,TokenLeading);
			}
			EatToken();
			if(!Target.OuterExpr->MarkupTag)
				return S40();
			if(Target.PriorClause)
				ApplyTrailing(*Target.PriorClause,BlockStart); // TODO: Fix, as this is bad for <m(a)\n ;a>.
			Target.OuterExpr->MarkupFinished=true;
			capture_t PreContent,PostContent;
			switch(nat8(PostfixToken)) {
			case token(u8","): {
				GRAMMAR_LET(InnerContent,MarkupExpr(Target.OuterExpr,PostfixStart));
				return InvokeMarkup(Target,TokenLeading,capture_t(),InnerContent,capture_t());
			}
			case token(u8";"): {
				Gen.MarkupStart(PreContent,Snip(PostfixStart));
				GRAMMAR_LET(Content,Trimmed(false));
				cursor ContentsEnd=Cursor;
				GRAMMAR_RUN(Require(u8">",&parser::S51));
				Gen.MarkupStop(PostContent,Snip(ContentsEnd));
				return InvokeMarkup(Target,TokenLeading,PreContent,Content,PostContent);
			}
			case token(u8">"): {
				Gen.MarkupStart(PreContent,Snip(PostfixStart));
				GRAMMAR_LET(Content,Trimmed(true));
				cursor PostStart=Cursor;
				GRAMMAR_RUN(Require(u8"<",&parser::S52));
				Gen.MarkupStart(PostContent,Snip(PostStart));
				for(auto* ExpectMarkup=Target.OuterExpr; ExpectMarkup; ExpectMarkup=ExpectMarkup->OuterMarkup) {
					GRAMMAR_RUN(Require(u8"/",&parser::S44));
					if(!IsAlpha(Cursor[0]))
						return S44(ExpectMarkup->MarkupTag);
					auto TagStart=Cursor;
					GRAMMAR_LET(EndTag,Ident());
					if(EndTag!=ExpectMarkup->MarkupTag)
						return S43(ExpectMarkup->MarkupTag,EndTag);
					auto TagSnippet=Snip(TagStart);
					Gen.MarkupTag(PostContent,TagSnippet);
					GRAMMAR_RUN(Space(PostContent));
				}
				cursor PostEnd=Cursor;
				GRAMMAR_RUN(Require(u8">",&parser::S44));
				Gen.MarkupStop(PostContent,Snip(PostEnd));
				return InvokeMarkup(Target,TokenLeading,PreContent,Content,PostContent);
			}
			case token(u8":>"): {
				Gen.MarkupStart(PreContent,Snip(PostfixStart));
				GRAMMAR_RUN(Space(PreContent));
				if(!Ending())
					return S46();
				GRAMMAR_LET(SavedContext,Ind());
				GRAMMAR_LET(Content,Contents(true));
				GRAMMAR_RUN(Ded(SavedContext,&parser::S54));
				GRAMMAR_RUN(Space(PostContent));
				return InvokeMarkup(Target,TokenLeading,PreContent,Content,PostContent);
			}
			default: {
				break;
			}}
			[[fallthrough]];
		}
		case token::NewLine(): {
			GRAMMAR_LET(ScanToken,ScanKey(TokenLeading,
				Target.AllowPostfixes&token_set{u8"catch",u8"do",u8"else",u8"then",u8"until",u8"with",u8"{",u8">",u8":>",u8",",u8";"}));
			if(ScanToken)
				return Invoke(Target,BlockStart,TokenLeading);
			return Target.OnFinish(*this);
		}
		default:  // Ensure static analysis happy with all permutations covered
			break;
		}
		Err(); // AllowPostfixes makes this unreachable.
	}

	// Markup.
	result_t<nothing> InvokeMarkup(invoke& InvokeTarget,const capture_t& TokenLeading,const capture_t& PreContent,syntax_t& Content,const capture_t& PostContent) {
		auto& MarkupExpr = *InvokeTarget.OuterExpr;
		auto  NoTrailing = trailing{Cursor, capture_t()};
		GRAMMAR_RUN(UpdateFrom(MarkupExpr,NoTrailing,Gen.InvokeMarkup(
			Snip(*MarkupExpr.MarkupStart),
			!MarkupExpr.OuterMarkup? u8"<": u8",",
			MarkupExpr.ExprLeading,
			ApplyTrailing(MarkupExpr),
			InvokeTarget.Clauses[0],InvokeTarget.Clauses[1],
			TokenLeading,
			PreContent,Content,PostContent)
		));
		MarkupExpr.ExprLeading = capture_t();
		return MarkupExpr.OnFinish(*this);
	}
	result_t<syntax_t> Markup() {
		GRAMMAR_ASSERT(Cursor[0]=='<');
		auto Start=Cursor;
		Next(1);
		return MarkupExpr(nullptr,Start);
	}

	// Expressions.
	struct ins {cursor Start; token InToken; cursor NextStart; capture_t NextLeading; const ins* NextIns;};
	result_t<nothing> InChoice(expr& PostfixExpr,cursor Start,const ins* Ins=nullptr) {
		// In := ("in" Key | ':') Space (In | NotEq)

		scoped_guard ExprDepthGuard(ExprDepth, ExprDepth + 1);
		if (ExprDepth > VERSE_MAX_EXPR_DEPTH)
			return S99();

		// Here, we parse the Choice into PostfixExpr without finishing it.
		auto InToken = Cursor.Token;
		if(InPrefixes.Has(Cursor.Token)) {
			EatToken();
			auto NextStart=Cursor;
			GRAMMAR_LET(NextLeading,Space());
			auto NextIn=ins{Start,InToken,NextStart,NextLeading,Ins};
			return InChoice(PostfixExpr,Cursor,&NextIn);
		}
		GRAMMAR_RUN(WhenExpr(InToken->Symbol,prec::Choice,prec::Choice,&PostfixExpr,capture_t(),[&](expr& Right)->result_t<nothing> {
			auto NewRight=*Right;
			for(; Ins; Ins=Ins->NextIns) {
				auto RightBlock=SingletonBlock(SnipFinished(Ins->NextStart,Right),Gen.Leading(Ins->NextLeading,NewRight));
				GRAMMAR_SET(NewRight,Gen.PrefixToken(
					SnipFinished(Ins->Start,Right),Ins->InToken->PrefixMode,Ins->InToken->Symbol,
					RightBlock,false));
			}
			return UpdateFrom(PostfixExpr,Right.Trailing,NewRight);
		}));
		return nothing{};
	}
	result_t<nothing> DefOrSetPostfix(expr& Target,token_set Postfixes) {
		auto DefineToken=Cursor.Token;
		if(Postfixes.Has(DefineToken)) {
			EatToken();
			GRAMMAR_LET(Right,BraceInd(DefineToken->Symbol,prec::Def,Target));
			GRAMMAR_RUN(UpdateFrom(Target,Right.BlockTrailing,Gen.InfixBlock(
				SnipFinished(Target.Start,Right),
				ApplyTrailing(Target),DefineToken->Symbol,Right)));
		}
		return nothing{};
	}
	result_t<nothing> Base(text What,prec Prec,expr& Target,error_t(parser::*OnTokenError)(text),error_t(parser::*OnPrecError)(text,text)) {
		// Base := '(' List ')' | Num | Char | Path | String | Markup | If | !Reserved QualIdent
		GRAMMAR_ASSERT(CheckToken());
		auto BaseToken = Cursor.Token;
		if(Prec<=BaseToken->PrefixPrec) {
			switch(nat8(Cursor.Token)) {
			case token::Digit(): {
				if(Cursor[0]=='0' && Cursor[1]=='o' && IsHex(Cursor[2])) {
					GRAMMAR_LET(c,Char8());
					return UpdateSpaceTrailing(Target,Gen.Char8(Snip(Target.Start),c));
				}
				else if(Cursor[0]=='0' && Cursor[1]=='u' && IsHex(Cursor[2])) {
					GRAMMAR_LET(c,Char32());
					return UpdateSpaceTrailing(Target,Gen.Char32(Snip(Target.Start),c,true,false));
				}
				else return UpdateSpaceTrailing(Target,Num());
			}
			case token(u8"\""): {
				// String := '"' {Interp | CharEsc | !('\'|'{'|'}'|'"') Text} '"'
				Next(1);
				GRAMMAR_LET(Capture,String<place::String>(Cursor));
				GRAMMAR_RUN(Require(u8"\"",&parser::S32));
				return UpdateSpaceTrailing(Target,Gen.String(Snip(Target.Start),Capture));
			}
			case token(u8"'"): {
				// CharLit := ''' Printable ''' !''' | ''' CharEsc '''
				return UpdateSpaceTrailing(Target,CharLit());
			}
			case token::Alpha():                    // Ident.
			case token(u8"("):                      // QualIdent or Paren.
			case token(u8"at"): case token(u8"of"): // Infix operator tokens that are allowed as identifiers.
			case token(u8"to"):
			case token(u8"next"): case token(u8"over"): case token(u8"when"): case token(u8"where"): case token(u8"while"):
			case token(u8"and"): case token(u8"or"): {
				// Ident     := Alpha {Alnum} !Alnum ["'" {!('<#'|'#>'|'\'|'{'|'}'|'"'|''') 0o20-0o7E} "'"]
				// QualIdent := ['(' List ':)' Space] Ident
				// Base      := '(' List ')' | ..
				// Postfix-only non-ScanKey keywords are valid identifiers.
				return UpdateSpaceTrailing(Target,QualIdent(What,Target,true));
			}
			case token(u8"@"): {
				// Expr := .. | '@' Space Call Scan &('@'|QualIdent) Expr
				EatToken();
				result<syntax_t,nothing> AttrSyntax;

				// Set up for parsing RightExpr later. It may receive QualIdent early, e.g. in @a (b:)c...
				auto RightExpr = when_expr(prec::Expr,&Target,AllTokens,Cursor,capture_t(),[&](expr& RightExpr) {
					return UpdateFrom(Target,RightExpr.Trailing,Gen.PrefixAttribute(
						SnipFinished(Target.Start,RightExpr),
						*AttrSyntax,*RightExpr));
				});

				// Parse attribute, with RightExpr as its QualIdent target for @a (b:)c.
				GRAMMAR_LET(AttrLeading,Space());
				GRAMMAR_RUN(WhenExpr(u8"@",prec::Call,prec::Prefix,nullptr,AttrLeading,[&](expr& AttrExpr)->result_t<nothing> {
					ApplyTrailing(AttrExpr,true);
					AttrSyntax=*AttrExpr;
					return nothing{};
				},AllTokens,&parser::S71,&RightExpr));

				// Parse all or the remainder of RightExpr.
				if(!RightExpr.ExprSyntax) {
					GRAMMAR_RUN(Scan(RightExpr.ExprLeading));
					RightExpr.Start=Cursor;
					if(Cursor[0]!='@' && Cursor[0]!='(' && !IsAlnum(Cursor[0]))
						return S67();
					GRAMMAR_RUN(Base(What,Prec,RightExpr,&parser::S71,&parser::S60));
				}
				GRAMMAR_RUN(Postfix(What,Prec,RightExpr,&parser::S71,&parser::S60));
				return RightExpr.Result;
			}
			case token(u8"<"): {
				// Base = .. | Markup | ..
				return UpdateSpaceTrailing(Target,Markup());
			}
			case token(u8"/"): {
				// Base = (.. | Path | ..) Space
				GRAMMAR_LET(P,Path());
				return UpdateSpaceTrailing(Target,Gen.Path(Snip(Target.Start),P));
			}
			case token(u8":"): case token(u8"in"): {
                // In  := ("in" Key | ':') Space (In | NotEq)
				// Def := (Or | (In|Var) Is) {In Is | ..}

				// Postfix x:t leads here, so capture any :t<v, keeping x:t definition at the top.
				auto PostfixExpr = when_expr(prec::Def,&Target,AllTokens,Cursor,capture_t(),[&](expr& PostfixExpr)->result_t<nothing> {
					// This runs when InChoice or Postfix below finishes.
					GRAMMAR_RUN(UpdateFrom(Target,PostfixExpr.Trailing,*PostfixExpr));
					return DefOrSetPostfix(Target,DefPostfixes);
				},nullptr);

				// InChoice parses Choice into PostfixExpr, then Postfix extends to NotEq.
				GRAMMAR_RUN(InChoice(PostfixExpr,Target.Start));
				GRAMMAR_RUN(Postfix(BaseToken->Symbol,prec::NotEq,PostfixExpr));
				return nothing{};
			}
			case token(u8"var"): case token(u8"set"): case token(u8"ref"): case token(u8"alias"): {
				// Var := (("var" [Space '<' Space Choice Space '>'])|"set"|"ref"|"alias") Key Space Choice
				// Def := (Or | (In|Var) Space (('='|':='|'+='|'*='|'/=') Space (BraceInd | Def) | !'=' !':=')) {&In Def | ..}
				bool bIsVar = Cursor.Token == token(u8"var");
				EatToken();
				syntaxes_t Attributes;
				if (bIsVar)
				{
#if !TIM /* This syntax will evolve from var<specifier> x:t=v to x:t<writes><specifier>=v. */
					while (true)
					{
						GRAMMAR_LET(StartingSpace,Space());
						if (Cursor.Token != token(u8"<"))
						{
							break;
						}

						EatToken();
						GRAMMAR_LET(MiddleSpace,Space());
						GRAMMAR_RUN(WhenExpr(u8"<",prec::Choice,prec::Less,&Target,StartingSpace,[&](expr& Expr)->result_t<nothing> {
							ApplyTrailing(Expr,true);
							Gen.SyntaxesAppend(Attributes,*Expr);
							return nothing{};
						}));
						GRAMMAR_LET(EndingSpace,Space());
						GRAMMAR_RUN(RequireClose(Cursor,u8"<",u8">",&parser::S85));
					}
#endif
				}
				auto ChoiceStart=Cursor;
				GRAMMAR_LET(Middle,Space());
				return WhenExpr(BaseToken->Symbol,prec::Choice,prec::Choice,&Target,Middle,[&](expr& Choice)->result_t<nothing> {
					auto ChoiceBlock=SingletonBlock(ChoiceStart,Choice);
					GRAMMAR_RUN(UpdateFrom(Target,ChoiceBlock.BlockTrailing,Gen.PrefixToken(
						SnipFinished(Target.Start,Choice),
						BaseToken->PrefixMode,BaseToken->Symbol,
						ChoiceBlock,false,Attributes)));
					if(SetPostfixes.Has(Cursor.Token)) // Translate "set x=3" to "set{x}:=3".
						return DefOrSetPostfix(Target,SetPostfixes);
					return nothing{};
				});
			}
			case token(u8".."): case token(u8"not"): /*case token("!"):*/ {
				// Not  := .. | ("not" Key) Space Not
				// Def  := .. | (.. | '..') Space Def
				EatToken();
				auto RightStart=Cursor;
				GRAMMAR_LET(Middle,Space());
				return WhenExpr(BaseToken->Symbol,BaseToken->PrefixPrec,BaseToken->PrefixPrec,&Target,Middle,[&](expr& RightExpr) {
					auto RightBlock=SingletonBlock(RightStart,RightExpr);
					return UpdateFrom(Target,RightBlock.BlockTrailing,Gen.PrefixToken(
						SnipFinished(Target.Start,RightExpr),
						BaseToken->PrefixMode,BaseToken->Symbol,
						RightBlock,false));
				});
			}
			case token(u8"&"): {
				// Def := .. | ('&' | ..) Space Def
				EatToken();
				GRAMMAR_LET(Middle,Space());
				return WhenExpr(u8"&",BaseToken->PrefixPrec,BaseToken->PrefixPrec,&Target,Middle,[&](expr& Right) {
					return UpdateFrom(Target,Right.Trailing,Gen.Escape(
						SnipFinished(Target.Start,Right),*Right));
				});
			}
			case token(u8"^"): case token(u8"?"): case token(u8"+"): case token(u8"-"): case token(u8"*"): {
				// Prefix := .. | ('^' | '?' | .. | '+' | '-' | '*') Space (Brace | Prefix)
				EatToken();
				return WhenBraceCall(BaseToken->Symbol,BaseToken->PrefixPrec,Target,[&](block_t& RightBlock)->result_t<nothing> {
					return UpdateFrom(Target,RightBlock.BlockTrailing,Gen.PrefixToken(
						SnipFinished(Target.Start,RightBlock),
						BaseToken->PrefixMode,BaseToken->Symbol,
						RightBlock,RightBlock.Punctuation==punctuation::Braces));
				});
			}
			case token(u8"["): {
				// Prefix := .. | (.. | '[' List ']' | ..) Space (Brace | Prefix)
				EatToken();
				GRAMMAR_LET(Left,List(u8"]",&parser::S85,Cursor,capture_t(),punctuation::None,Cursor));
				GRAMMAR_RUN(RequireClose(Target.Start,u8"[",u8"]",&parser::S85));
				return WhenBraceCall(u8"[]",BaseToken->PrefixPrec,Target,[&](block_t& Right)->result_t<nothing> {
					return UpdateFrom(Target,Right.BlockTrailing,Gen.PrefixBrackets(
						SnipFinished(Target.Start,Right),
						Left,Right));
				});
			}
			case token(u8"if"): {
				// If := "if" Key [Specs] (Paren (Block | Then) | Block [Then]) (Else | !Else)
				Target.MarkupTag=u8"if"; // We propagate markup to support <if(a)>Hello</if>.
				EatToken();
				GRAMMAR_RUN(UpdateSpaceTrailing(Target,Gen.Native(Snip(Target.Start),u8"if")));
				invoke IfTarget{u8"if",Target,Target.Start,token(u8"if"),token_set{u8"then"},token_set{u8"else"}};
				return Invoke(IfTarget,Cursor);
			}
			case token(u8"return"): case token(u8"yield"): case token(u8"break"): case token(u8"continue"): {
				// Def := .. | Return [KeyBlock|Def] StopDef
				EatToken();
				block_t Right;
				Right.BlockTrailing.TrailingStart=Cursor;
				GRAMMAR_SET(Right.BlockTrailing.TrailingCapture,Space());
				if(!StopDef.Has(Cursor.Token))
					GRAMMAR_SET(Right,KeyBlock(prec::Def,Target,*Right.BlockTrailing.TrailingStart,capture_t(),u8"",Right.BlockTrailing.TrailingCapture));
				return UpdateFrom(Target,Right.BlockTrailing,Gen.PrefixToken(
					SnipFinished(Target.Start,Right),
					BaseToken->PrefixMode,BaseToken->Symbol,
					Right,false));
			}
			case token(u8"!"): 
				return S62();
			default: {  // Static analysis warnings without default
				break;
			}}
			Err(); // Should never occur due to structure of the precedence table.
		}
		if(BaseToken->PrefixPrec==prec::Never)
			return (this->*OnTokenError)(What);
		else
			return (this->*OnPrecError)(What,BaseToken->Symbol);
	}
	result_t<nothing> Postfix(text /*What*/,prec Prec,expr& Target,error_t(parser::*/*OnTokenError*/)(text)=&parser::S71,error_t(parser::*/*OnPrecError*/)(text,text)=&parser::S60) {
		while(!Target.Finished) {
			auto PostfixStart = Cursor;
			auto TokenLeading = capture_t();
			auto PostfixToken = Cursor.Token;
		token_leading_loop:
			GRAMMAR_ASSERT(CheckToken());
			if(!(Prec<=PostfixToken->PostfixTokenPrec || (Target.MarkupStart && MarkupPostfixes.Has(PostfixToken)))) {
			finished_postfix:
				Cursor=PostfixStart; //backtrack NewLine's ScanToken
				return Target.OnFinish(*this);
			}
			if(!Target.AllowPostfixes.Has(Cursor.Token)) // Immediate error disallowing e.g. a<=b>c per grammar.
				return S61(PostfixToken->Symbol);
			switch(nat8(Cursor.Token)) {
			case token(u8"&"):
				if(Cursor[1]=='&') 
					return S62();
				goto binary_operator;
			case token(u8"|"):
				if(Cursor[1]=='|') 
					return S62();
				goto binary_operator;
			case token(u8">"):
				if(Target.MarkupStart)
					goto markup_postfix;
				goto binary_operator; // Else continue on as greater operator.
			binary_operator:
			case token(u8"*"):  case token(u8"/"):
			case token(u8"+"):  case token(u8"-"):  
			case token(u8"to"): case token(u8".."): case token(u8"->"):
			case token(u8">="): 
			case token(u8"<="):
			case token(u8"<>"):
			case token(u8"="):
			case token(u8"and"): /*case token(u8"&&"):*/
			case token(u8"or"):  /*case token(u8"||"):*/ {
				// Mul     := Prefix  { Space ('*' | '/' | '&'       ) Scan  Prefix  }
				// Add     := Mul     { Space ('+' | '-'             ) Scan  Mul     }
				// To      := Add     [ Space ("to" Key | ".." | "->") Scan  To      ]
				// Choice  := To      [ Space ('|'                   ) Scan  Choice  ]
				// Greater := Choice  [ Space ('>'  | ">="           ) Scan  Greater ]
				// Less    := Greater [ Space ('<'  | "<="           ) Scan  &(Choice Space !'>' !'>=') Less]
				// NotEq   := Less    { Space ('<>'                  ) Scan  Choice  }
				// Eq      := NotEq   { Space ('='                   ) Scan  NotEq   }
				// And     := Not     { Space ("and" Key             ) Scan  And     }
				// Or      := And     { Space ("or"  Key             ) Scan  Or      }
				Target.MarkupTag=u8"";
				EatToken();
				capture_t Leading;
				GRAMMAR_RUN(Scan(Leading));
				GRAMMAR_RUN(WhenExpr(PostfixToken->Symbol,PostfixToken->PostfixRightPrec(),PostfixToken->PostfixRightPrec(),&Target,Leading,[&](expr& Right)->result_t<nothing> {
					return UpdateFrom(Target,Right.Trailing,Gen.InfixToken(
						SnipFinished(Target.Start,Right),PostfixToken->PostfixMode,
						ApplyTrailing(Target),PostfixToken->Symbol,*Right
					));
				},PostfixToken->PostfixAllowMask));
				continue;
			}
			case token(u8"^"): case token(u8"?"): case token(u8"ref"): {
				// Call    := Base {Space Postfix}
				// Postfix := .. | ('^' | '?' | "ref" | ..)
				Target.MarkupTag=u8"";
				EatToken();
				GRAMMAR_RUN(UpdateSpaceTrailing(Target,Gen.PostfixToken(
					Snip(Target.Start),PostfixToken->PostfixMode,
					ApplyTrailing(Target),PostfixToken->Symbol)));
				continue;
			}
			case token(u8"["): {
				// Call    := Base {Space Postfix}
				// Postfix := .. | (.. | '[' List ']' ..)
				Target.MarkupTag=u8"";
				EatToken();
				GRAMMAR_LET(Block0,List(u8"]",&parser::S83,Cursor,capture_t(),punctuation::Brackets,Cursor));
				GRAMMAR_RUN(RequireClose(Target.Start,u8"[",u8"]",&parser::S83));
				Block0.BlockSnippet=Snip(PostfixStart);
				GRAMMAR_RUN(UpdateSpaceTrailing(Target,Gen.Call(
					Snip(Target.Start),mode::Closed,
					ApplyTrailing(Target),Block0)));
				continue;
			}
			case token(u8"@"): {
				// Expr := Fun {'@' Space Call} StopExpr | ..
				EatToken();
				GRAMMAR_LET(Leading,Space());
				GRAMMAR_RUN(WhenExpr(PostfixToken->Symbol,prec::Call,prec::Call,&Target,Leading,[&](expr& Right)->result_t<nothing> {
					return UpdateFrom(Target,Right.Trailing,Gen.PostfixAttribute(
						SnipFinished(Target.Start,Right),
						ApplyTrailing(Target),*Right));
				}));
				continue;
			}
			case token(u8"at"): case token(u8"of"): {
				// Postfix := .. | ("at"|"of") Key (KeyBlock | Fun)
				Target.MarkupTag=u8"";
				EatToken();
				GRAMMAR_LET(PunctuationLeading,Space());
				GRAMMAR_LET(Right,KeyBlock(prec::Fun,Target,PostfixStart,capture_t(),PostfixToken->Symbol,PunctuationLeading));
				GRAMMAR_RUN(UpdateFrom(Target,Right.BlockTrailing,Gen.Call(
					SnipFinished(Target.Start,Right),PostfixToken->PostfixMode,
					ApplyTrailing(Target),Right)));
				continue;
			}
			case token(u8"=>"): case token(u8":="): case token(u8"next"): {
				// Def  := .. { .. | Space ":=" Space (BraceInd | Def) | ..} | ..
				// Fun  := Def {Space ("=>" Space | "next" Key) (BraceInd | Fun) } StopFun
				EatToken();
				GRAMMAR_LET(Right,BraceInd(PostfixToken->Symbol,PostfixToken->PostfixRightPrec(),Target));
				GRAMMAR_RUN(UpdateFrom(Target,Right.BlockTrailing,Gen.InfixBlock(
					SnipFinished(Target.Start,Right),
					ApplyTrailing(Target),PostfixToken->Symbol,Right)));
				continue;
			}
			case token(u8"."): {
				// Postfix := .. | (.. | ScanKey '.' QualIdent)
				if(!IsSpace(Cursor[1]) /*&& !IsEnding(Cursor[1])*/) {
					Target.MarkupTag=u8"";
					EatToken();
					Gen.CaptureAppend(Target.Trailing.TrailingCapture,TokenLeading);
					GRAMMAR_LET(Id,QualIdent(u8".",Target,false));
					GRAMMAR_RUN(UpdateSpaceTrailing(Target,Gen.InfixToken(
						Snip(Target.Start),PostfixToken->PostfixMode,
						ApplyTrailing(Target),PostfixToken->Symbol,Id)));
					continue;
				}
				[[fallthrough]]; // Else it's a macro invocation handled below.
			}
			case token(u8"{"): case token(u8":"): case token(u8"<"): case token(u8"("):
			case token(u8"in"): case token(u8"with"): 
			case token(u8":>"): case token(u8";"): case token(u8","): markup_postfix: {
				// Invoke := [Specs] (Paren [Specs] (Block | Do) | Block [[Specs] Do]) (Until | !Until)
				// Def    := (Or | ..) {&In Def | ..}
				invoke InvokeTarget{u8"macro invocation",Target,Target.Start,token::None(),token_set{u8"do"},token_set{u8"until",u8"catch"}};
				GRAMMAR_RUN(Invoke(InvokeTarget,PostfixStart,TokenLeading));
				if(Cursor.Pos==PostfixStart.Pos && InPrefixes.Has(PostfixToken)) {
					if(Prec>prec::Def)
						goto finished_postfix;
					// Parse Def and generate a tokenless definition of Target.
					GRAMMAR_RUN(WhenExpr(PostfixToken->Symbol,prec::Def,prec::Def,&Target,capture_t(),[&](expr& InExpr) {
						auto InBlock=SingletonBlock(InExpr.Start,InExpr);
						return UpdateFrom(Target,InBlock.BlockTrailing,Gen.InfixBlock(
							SnipFinished(Target.Start,InExpr),
							ApplyTrailing(Target),u8"",InBlock));
					}));
				}
				continue;
			}
			case token(u8"is"): {
				// Def := .. (.. | ScanKey "is" Key (KeyBlock | Def) | ..)
				EatToken();
				GRAMMAR_LET(PunctuationLeading,Space());
				GRAMMAR_LET(Right,KeyBlock(prec::Def,Target,PostfixStart,TokenLeading,u8"is",PunctuationLeading));
				GRAMMAR_RUN(UpdateFrom(Target,Right.BlockTrailing,Gen.InfixBlock(
					SnipFinished(Target.Start,Right),
					ApplyTrailing(Target),u8"is",Right)));
				continue;
			}
			case token(u8"over"): case token(u8"when"): case token(u8"where"): case token(u8"while"): {
				// Def := .. (.. | Space "where" Key (KeyBlock | Defs) | ..)
				// Fun := Def { Space ('over' | 'upon' | 'while') Key (KeyBlock | Defs) | ..) StopFun
				EatToken();
				GRAMMAR_LET(Right,KeyBlockDefs(Target,PostfixStart,TokenLeading,PostfixToken->Symbol));
				GRAMMAR_RUN(UpdateFrom(Target,Right.BlockTrailing,Gen.InfixBlock(
					SnipFinished(Target.Start,Right),
					ApplyTrailing(Target),PostfixToken->Symbol,Right)));
				continue;
			}
			case token::NewLine():
				GRAMMAR_SET(PostfixToken,ScanKey(TokenLeading,token_set{u8"is",u8"with",u8"{",u8">",u8":>",u8".",u8",",u8";"}));
				goto token_leading_loop;
			case token(u8"=="):
				return S65();
			case token(u8"+="): case token(u8"-="): case token(u8"*="): case token(u8"/="):
				return S66(PostfixToken->Symbol);
			default:
				Err(); // Should be unreachable due to precedence.
			}
		}
		return nothing{};
	}
	result_t<expr*> FinishExpr(token Token,prec FinishPrec,expr& SourceExpr) {
		// Preemptively finish and generate syntax for all expressions tighter than FinishPrec,
		// producing an error if there is no expression at or looser than FinishPrec.
		GRAMMAR_ASSERT(FinishPrec>=prec::Def); // Vital because prec.Def and looser don't handle preemptive finish.
		for(auto Expr=&SourceExpr; Expr; Expr=Expr->OuterExpr) {
			if(Expr->FinishPrec<=FinishPrec)
				if(Token==token::None() || Expr->AllowPostfixes.Has(Token))
					return Expr;
			Expr->ExprStop=true;
			if(!Expr->Finished)
				GRAMMAR_RUN(Expr->OnFinish(*this));
		}
		return nullptr;
	}
	template<class f> struct when_expr: expr {
		using       result_type = decltype((*(f*)nullptr)(*(expr*)nullptr));
		f           F;
		result_type Result;
		when_expr(prec FinishPrec0,expr* OuterExpr0,token_set PostfixAllow0,const cursor& Start0,
			const capture_t& ExprLeading0,const f& F0,expr* QualIdentTarget0=nullptr):
			expr(FinishPrec0,Start0,OuterExpr0,PostfixAllow0,QualIdentTarget0), F(F0) {
			this->ExprLeading=ExprLeading0;
		}
		result_t<nothing> WhenExprParse(parser& Parser,text What,prec ParsePrec,error_t(parser::*OnTokenError)(text),error_t(parser::*OnPrecError)(text,text)) {
			scoped_guard ExprDepthGuard(Parser.ExprDepth, Parser.ExprDepth + 1);
			if (Parser.ExprDepth > VERSE_MAX_EXPR_DEPTH)
				return Parser.S99();
			GRAMMAR_RUN(Parser.Base(What,ParsePrec,*this,OnTokenError,OnPrecError));
			GRAMMAR_RUN(Parser.Postfix(What,ParsePrec,*this,OnTokenError,OnPrecError));
			GRAMMAR_ASSERT(this->Finished);
			return nothing{};
		}
		result_t<nothing> OnFinish(parser& Parser) override {
			GRAMMAR_RUN(expr::OnFinish(Parser));
			this->ExprSyntax = Parser.Gen.Leading(this->ExprLeading,**this);
			GRAMMAR_SET(Result,F(*this));
			GRAMMAR_ASSERT(!this->Trailing);
			return nothing{};
		}
	};
	template<class f> when_expr(prec,expr*,token_set,const cursor&,const capture_t&,const f&,expr* e=nullptr,bool b=false)->when_expr<f>;
	template<class f> typename when_expr<f>::result_type WhenExpr(text What,prec ParsePrec,prec FinishPrec,expr* OuterExpr,
		const capture_t& ExprLeading,const f& F,
		token_set AllowPostfixes=AllTokens,error_t(parser::*OnTokenError)(text)=&parser::S71,expr* QualIdentTarget0=nullptr) {
		// Start parsing an expression which may be finished preemptively by FinishExpr.
		auto Target = when_expr(FinishPrec,OuterExpr,AllowPostfixes,Cursor,ExprLeading,F,QualIdentTarget0);
		GRAMMAR_RUN(Target.WhenExprParse(*this,What,ParsePrec,OnTokenError,&parser::S60));
		return Target.Result;
	}
	result_t<syntax_t> MarkupExpr(expr* OuterMarkup,cursor MarkupStart) {
		capture_t Leading;
		GRAMMAR_RUN(Scan(Leading));
		if(Cursor[0]=='/')
			return S42();
		auto Expr = when_expr(prec::Call,nullptr,AllTokens,Cursor,Leading,[&](expr& Expr)->result_t<nothing> {
			Expr.Trailing = trailing{};
			return nothing{};
		});
		Expr.MarkupStart = MarkupStart;
		Expr.OuterMarkup = OuterMarkup;
		GRAMMAR_RUN(Expr.WhenExprParse(*this,u8"markup",prec::Call,&parser::S74,&parser::S64));
		if(!Expr.MarkupFinished)
			return S41();
		return *Expr;
	}

	// Separated expressions.
	result_t<block_t> Commas(text What,prec Prec,cursor Start,capture_t& Leading,error_t(parser::*OnTokenError)(text)) {
		// Commas := Expr {',' Scan Expr}
		block_t Block0;
		for(;;) {
			bool More=false;
			GRAMMAR_RUN(WhenExpr(What,Prec,Prec,nullptr,Leading,[&](expr& Expr)->result_t<nothing> {
				More=Eat(u8",");
				if(More)
					ApplyTrailing(Expr,true);
				else
					Block0.BlockSnippet=Snip(Start,*Expr.Trailing.TrailingStart),
					Block0.BlockTrailing.MoveFrom(Expr.Trailing);
				Leading=capture_t();
				Gen.SyntaxesAppend(Block0.Elements,*Expr);
				return nothing{};
			},AllTokens,OnTokenError));
			if(!More)
				return Block0;
			Block0.Form=form::Commas;
			GRAMMAR_RUN(Scan(Leading));
		}
	}
	result_t<block_t> List(text What,error_t(parser::*OnTokenError)(text),cursor BlockStart,const capture_t& PunctuationLeading,punctuation Punctuation,cursor CommasStart,const capture_t& Leading=capture_t()) {
		// Separator := (';'|Ending) Scan
		// List      := push; set LinePrefix=""; Scan [Commas {Separator Commas} [Separator]]; pop
		auto SavedContext            = Context;
		bool Some                    = false;
		Context.LinePrefix           = false;
		auto ListBlock               = block_t{};
		ListBlock.Form               = form::List;
		ListBlock.PunctuationLeading = PunctuationLeading;
		ListBlock.Punctuation        = Punctuation;
		ListBlock.ElementsTrailing   = Leading;
		GRAMMAR_RUN(Scan(ListBlock.ElementsTrailing));
		if(!StopList.Has(Cursor.Token))
			for(;;) {
				GRAMMAR_LET(CommasBlock,Commas(What,prec::Expr,CommasStart,ListBlock.ElementsTrailing,OnTokenError));
				ApplyTrailing(CommasBlock,Cursor);
				CommasBlock.BlockSnippet = Snip(CommasStart);
				bool More                = false;
				if(Cursor.Token==token(u8";") || Ending()) {

					// Attribute Commas-trailing [';'] Space &NewLine to CommasBlock, following Scan to ListBlock.
					auto SemicolonStart = Cursor;
					if(Eat(u8";"))
						Gen.Semicolon(CommasBlock.ElementsTrailing,Snip(SemicolonStart));
					GRAMMAR_LET(SemicolonTrailing,Space());
					Gen.CaptureAppend(CommasBlock.ElementsTrailing,SemicolonTrailing);

					// Start parsing next list element.
					CommasBlock.BlockSnippet = Snip(CommasStart);
					CommasStart              = Cursor;
					GRAMMAR_RUN(Scan(ListBlock.ElementsTrailing));
					More                     = !StopList.Has(Cursor.Token);
				}
				if(More || Some) {
					// Multiple Semicolon or NewLine separated elements.
					Some = true;
					GRAMMAR_LET(CommasSyntax,Gen.Parenthesis(CommasBlock));
					Gen.SyntaxesAppend(ListBlock.Elements,CommasSyntax);
				}
				else {
					// Single Commas block.
					Gen.CaptureAppend(CommasBlock.ElementsTrailing,ListBlock.ElementsTrailing);
					ListBlock.Form             = CommasBlock.Form;
					ListBlock.Elements         = CommasBlock.Elements;
					ListBlock.ElementsTrailing = CommasBlock.ElementsTrailing;
				}
				if(!More)
					break;
			}
		if(StopList.Has(Cursor.Token)) {
			Context                = SavedContext;
			ListBlock.BlockSnippet = Snip(BlockStart);
			return ListBlock;
		}
		return S77();
	}
	result_t<syntax_t> File() {
		// File := [0oEF 0oBB 0oBF] set Nest=true; set BlockInd=""; set LineInd=""; List Scan end
		if(nat8(Cursor[0])==0xEF) {
			if(nat8(Cursor[1])==0xBB && nat8(Cursor[2])==0xBF) Next(3);
			else return S01();
		}
		GRAMMAR_LET(Block0,List(u8"",&parser::S70,Cursor,capture_t(),punctuation::None,Cursor));
		return Gen.File(Block0);
	}
	result_t<syntax_t> CheckResult(const result_t<syntax_t>& Result) {
		GRAMMAR_LET(Syntax,Result);
		if(Cursor[0]!=0)
			return S70(u8"");
		if(nat(Cursor.Pos-InputString)!=InputLength)
			return S01();
		return Syntax;
	}

	// Friends.
	template<class t> friend result<typename t::syntax_t,typename t::error_t> File(t& Gen,nat n,const char8* s,nat Line);
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Default Generator Framework inheriting a user generator.

template<class gen_t> struct generate: gen_t {
	using syntax_t   = typename gen_t::syntax_t;
	using syntaxes_t = typename gen_t::syntaxes_t;
	using error_t    = typename gen_t::error_t;
	using capture_t  = typename gen_t::capture_t;
	using block_t    = block<syntaxes_t,capture_t>;
	template<class t> using result_t = result<t,error_t>;

	// Passthrough constructor.
	template<class... ts> generate(const ts&... TS): gen_t(TS...) {}

	// Default translators from concrete syntax callbacks to abstract syntax callbacks.
	result<syntax_t,error_t> Units(const snippet& Snippet,const syntax_t& Num,text Units) const {
		GRAMMAR_LET(UnitsIdent,this->Ident(snippet{},u8"units'",Units,u8"'"));
		syntaxes_t Parameters;
		this->SyntaxesAppend(Parameters,Num);
		return this->Call(Snippet,mode::Open,UnitsIdent,block_t{Snippet,Parameters});
	}
	result_t<syntax_t> Parenthesis(const block_t& Block) const {
		if(this->SyntaxesLength(Block.Elements)!=1) {
			GRAMMAR_LET(Macro,this->Native(snippet{},u8"array"));
			return this->Invoke(Block.BlockSnippet,Macro,Block,nullptr,nullptr);
		}
		else return this->SyntaxesElement(Block.Elements,0);
	}
	result_t<syntax_t> StringLiteral(const snippet& Snippet,const capture_t& Capture) const {
		syntaxes_t Characters;
		nat n = 0u;  // Inexplicably gives "warning C4700: uninitialized local variable 'Length' used" when not initialized separately in Visual Studio 2019 14.29.30139
		n=this->CaptureLength(Capture);
		for(nat i=0; i<n; i++) {
			GRAMMAR_LET(ch,this->Char8(snippet{},this->CaptureElement(Capture,i)));
			this->SyntaxesAppend(Characters,ch);
		}
		GRAMMAR_LET(Macro,this->Native(snippet{},u8"array"));
		return this->Invoke(Snippet,Macro,block_t{snippet{},Characters,form::Commas},nullptr,nullptr);
	}
	result_t<syntax_t> StringInterpolate(const snippet& Snippet,place Place,bool /*Brace*/,const block_t& Block) const {
		GRAMMAR_ASSERT(Place==place::String || Place==place::Content);
		GRAMMAR_LET(FunctionSyntax,this->Native(snippet{},Place==place::String? u8"ToString": u8"ToMarkup"));
		return this->Call(Snippet,mode::Open,FunctionSyntax,Block);
	}
	result_t<syntax_t> String(const snippet& Snippet,const syntaxes_t& Splices) const {
		if(this->SyntaxesLength(Splices)==1)
			return this->SyntaxesElement(Splices,0);
		if(this->SyntaxesLength(Splices)==0)
			return this->Parenthesis(block_t{});
		GRAMMAR_LET(FunctionSyntax,this->Native(snippet{},u8"Concatenate"));
		return this->Call(Snippet,mode::Open,FunctionSyntax,block_t{snippet{},Splices,form::Commas});
	}
	result_t<syntax_t> Content(const snippet& Snippet,const syntaxes_t& Splices) const {
		return String(Snippet,Splices);
	}
	result_t<syntax_t> Contents(const snippet& Snippet,const capture_t& /*Leading*/,const syntaxes_t& Splices) const {
		GRAMMAR_LET(Macro,this->Native(snippet{},u8"array"));
		return this->Invoke(Snippet,Macro,block_t{snippet{},Splices},nullptr,nullptr);
	}
	result_t<syntax_t> InvokeMarkup(const snippet& Snippet,text /*StartToken*/,const capture_t& /*Leading*/,const syntax_t& Macro,block_t* Clause,block_t* DoClause,const capture_t& /*TokenLeading*/,const capture_t& /*PreContent*/,const syntax_t& Content,const capture_t& /*PostContent*/) const {
		GRAMMAR_LET(DefineMacro, this->Native(snippet{},u8"operator':='"));
		GRAMMAR_LET(ContentIdent,this->Ident(snippet{},u8"Content",u8"",u8""));
		block_t DefineClause;    this->SyntaxesAppend(DefineClause  .Elements,ContentIdent);
		block_t DefineDoClause;  this->SyntaxesAppend(DefineDoClause.Elements,Content     );
		GRAMMAR_LET(ContentSyntax ,this->Invoke(snippet{},DefineMacro,DefineClause,&DefineDoClause,nullptr));
		auto LastClause = !Clause? block_t{}: DoClause? *DoClause: *Clause;
		this->SyntaxesAppend(LastClause.Elements,ContentSyntax);
		return this->Invoke(Snippet,Macro,!DoClause? LastClause: *Clause,DoClause? &LastClause: nullptr,nullptr);
	}
	result_t<syntax_t> PrefixToken(const snippet& Snippet,mode Mode,text Symbol,const block_t& Block,bool Lift, const syntaxes_t& /*VarAttributes*/ = {}) const {
		if(Symbol==u8"in")
			Symbol=u8":";
		if(Lift)
			return this->Err(Snippet,"S98","Feature is not currently supported");
		GRAMMAR_LET(Macro,/*IsAlnum(Symbol[0])?
			this->Ident(snippet{},Symbol,u8"",u8""):*/
			this->Ident(snippet{},u8"prefix'",Symbol,u8"'"));
		if(Mode==mode::Open || Mode==mode::Closed)
			return this->Call(Snippet,Mode,Macro,Block);
		else if(Mode==mode::With)
			return this->Invoke(Snippet,Macro,Block,nullptr,nullptr);
		else Err();
	}
	result_t<syntax_t> PrefixBrackets(const snippet& Snippet,const block_t& Left,const block_t& Right) const {
		if(Right.Punctuation==punctuation::Braces)
			return this->Err(Snippet,"S98","Feature is not currently supported");
		if(this->SyntaxesLength(Left.Elements)==0) {
			GRAMMAR_LET(Macro,this->Ident(snippet{},u8"prefix'[]'",u8"",u8""));
			return this->Call(Snippet,mode::Closed,Macro,Right);
		}
		GRAMMAR_LET(Macro,this->Ident(snippet{},u8"operator'[]'",u8"",u8""));
		block_t Parameters;
		GRAMMAR_LET(LeftSyntax ,this->Parenthesis(Left )); this->SyntaxesAppend(Parameters.Elements,LeftSyntax);
		GRAMMAR_LET(RightSyntax,this->Parenthesis(Right)); this->SyntaxesAppend(Parameters.Elements,RightSyntax);
		Parameters.Form=form::Commas;
		return this->Call(Snippet,mode::Closed,Macro,Parameters);
	}
	result_t<syntax_t> PostfixToken(const snippet& Snippet,mode Mode,const syntax_t& Left,text Symbol) const {
		GRAMMAR_LET(Macro,this->Ident(snippet{},u8"operator'",Symbol,u8"'"));
		block_t Parameters;
		this->SyntaxesAppend(Parameters.Elements,Left);
		if(Mode==mode::Open || Mode==mode::Closed)
			return this->Call(Snippet,Mode,Macro,Parameters);
		else if(Mode==mode::With)
			return this->Invoke(Snippet,Macro,Parameters,nullptr,nullptr);
		else Err();
	}
	result_t<syntax_t> InfixToken(const snippet& Snippet,mode Mode,const syntax_t& Left,text Symbol,const syntax_t& Right) const {
		if(Symbol==u8"to")
			Symbol=u8"->";
		GRAMMAR_LET(Macro,this->Ident(snippet{},u8"operator'",Symbol,u8"'"));
		block_t Parameters;
		this->SyntaxesAppend(Parameters.Elements,Left);
		this->SyntaxesAppend(Parameters.Elements,Right);
		if(Mode==mode::Closed || Mode==mode::Open)
			return Parameters.Form=form::Commas, this->Call(Snippet,Mode,Macro,Parameters);
		else if(Mode==mode::With)
			return this->Invoke(Snippet,Macro,Parameters,nullptr,nullptr);
		else Err();
	}
	result_t<syntax_t> InfixBlock(const snippet& Snippet,const syntax_t& LeftSyntax,text Symbol,const block_t& Right) const {
		if(Symbol==u8"" || Symbol==u8"is" || Symbol==u8"=")
			Symbol=u8":=";
		block_t LeftBlock;
		this->SyntaxesAppend(LeftBlock.Elements,LeftSyntax);
		GRAMMAR_LET(Macro,this->Ident(snippet{},u8"operator'",Symbol,u8"'"));
		return this->Invoke(Snippet,Macro,LeftBlock,&Right,nullptr);
	}
	syntax_t Leading(const capture_t& /*Capture*/,const syntax_t& Syntax) const {
		return Syntax;
	}
	syntax_t Trailing(const syntax_t& Syntax,const capture_t& /*Capture*/) const {
		return Syntax;
	}
	result<syntax_t,error_t> File(const block_t& Block) const {
		return Parenthesis(Block);
	}

	// String callbacks that can contribute to abstract syntax.
	// In all string callbacks, every non-empty Snippet's text span is guaranteed to be inside the parser's input string.
	void Text(capture_t& Capture,const snippet& Snippet,place Place) const {
		if(Place==place::Content || Place==place::String)
			this->gen_t::Text(Capture,Snippet,Place);
	}
	void NewLine(capture_t& Capture,const snippet& /*Snippet*/,place Place) const {
		if(Place==place::Content) {
			char8 Char8 = '\n'; // We normalize markup NewLine to \n.
			snippet NewSnippet = {};
			NewSnippet.Text = text{&Char8,&Char8+1};
			this->gen_t::Text(Capture,NewSnippet,Place);
		}
	}
	void StringBackslash(capture_t& Capture,const snippet& /*Snippet*/,place Place,char8 Backslashed) const {
		if(Place==place::Content || Place==place::String) {
			// We pass through backslashed control characters as-is.
			char8 Char8 = Backslashed=='n'? '\n': Backslashed=='r'? '\r': Backslashed=='t'? '\t': Backslashed;
			snippet NewSnippet = {};
			NewSnippet.Text = text{&Char8,&Char8+1};
			this->gen_t::Text(Capture,NewSnippet,Place);
		}
	}

	// Optional string callbacks which don't contribute to abstract syntax.
	void LineCmt(capture_t& /*Capture*/,const snippet& /*Snippet*/,place /*Place*/,const capture_t& /*Comments*/) const {}
	void BlockCmt(capture_t& /*Capture*/,const snippet& /*Snippet*/,place /*Place*/,const capture_t& /*Comments*/) const {}
	void IndCmt(capture_t& /*Capture*/,const snippet& /*Snippet*/,place /*Place*/,const capture_t& /*Comments*/) const {}
	void Indent(capture_t& /*Capture*/,const snippet& /*Snippet*/,place /*Place*/) const {}
	void BlankLine(capture_t& /*Capture*/,const snippet& /*Snippet*/,place /*Place*/) const {}
	void Semicolon(capture_t& /*Capture*/,const snippet& /*Snippet*/) const {}
	void MarkupTrim(capture_t& Capture) const {Capture=capture_t();}
	void MarkupStart(capture_t& /*Capture*/,const snippet& /*Snippet*/) const {}
	void MarkupTag(capture_t& /*Capture*/,const snippet& /*Snippet*/) const {}
	void MarkupStop(capture_t& /*Capture*/,const snippet& /*Snippet*/) const {}
	void LinePrefix(capture_t& /*Capture*/,const snippet& /*Snippet*/) const {}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Public parsing interface.

template<class gen_t> result<typename gen_t::syntax_t,typename gen_t::error_t> File(gen_t& Gen,nat n,const char8* s,nat Line=1) {
	auto Parser=parser(Gen,n,s,Line);
	return Parser.CheckResult(Parser.File());
}

}}
