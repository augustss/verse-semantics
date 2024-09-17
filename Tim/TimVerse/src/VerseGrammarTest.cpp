#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <cstdio>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <string>
#define TIM 1
#include "VerseGrammar.h"
using namespace Verse::Grammar;

// Types.
struct test_error {
	std::string Filename;
	nat         StartLine, StartColumn;
	std::string Code;
	std::string Message;
};

// Helpers.
[[noreturn]] void TestErr() {
	__debugbreak();
	std::exit(1);
};
std::string TestLoadTextFile(const char* Filename) {
	auto LoadFile=std::fopen("About.verse","rb");
	if(!LoadFile)
		std::cout << "Failed to open ",Filename,"\n", TestErr();
	std::fseek(LoadFile,0,SEEK_END);
	auto LoadSize=std::ftell(LoadFile);
	std::fseek(LoadFile,0,SEEK_SET);
	std::string LoadString(LoadSize,'\0');
	auto LoadedSize=fread(&LoadString[0],1,LoadSize,LoadFile);
	if(LoadedSize!=LoadSize)
		std::cout << "Failed to read " << Filename << "\n", TestErr();
	return LoadString;
}
template<class t,class u,class... vs> std::string ToString(const t& T,const u& U,const vs&... VS);
std::string ToString(const std::string& S) {return S;}
std::string ToString(const char* S) {return S;}
std::string ToString(text Text) {
	nat n=Length(Text);
	std::string String(n,'\0');
	for(nat i=0; i<n; i++)
		String[i]=Text[i];
	return String;
}
template<class t> std::string ToString(const t& T) {return std::to_string(T);}
void Append(std::string& S) {}
template<class t,class... us> void Append(std::string& S,const t& T,const us&... US) {S+=ToString(T); Append(S,US...);}
std::string ToString(const test_error& TestError) {
	return ToString(TestError.Filename,"(",TestError.StartLine,",",TestError.StartColumn,"): ",
		"error verse",TestError.Code,": ",TestError.Message);
}
template<class t,class u,class... vs> std::string ToString(const t& T,const u& U,const vs&... VS) {
	std::string Result;
	Append(Result,T,U,VS...);
	return Result;
}
template<class t> std::string ToStringSeparated(const std::vector<t>& Elements,const char* Separator) {
	std::string Result;
	bool First=true;
	for(const auto& E:Elements) {
		if(!First)
			Append(Result,Separator);
		Append(Result,E);
		First=false;
	}
	return Result;
}
std::string ToStringBase(nat Value,nat Base,nat MinDigits) {
	static const char Digits[][16]={"0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"};
	std::string Result;
	nat Count = 0;
	while(Count++<MinDigits || Value!=0) {
		auto q = Value/Base;
		auto r = Value-q*Base;
		Result = ToString(Digits[r],Result);
		Value  = q;
	}
	return Result;
}

// Generator to test lossnessness.
struct generate_string {

	// Types we must expose to parser.
	using syntax_t   = std::string;
	using syntaxes_t = std::vector<std::string>;
	using error_t    = test_error;
	using capture_t  = std::string;
	using block_t    = block<syntaxes_t,capture_t>;

	// Manipulation operations we must expose to parser.
	static void SyntaxesAppend(syntaxes_t& as,const syntax_t& a) {as.push_back(a);}
	static nat SyntaxesLength(syntaxes_t& as) {return as.size();}
	static syntax_t SyntaxesElement(syntaxes_t& as,nat i) {return as[i];}
	static void CaptureAppend(capture_t& s,const capture_t& t) {s.insert(s.end(),t.cbegin(),t.cend());}

	// Internal.
	const char* Filename;
	static std::string CheckSnippet(const snippet& Snippet,const std::string& Result) {
		if(Result!=ToString(Snippet.Text))
			std::cout << "\n    snippet      {" << ToString(Snippet.Text) << "}\n    reproduction {" << Result << "}", TestErr();
		return Result;
	}
	static std::string ToStringBlock(const block_t& Block) {
		GRAMMAR_ASSERT(Block.Token || !Block.TokenLeading.size());
		GRAMMAR_ASSERT(Block.Punctuation!=punctuation::None || !Block.PunctuationLeading.size());
		GRAMMAR_ASSERT(Block.Punctuation!=punctuation::None || !Block.PunctuationTrailing.size());
		auto Elements = ToString(ToStringSeparated(Block.Elements,Block.Form==form::Commas? ",": ""),Block.ElementsTrailing);
		auto PunctuatedList =
			Block.Punctuation==punctuation::Braces?        ToString("{",Elements,"}"):
			Block.Punctuation==punctuation::Parens?        ToString("(",Elements,")"):
			Block.Punctuation==punctuation::Brackets?      ToString("[",Elements,"]"):
			Block.Punctuation==punctuation::AngleBrackets? ToString("<",Elements,">"):
			Block.Punctuation==punctuation::Qualifier?     ToString("(",Elements,":)"):
			Block.Punctuation==punctuation::Dot?           ToString(".",Elements):
			Block.Punctuation==punctuation::Colon?         ToString(":",Elements):
			Elements;
		return CheckSnippet(Block.BlockSnippet,ToString(
			ToStringSeparated(Block.Specifiers,""),
			Block.TokenLeading,
			Block.Token,
			Block.PunctuationLeading,
			PunctuatedList,
			Block.PunctuationTrailing
		));
	}

	// Syntax generation for abstract syntax that coincides with concrete syntax.
	template<class... ps> error_t Err(const snippet& Snippet,const char* Code,const ps&... PS) const {
		return test_error{Filename,Snippet.StartLine,Snippet.StartColumn,Code,ToString(PS...)};
	}
	result<syntax_t,error_t> Num(const snippet& Snippet,text Digits,text Fraction,text ExponentSign,text Exponent) const {
		return CheckSnippet(Snippet,ToString(Digits,Fraction? ToString(".",Fraction): "", Exponent? ToString("e",ExponentSign,Exponent): ""));
	}
	result<syntax_t,error_t> NumHex(const snippet& Snippet,text Digits) const {
		return CheckSnippet(Snippet,ToString("0x",Digits));
	}
	result<syntax_t,error_t> Units(const snippet& Snippet,const syntax_t& Num,text Units) const {
		return CheckSnippet(Snippet,ToString(Num,Units));
	}
	result<syntax_t,error_t> Char8(const snippet& Snippet,char8 Char8) const {
		return CheckSnippet(Snippet,ToString("0o",ToStringBase(nat(Char8),16,Length(Snippet.Text)-2)));
	}
	result<syntax_t,error_t> Char32(const snippet& Snippet,char32 Char32,bool Code,bool Backslash) const {
		char s[2]={char(Char32),'\0'}; //!!TODO: Fix for non-ASCII.
		if(Code)
			return CheckSnippet(Snippet,ToString("0u",ToStringBase(nat(Char32),16,Length(Snippet.Text)-2)));
		else if(!Backslash)
			return CheckSnippet(Snippet,ToString("'",s,"'"));
		else
			return CheckSnippet(Snippet,ToString("'\\",Char32=='\r'? "r": Char32=='\n'? "n": Char32=='\t'? "t": s,"'"));
	}
	result<syntax_t,error_t> Path(const snippet& Snippet,text Value) const {
		return CheckSnippet(Snippet,ToString(Value));
	}
	result<syntax_t,error_t> Invoke(const snippet& Snippet,const syntax_t& Macro,const block_t& Clause,const block_t* DoClause,const block_t* PostClause) const {
		return CheckSnippet(Snippet,ToString(Macro,ToStringBlock(Clause),DoClause? ToStringBlock(*DoClause): "",PostClause? ToStringBlock(*PostClause): ""));
	}
	result<syntax_t,error_t> Native(const snippet& Snippet,text Name) const {
		return CheckSnippet(Snippet,ToString(Name));
	}
	result<syntax_t,error_t> Ident(const snippet& Snippet,text A,text B,text C) const {
		return CheckSnippet(Snippet,ToString(A,B,C));
	}
	result<syntax_t,error_t> QualIdent(const snippet& Snippet,const block_t& QualifierBlock,text Name) const {
		return CheckSnippet(Snippet,ToString(ToStringBlock(QualifierBlock),Name));
	}
	result<syntax_t,error_t> Call(const snippet& Snippet,mode Mode,const syntax_t& FunctionSyntax,const block_t& CallBlock) const {
		return CheckSnippet(Snippet,ToString(FunctionSyntax,ToStringBlock(CallBlock)));
	}
	result<syntax_t,error_t> Escape(const snippet& Snippet,const syntax_t& Escaped) const {
		return CheckSnippet(Snippet,ToString("&",Escaped));
	}
	result<syntax_t,error_t> PrefixAttribute(const snippet& Snippet,const syntax_t& Left,const syntax_t& Right) const {
		return CheckSnippet(Snippet,ToString("@",Left,Right));
	}
	result<syntax_t,error_t> PostfixAttribute(const snippet& Snippet,const syntax_t& Left,const syntax_t& Right) const {
		return CheckSnippet(Snippet,ToString(Left,"@",Right));
	}
	result<syntax_t,error_t> File(const block_t& Block) const {
		return CheckSnippet(Block.BlockSnippet,ToStringBlock(Block));
	}

	// Syntax generation for non-canonical concrete syntax.
	result<syntax_t,error_t> Parenthesis(const block_t& Block) const {
		return CheckSnippet(Block.BlockSnippet,ToStringBlock(Block));
	}
	result<syntax_t,error_t> StringLiteral(const snippet& Snippet,const capture_t& S) const {
		return CheckSnippet(Snippet,S);
	}
	result<syntax_t,error_t> StringInterpolate(const snippet& Snippet,place Place,bool Brace,const block_t& Block) const {
		if(auto S=ToStringBlock(Block); Brace)
			return CheckSnippet(Snippet,ToString("{",S,"}"));
		else
			return CheckSnippet(Snippet,ToString("&",S));
	}
	result<syntax_t,error_t> String(const snippet& Snippet,const syntaxes_t& Splices) const {
		return CheckSnippet(Snippet,ToString("\"",ToStringSeparated(Splices,""),"\""));
	}
	result<syntax_t,error_t> Content(const snippet& Snippet,const syntaxes_t& Splices) const {
		return CheckSnippet(Snippet,ToString(ToStringSeparated(Splices,"")));
	}
	result<syntax_t,error_t> Contents(const snippet& Snippet,const capture_t& Leading,const syntaxes_t& Splices) const {
		std::string Result=Leading;
		for(auto E:Splices)
			Append(Result,"~",E);
		return CheckSnippet(Snippet,Result);
	}
	result<syntax_t,error_t> InvokeMarkup(const snippet& Snippet,text StartToken,const capture_t& Leading,const syntax_t& Macro,block_t* Clause,block_t* DoClause,const capture_t& TokenLeading,const capture_t& PreContent,const syntax_t& Content,const capture_t& PostContent) const {
		return CheckSnippet(Snippet,ToString(StartToken,Leading,Macro,
			Clause? ToStringBlock(*Clause): "",
			DoClause? ToStringBlock(*DoClause): "",
			TokenLeading,PreContent,Content,PostContent));
	}
	result<syntax_t,error_t> PrefixToken(const snippet& Snippet,mode Mode,text Symbol,const block_t& RightBlock,bool Lift,const syntaxes_t& Temp=syntaxes_t{}/*!!*/) const {
		return CheckSnippet(Snippet,ToString(Symbol,ToStringBlock(RightBlock)));
	}
	result<syntax_t,error_t> PrefixBrackets(const snippet& Snippet,const block_t& LeftBlock,const block_t& RightBlock) const {
		return CheckSnippet(Snippet,ToString("[",ToStringBlock(LeftBlock),"]",ToStringBlock(RightBlock)));
	}
	result<syntax_t,error_t> PostfixToken(const snippet& Snippet,mode Mode,const syntax_t& Left,text Symbol) const {
		return CheckSnippet(Snippet,ToString(Left,Symbol));
	}
	result<syntax_t,error_t> InfixToken(const snippet& Snippet,mode Mode,const syntax_t& Left,text Symbol,const syntax_t& Right) const {
		return CheckSnippet(Snippet,ToString(Left,Symbol,Right));
	}
	result<syntax_t,error_t> InfixBlock(const snippet& Snippet,const syntax_t& Left,text Symbol,const block_t& RightBlock) const {
		return CheckSnippet(Snippet,ToString(Left,RightBlock.Token? text(): Symbol,ToStringBlock(RightBlock)));
	}
	syntax_t Leading(const capture_t& Capture,const syntax_t& Syntax) const {
		return ToString(Capture,Syntax);
	}
	syntax_t Trailing(const syntax_t& Syntax,const capture_t& Capture) const {
		return ToString(Syntax,Capture);
	}

	// String operations for concrete syntax.
	void Text(capture_t& Capture,const snippet& Snippet,place Place) const {
		Append(Capture,Snippet.Text);
	}
	void NewLine(capture_t& Capture,const snippet& Snippet,place Place) const {
		Append(Capture,Snippet.Text);
	}
	void StringBackslash(capture_t& Capture,const snippet& Snippet,place Place,char8 Backslashed) const {
		char s[]={char(Backslashed),0};
		Append(Capture,CheckSnippet(Snippet,ToString("\\",s)));
	}
	void LineCmt(capture_t& Capture,const snippet& Snippet,place Place,const capture_t& Comment) const {
		Append(Capture,CheckSnippet(Snippet,ToString("#",Comment)));
	}
	void BlockCmt(capture_t& Capture,const snippet& Snippet,place Place,const capture_t& Comment) const {
		Append(Capture,CheckSnippet(Snippet,ToString("<#",Comment,"#>")));
	}
	void IndCmt(capture_t& Capture,const snippet& Snippet,place Place,const capture_t& Comment) const {
		Append(Capture,CheckSnippet(Snippet,ToString("<#>",Comment)));
	}
	void Indent(capture_t& Capture,const snippet& Snippet,place Place) const {
		Append(Capture,Snippet.Text);
	}
	void BlankLine(capture_t& Capture,const snippet& Snippet,place Place) const {
		Append(Capture,Snippet.Text);
	}
	void Semicolon(capture_t& Capture,const snippet& Snippet) const {
		GRAMMAR_ASSERT(Snippet.Text==u8";");
		Append(Capture,Snippet.Text);
	}
	void MarkupStart(capture_t& Capture,const snippet& Snippet) const {
		Append(Capture,Snippet.Text);
	}
	void MarkupTrim(capture_t& Capture) const {
	}
	void MarkupTag(capture_t& Capture,const snippet& Snippet) const {
		Append(Capture,"/",Snippet.Text);
	}
	void MarkupStop(capture_t& Capture,const snippet& Snippet) const {
		GRAMMAR_ASSERT(Snippet.Text==u8">");
		Append(Capture,Snippet.Text);
	}
	void LinePrefix(capture_t& Capture,const snippet& Snippet) const {
		GRAMMAR_ASSERT(Snippet.Text==u8"&");
		Append(Capture,Snippet.Text);
	}
};

// Test parsing.
void TestParsing(const char* ExpectedErrorCode,const char* Filename,const char8* Code0) {
	std::cout << "parse{" << (char*)Code0 << "}";

	// Check parser text reproducibility.
	auto GenerateString = generate_string{Filename};
	auto ParseString    = File(GenerateString,std::strlen((char*)Code0),Code0);
	if(ParseString && ExpectedErrorCode!="S00")
		std::cout << "Parse unexpectedly succeeded\n", TestErr();
	if(ParseString && *ParseString!=(char*)Code0)
		std::cout << "Parse mismatched{",*ParseString,"}";
	if(!ParseString && ParseString.GetError().Code!=ExpectedErrorCode)
		std::cout << ToString(ParseString.GetError()), TestErr();

}
void TestParsing(const char* ExpectedErrorCode,const char8* Code,const char8* Check=nullptr) {
	return TestParsing(ExpectedErrorCode,"immediate",Code);
}
void TestParsing(const char* ExpectedErrorCode,const char* Code,const char* Check=nullptr) {
	return TestParsing(ExpectedErrorCode,"immediate",(const char8*)Code);
}

// Main.
int main(int argc, char *argv[]) {
	std::cout << "Hello.\r\n";

	// Run TestParsing through included tests.
#include "VerseGrammarTests.h"

	// About.verse.
	auto Filename = "About.verse";
	auto String   = TestLoadTextFile(Filename);
	std::cout << "Size " << String.size() << "\n";
	TestParsing("S00",Filename,(char8*)String.c_str());

	std::cout << "Bye.\r\n";
}
