#include <cstdint>
#include "VerseGrammar.h"

#include <iostream>

void* resp;

class gen {
public:
  class syntax_t {
  };
  class error_t {
  };
  class syntaxes_t {
  };
  class capture_t {
  };

  using block_t    = Verse::Grammar::block<syntaxes_t, capture_t>;
  using result_t   = Verse::Grammar::result<syntax_t, error_t>;
  using token      = Verse::Grammar::token;
  using snippet    = Verse::Grammar::snippet;
  using text       = Verse::Grammar::text;
  using place      = Verse::Grammar::place;
  using mode       = Verse::Grammar::mode;
  using nat        = Verse::Grammar::nat;

  static error_t err;
  static syntax_t syn;
  //static result_t res;
#define res (*(result_t*)resp)

  error_t Err(const snippet& s, const char* a, const char* b) const { return err; }
  error_t Err(const snippet& s, const char *, const char *, text, text, text, const char *) const { return err; }
  error_t Err(const snippet& s, const char *, const char *, const char *, const char *, text, text, text) const { return err; }
  error_t Err(const snippet& s, const char *, const char *, text, text, text, const char *, text, const char*) const { return err; }
  error_t Err(const snippet& s, const char *, const char *, text, const char*, text, text, const char *) const { return err; }
  error_t Err(const snippet& s, const char*, const char*, text, const char*, text,text,text,const char*) const { return err; }
  error_t Err(const snippet& s, const char*, const char*, text, text, text) const { return err; }
  error_t Err(const snippet& s, const char*, const char*, text, const char*) const { return err; }
  result_t File(const block_t) const { return res; };
  void Text(const capture_t& c, const snippet& s, const place p) const { };
  void Semicolon(const capture_t&, const snippet&) const { };
  void CaptureAppend(capture_t,capture_t) const { };
  result_t Parenthesis(const block_t&) const { return res; };
  void SyntaxesAppend(syntaxes_t,syntax_t) const {};
  void MarkupTrim(capture_t&) const {};
  void LineCmt(capture_t,snippet,place,capture_t) const {};
  void BlockCmt(capture_t,snippet,place,capture_t) const {};
  void IndCmt(capture_t,snippet,place,capture_t) const {};
  void NewLine(capture_t,snippet,place) const {};
  void Indent(capture_t,snippet,place) const {};
  void BlankLine(capture_t,snippet,place) const {};
  syntax_t Trailing(syntax_t,capture_t) const { return syn; };
  //void ExprSyntax(syntax_t,capture_t) const;
  result_t Char8(snippet,char) const { return res; };
  result_t Char32(const snippet& Snippet, Verse::Grammar::char32 Char32, bool bCode, bool bBackslash) const { return res; } ;
  result_t String(const snippet& Snippet, const syntaxes_t& Splices) const { return res; };
  result_t StringLiteral(const snippet& Snippet, const capture_t& String) const { return res; };
  result_t Escape(const snippet& Snippet, const syntax_t& Escaped) const { return res; };
  result_t Path(const snippet& Snippet, text Value) const { return res; };
  result_t Call(const snippet& Snippet, mode Mode, const syntax_t& ReceiverSyntax, const block_t& CallBlock) const { return res; };
  result_t PostfixAttribute(const snippet& Snippet, const syntax_t& Base, const syntax_t& Attribute) const { return res; };
  result_t PrefixAttribute(const snippet& Snippet, const syntax_t& Attribute, const syntax_t& Base) const { return res; };
  result_t QualIdent(const snippet& Snippet, const block_t& QualifierBlock, text Name) const { return res; };
  result_t Ident(const snippet& Snippet, const text& NameA, const text& NameB, const text& NameC) const { return res; };
  result_t Native(const snippet& Snippet, const text& Name) const { return res; };
  result_t Invoke(const snippet& Snippet, const syntax_t& MacroCommand, const block_t& Clause1, const block_t* Clause2, const block_t* Clause3) const { return
res; };
  result_t Units(const snippet& Snippet, const syntax_t& Num, text Units) const { return res; };
  result_t NumHex(const snippet& Snippet, text Digits) const { return res; };
  result_t Num(const snippet& Snippet, text Digits, text Fraction, text ExponentSign, text Exponent) const { return res; };
  result_t PrefixToken(const snippet& Snippet, mode Mode, text Symbol, const block_t& RightBlock, bool bLift, const syntaxes_t& Specifiers = { }) const { return res; };
  result_t PrefixBrackets(const snippet& Snippet, const block_t& LeftBlock, const block_t& RightBlock) const { return res; };
  result_t StringInterpolate(const snippet& Snippet, place Place, bool bBrace, const block_t& Block) const { return res; };
  void StringBackslash(capture_t& Capture, const snippet& Snippet, place Place, char Backslashed) const {} ;
  void Text(capture_t& Capture, const snippet& Snippet, place Place) const {};
  syntax_t Leading(const capture_t& Capture, const syntax_t& Syntax) const { return syn; };
  result_t InfixBlock(const snippet& Snippet, const syntax_t& Left, text Symbol, const block_t& RightBlock) const { return res; };
  void LinePrefix(capture_t& Capture, const snippet& Snippet) const {};
  result_t InfixToken(const snippet& Snippet, mode Mode, const syntax_t& Left, text Symbol, const syntax_t& Right) const { return res; };
  void MarkupStart(capture_t& Capture, const snippet& Snippet) const {};
  void MarkupTag(capture_t& Capture, const snippet& Snippet) const {};
  void MarkupStop(capture_t& Capture, const snippet& Snippet) const {};
  result_t PostfixToken(const snippet& Snippet, mode Mode, const syntax_t& Left, text Symbol) const { return res; };
  result_t Content(const snippet& Snippet, const syntaxes_t& Splices) const { return res; };
  result_t InvokeMarkup(const snippet& Snippet, text StartToken, const capture_t& Leading, const syntax_t& Macro, block_t* Clause1, block_t* DoClause, const capture_t& TokenLeading, const capture_t& PreContent, const syntax_t& Content, const capture_t& PostContent) const { return res; };
  result_t InSpecifiers(const snippet& Snippet, const syntax_t& In, const syntaxes_t& InSpecifiers) const { return res; };
  result_t Contents(const snippet& Snippet, const capture_t& Leading, const syntaxes_t& Splices) const { return res; };
  static nat SyntaxesLength(const syntaxes_t& As) { return 0; };
  static nat CaptureLength(const capture_t& S) { return 0; };
} g;

int
main()
{
  std::cout << "this is main\n";

  //gen::res = gen::result_t(gen::syn);
  resp = new gen::result_t();

  gen::result_t rslt = Verse::Grammar::File(g, 1, "1");
}
