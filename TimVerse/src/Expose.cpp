//==============================================================================================================================================================
// Comparison implementation.

#include "Terse.h"
#pragma warning(disable:4100 4244 4706)
#if __clang__
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#endif
using namespace Verse;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Containers.

// Functions.
array<comparable> managed_function<>::OnKeys() const {
	VERSE_UNEXPECTED;
}

current_step managed_function<>::MapCallStep(const map<>& Self,const future<>& R,const future<>& P,const step& Step0) {
	VERSE_UNIMPLEMENTED;
}

// Arrays.
current_step managed_function<>::ArrayCallStep(const array<>& Self,const future<>& R,const future<>& P,const step& Step0) {
	if(auto N=Self->ContainerLength; N==0)
		return FailStep(locus{},"ArrayCallStep-immediate",Step0);
	else return WhenResolveStep(VERSE_HERE("ArrayCallStep-parameter"),only_cardinalities&resolves,P,
		[=]()->current_step {
			if(Cast<any>(P) || N>1 && !(iterates<=Thread->AllowFx+contradicts))
				return Step0;
			return ForForkStep(locus{},Truth(computes),0,N,Step0,[=](nat I,const step& Step1)->current_step {
				return UnifyStep("ArrayCallStep-fork",locus{},P,I,Step1);
			});
		},
		[=](const any& A,const step& Step1)->current_step {
			if(auto i=CastIndex(A); i<N)
				return UnifyStep("ArrayCallStep-result",locus{},R,Self(i),Step1);
			else
				return FailStep(locus{},"ArrayCallStep-parameter",Step1);
		}
	);
}

// Tuples.
box<tuple<>> Verse::ExposeValue(tuple<>) {
	return FalseBox;
}

// Bool.
box<option<tuple<>>> Verse::ExposeValue(bool b) {
	return b? TrueBox: FalseOptionBox;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Atom comparison.

total_ordering Verse::CompareDynamic(const string& a,const char8* b) {
	nat al=Length(a),i;
	for(i=0; i<al&&b[i]&&a[i]==b[i]; i++) {}
	return CompareTotal(i<al?a[i]:u8'\0',char8(b[i]));
}
total_ordering Verse::CompareDynamic(const string& a,const char* b) {
	return CompareDynamic(a,(char8*)b);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Signatures.

path expose<float32   >::ExposeStaticSignature()         {return "/Verse.org/float32"_VP;}
path expose<float64   >::ExposeStaticSignature()         {return "/Verse.org/float64"_VP;}
path expose<char      >::ExposeStaticSignature()         {return "/Verse.org/char8"_VP;}
path expose<bool      >::ExposeStaticSignature()         {return "/Verse.org/function"_VP;}
path expose<range_span>::ExposeStaticSignature()         {return "/Verse.org/function"_VP;}
path expose<array<>   >::ExposeStaticSignature()         {return "/Verse.org/function"_VP;}
path expose<real      >::ExposeStaticSignature()         {return "/Verse.org/real"_VP;}
path expose<rational  >::ExposeStaticSignature()         {return "/Verse.org/real"_VP;}
path expose<natural   >::ExposeStaticSignature()         {return "/Verse.org/real"_VP;}
path expose<integer   >::ExposeStaticSignature()         {return "/Verse.org/real"_VP;}

path Verse::ExposeSignature(Internal::integer_immediate) {return "/Verse.org/real"_VP;}
path Verse::ExposeSignature(Internal::ptr_immediate)     {return "/Verse.org/pointer"_VP;}

path managed_function<>::OnSignature() const             {return "/Verse.org/function"_VP;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Hashing.

nat Verse::PayloadHash(nat            n) {return Internal::HashPayload+n*Internal::HashInteger/65536;}
nat Verse::ExposeHash (const managed* p) {return PayloadHash(nat(p));}
nat Verse::ExposeHash (nat            n) {return PayloadHash(kernel::EncodeIntegerDigit(n));}
nat Verse::ExposeHash (int64          n) {return PayloadHash(kernel::EncodeIntegerDigit(n));}
nat Verse::ExposeHash (nat32          n) {return PayloadHash(kernel::EncodeIntegerDigit(n));}
nat Verse::ExposeHash (int32          n) {return PayloadHash(kernel::EncodeIntegerDigit(n));}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Dynamic dispatch.

VERSE_NO_INLINE nat Verse::NewImmediateMethodsIndex(const immediate_methods& these) {
	// To support DLLs, each will have their own ImmediateMethodsOfType<t>, so we'd need to match them up by name or somesuch.
	static nat Count=3;
	nat Index=++Count;
	if(Index>=CountOf(ImmediateMethodsOfIndex))
		VERSE_ERR("Expand ImmediateMethodsOfIndex up to PayloadMethodsMask and recompile");
	ImmediateMethodsOfIndex[Index]=these;
	//Print("methods_index ",Index,": ",these.ImmediateNativeName);
	return Index;
}
path Verse::Signature(const pin<any>& A) {
    if(const auto P=A.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P)->OnSignature();
	else
		return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateSignature(A);
}
const char* Verse::NativeNameOf(const pin<any>& A) {
    if(const auto P=A.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P)->OnNativeName();
	else
		return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateNativeName;
}
dynamic_ordering Verse::CompareDynamic(const any& A,const any& B) {
	if(A.Payload==B.Payload)
		return equality_ordering(true);
	pin<any> AP(A),BP(B);
	if(auto P=AP.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P)->OnCompare(B);
	else if(!kernel::PayloadIsManaged(P))
		return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateCompare(A,B);
	if(kernel::IsPointer(kernel::AdvancePinned(AP))) {
		if(kernel::IsPointer(kernel::AdvancePinned(BP)))
			return dynamic_ordering(AP.Payload==BP.Payload);
		else if(IsComparableDynamic(B))
			return dynamic_ordering(false);
	}
	return dynamic_ordering();
}
bool Verse::IsComparableDynamic(const pin<any>& A) {
	if(auto P=A.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P)->OnIsComparable();
	else if(!kernel::PayloadIsManaged(P))
		return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateIsComparable(A);
	else if(kernel::IsPointer(kernel::AdvancePinned(A)))
		return true;
	else
		return false;
}
dynamic_ordering Verse::CompareDynamic(const future<>& U,const future<>& V) {
	if(auto A=Cast<any>(U),B=Cast<any>(V); A&&B)
		return CompareDynamic(A.PresumeReference(),B.PresumeReference());
	return dynamic_ordering();
}
equality_ordering Verse::CompareDynamic(const comparable& a,const comparable& b) {
	auto o=CompareDynamic(static_cast<const any&>(a),static_cast<const any&>(b));
	VERSE_ASSERT(!o.IsIncomparable());
	return equality_ordering(o==nullptr);
}
nat Verse::HashDynamic(const pin<any>& A) {
	if(auto P=A.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P)->OnHash();
	else if(!kernel::PayloadIsManaged(P))
		return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateHash(A);
	else if(kernel::IsPointer(kernel::AdvancePinned(A)))
		return PayloadHash(A.Payload);
	VERSE_UNEXPECTED;
}
string Verse::ToString(const future<>& F) {
	if(auto A=Cast<any>(F)) {
		if(auto P=A->Payload; kernel::PayloadIsBox(P))
			return kernel::AllocationBase(P)->OnToString();
		else if(!kernel::PayloadIsManaged(P))
			return ImmediateMethodsOfIndex[kernel::PayloadMethodsIndex(P)].ImmediateToString(A.Coerce());
		else if(kernel::IsPointer(kernel::AdvancePinned(F)))
			return "pointer"_VS;
	}
	return ".."_VS;
}
void        managed::OnCloned(cloner&) {}
void        managed::OnDestructor()         {VERSE_UNEXPECTED;}
void        managed::OnCopy(managed*) const {VERSE_UNEXPECTED;}
path        managed::OnSignature()    const {VERSE_UNEXPECTED;}
const char* managed::OnNativeName()   const {VERSE_UNEXPECTED;}
string      managed::OnToString()     const {VERSE_UNEXPECTED;}
bool        managed::OnIsMap()        const {return false;}
bool        managed::OnIsSet()        const {return false;}
bool        managed::OnIsArray()      const {return false;}
bool        managed::OnIsComparable() const {return false;}
bool        managed::OnIsUnique()     const {VERSE_UNEXPECTED;}
nat         managed::OnHash()         const {VERSE_UNEXPECTED;}
dynamic_ordering managed::OnCompare(const pin<any>& B) const {VERSE_UNEXPECTED;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Characters. Encoding guide:
//    C++ char:   ASCII character   encoded as box<char32> with code point 0..0x7F.
//    C++ char8:  UTF-8 code unit   encoded as box<char32> with code point 0..0x7F or box<char8> with code unit 0x80..0xFF.
//    C++ char16: UTF-16 code unit  encoded as box<char32> with code point 0..0xD7FF | surrogate 0xD800..0xDFFF | code point 0xE000..0xFFFF.
//    C++ char32: UTF-32 code point encoded as box<char32> with code point 0..0xD7FF | code point 0xE000..0x10FFFF.

// Payload.
static nat EncodeCharacter(char ch) {
	return kernel::EncodeImmediate(nat(ch),Internal::MethodsIndexOf<char32>);
}
static nat EncodeCharacter(char8 cu) {
	return
		cu<=0x7F? kernel::EncodeImmediate(nat(cu),Internal::MethodsIndexOf<char32>):
		          kernel::EncodeImmediate(nat(cu),Internal::MethodsIndexOf<char8 >);
}
static nat EncodeCharacter(char16 cu) {
	return kernel::EncodeImmediate(nat(cu),Internal::MethodsIndexOf<char32>); // Including surrogates.
}
static nat EncodeCharacter(char32 cp) {
	return kernel::EncodeImmediate(nat(cp),Internal::MethodsIndexOf<char32>);
}

// Exposing.
comparable Verse::ExposeValue(char   ch) {return comparable(construct_payload{},EncodeCharacter(ch));}
comparable Verse::ExposeValue(char8  cu) {return comparable(construct_payload{},EncodeCharacter(cu));}
comparable Verse::ExposeValue(char16 cu) {return comparable(construct_payload{},EncodeCharacter(cu));}

// Hashing.
nat Verse::ExposeHash(char   ch) {return PayloadHash(EncodeCharacter(ch));}
nat Verse::ExposeHash(char8  cu) {return PayloadHash(EncodeCharacter(cu));}
nat Verse::ExposeHash(char16 cu) {return PayloadHash(EncodeCharacter(cu));}
nat Verse::ExposeHash(char32 cp) {return PayloadHash(EncodeCharacter(cp));}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// String Operations.

string Verse::operator+(const string& S,const char* T) {
	return S+string(T);
}
string Verse::operator+(const string& S,const char8* T) {
	return S+string(T);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// ToString.

string Verse::ExposeToString(const char*    s) {return string_fix_me(s);}
string Verse::ExposeToString(const char8*   s) {return string_fix_me(s);}
string Verse::ExposeToString(span<char8>    s) {return s;}
string Verse::ExposeToString(const integer& i) {return ToStringBase(i,10);}
string Verse::ExposeToString(int64          i) {return ToStringBase(i,10);}
string Verse::ExposeToString(nat            i) {return ToStringBase(i,10);}
string Verse::ExposeToString(int32          i) {return ToStringBase(i,10);}
string Verse::ExposeToString(nat32          i) {return ToStringBase(i,10);}
string Verse::ExposeToString(char           c) {return string{char8(c)};}
string Verse::ExposeToString(char8          c) {return string{c};}
string Verse::ExposeToString(char16         c) {
	// Encodes surrogates independent of pairing, a non-standard but well defined encoding known as WTF-8.
	return ExposeToString(char32(c));
}
string Verse::ExposeToString(char32 c) {
    if     (c<=0x00007F) return string{char8(c)};
    else if(c<=0x0007FF) return string{char8(0xC0|c/0x00040),char8(0x80|c       %0x40)};
    else if(c<=0x00FFFF) return string{char8(0xE0|c/0x01000),char8(0x80|c/0x0040%0x40),char8(0x80|c       %0x40)};
    else if(c<=0x10FFFF) return string{char8(0xF0|c/0x40000),char8(0x80|c/0x1000%0x40),char8(0x80|c/0x0040%0x40),char8(0x80|c%0x40)};
    else VERSE_ERR("Bad Unicode code point");
}
string Verse::ExposeToString(const string8& a) {
    return a;
}
string Verse::ExposeToString(const string16& a) {
    VERSE_UNIMPLEMENTED;
}
string Verse::ExposeToString(const string32& a) {
    VERSE_UNIMPLEMENTED;
}
string Verse::ExposeToString(const array<>& as) {
	if(auto SO=Cast<string>(as))
		return ToString(SO.Coerce());
	string rs=False;
	for(nat i=0,n=Length(as); i<n; i++) {
		if(i>0)
			rs+=","_VS;
		rs+=ToCode(as(i));
	}
    return rs;
}
string Verse::ExposeToString(const path& p) {
	return p.Value;
}
string Verse::ExposeToString(fx Fx)  {
	return ToCode(Fx);
}
string Verse::ExposeToString(const locus& Locus) {
	return
		(Locus.Filename? Locus.Filename: "command"_VS)+
		(Locus.StartLine? ToString("("_VS,Locus.StartLine,","_VS,Locus.StartColumn,")"_VS): False);
}
string Verse::ExposeToString(const error& Error) {
	return ToString(Error.Locus,": error verse",Error.ErrorCode,": ",Error.Message,
		Error.Internal? ToString(" [",Error.Internal,"]"): False);
}
string Verse::ToString() {return False;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Paths.

path::path(const string& Value0): Value(Value0) {
	// Should validate.
}
total_ordering Verse::ExposeCompare(const path& P0,const path& P1) {
	return CompareTotal(P0.Value,P1.Value);
}
nat Verse::ExposeHash(const path& P) {
	return Hash(P.Value); //!!sucks
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Locus and errors.

void Verse::Internal::PrintError(const char* File,nat Line,const char* Function,const error& Error) {
	Print(), Print(Error);
}
void Verse::Err(const error& Error) {
	Print(), Print(Error);

	VERSE_DEBUG_BREAK;
	Verse::Exit(1);
}
equality_ordering Verse::ExposeCompare(const locus& A,const locus& B) {
	return equality_ordering(A.Filename==B.Filename && A.StartLine==B.StartLine && A.StartColumn==B.StartColumn && A.StopLine==B.StopLine && A.StopColumn==B.StopColumn);
}
equality_ordering Verse::ExposeCompare(const error& A,const error& B) {
	return equality_ordering(A.Locus==B.Locus && A.ErrorCode==B.ErrorCode && A.Priority==B.Priority && A.Message==B.Message && A.Internal==B.Internal);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// ToCode to parseable syntax.

// Integer family.
VERSE_NO_INLINE string Verse::ToStringBase(const integer& Value0,nat Base,nat MinDigits) {
	static const char8 Digits[]=u8"0123456789ABCDEF";
    VERSE_ASSERT(Base>0 && Base<=16);
	bool   NonNegative = Value0>=0;
    auto   Value       = NonNegative? Value0: -Value0;
	nat    Count       = 0;
	string Result;
	while(Count++<MinDigits || Value!=0) {
		auto[q,r] = EuclideanDivision(Value,Base).Coerce();
		Result    = ToString(Digits[Presume<nat>(r)],Result);
		Value     = q;
	}
	if(NonNegative)
		return Result;
	return ToString("-",Result);
}
string Verse::ToCode(int32 i) {return ToCode(integer(i));}
string Verse::ToCode(nat32 i) {return ToCode(integer(i));}
string Verse::ToCode(int64 i) {return ToCode(integer(i));}
string Verse::ToCode(nat64 i) {return ToCode(integer(i));}
VERSE_NO_INLINE string Verse::ToCode(const integer& I) {
    return ToString(I);
}
string Verse::ToCode(const rational& a) {
    if(auto aio=Cast<integer>(a))
        return ToCode(aio.Coerce());
    rational a0=a;
    integer d0=Denominator(a);
    for(nat Fraction=1;; Fraction++) {
        integer d1=Denominator(a0*=10);
        if(auto aio=Cast<integer>(a0)) {
            string s=ToCode(aio.Coerce());
            if(auto n=Length(s); n<=Fraction)
				s=For(Fraction+1-n,[](nat){return u8'0';})+s;
            return s.Slice(0,Length(s)-Fraction)+"."_VS+s.Slice(Length(s)-Fraction,Length(s));
        }
        else if(d1==d0)
            return ToCode(Numerator(a)) + "/"_VS + ToCode(Denominator(a));
        d0=d1;
    }
}

// Characters.
string Verse::ToCode(char   c) {return ToCode(char8(c));}
string Verse::ToCode(char8  c) {return c<0x7F? ToCode(char32(c)): ToString("0o",ToStringBase(nat(c),16));}
string Verse::ToCode(char16 c) {return ToCode(char32(c));} // Correct even for surrogates 0xD800..0xDFFF.
string Verse::ToCode(char32 c) {return c>=0x20&&c<0x7f? string{u8'\'',char8(c),u8'\''}: ToString("0u",ToStringBase(nat(c),16));}

// Fx as specifies relative to specified default.
// When default is <effects>, always returns a non-blank set of specifiers.
string Verse::ToCode(fx Fx,fx DefaultFx) {
	if(Fx==effects) return "<effects>"_VS;
	string s;

	// Cardinalities.
	if(Fx+contradicts!=DefaultFx+contradicts) {
		if     (Fx<=contradicts        )  s+="<contradicts>"_VS;
		else if(Fx<=succeeds           )  s+="<succeeds>"_VS;
		else if(Fx<=succeeds+ambiguates)  s+="<succeeds><ambiguates>"_VS;
		else if(Fx<=fails              )  s+="<fails>"_VS;
		else if(Fx<=decides            )  s+="<decides>"_VS;
		else if(Fx<=decides +ambiguates)  s+="<decides><ambiguates>"_VS;
		else if(Fx<=resolves           )  s+="<resolves>"_VS;
		else if(Fx<=iterates           )  s+="<iterates>"_VS;
		else if(Fx<=abstracts          )  s+="<abstracts>"_VS;
		else                              s+="<abstracts><iterates>"_VS;
	}

	// Recurses.
	if(Fx+converges!=DefaultFx+converges) {
		if     (Fx<=converges          )  s+="<converges>"_VS;
		else                              s+="<recurses>"_VS;
	}

	// Transacts and imperatives.
	if(Fx+computes!=DefaultFx+computes) {
		if(Fx+computes<=computes) s+="<computes>"_VS;
		else {

			// Transacts.
			if(Fx+no_transacts!=DefaultFx+no_transacts) {
				if(transacts<=Fx+no_transacts  )  s+="<transacts>"_VS;
				else {
					if(allocates<=Fx+no_transacts)  s+="<allocates>"_VS;
					if(reads    <=Fx+no_transacts)  s+="<reads>"_VS;
					if(writes   <=Fx+no_transacts)  s+="<writes>"_VS;
				}
			}

			// Imperatives.
			if(Fx+no_imperatives!=DefaultFx+no_imperatives) {
				if(interacts<=Fx+no_imperatives)  s+="<interacts>"_VS;
				if(throws   <=Fx+no_imperatives)  s+="<throws>"_VS;
				if(suspends <=Fx+no_imperatives)  s+="<suspends>"_VS;
			}
		}
	}

	// Rejects.
	if(Fx+accepts!=DefaultFx+accepts) {
		if     (Fx+accepts<=accepts)  s+="<accepts>"_VS;
		else                          s+="<rejects>"_VS;
	}

	return s;
}
string Verse::ToCode(const path& p) {
	return p.Value;
}
VERSE_NO_INLINE string Verse::ToCode(const string& s) {
	string Result="\""_VS;
	nat i=0,n=Length(s),Initial;
    char8 ch=0;
	while(i<n) {
		for(Initial=i; i<n && (ch=s[i]) && ch>=0x20 && ch<0x7F && ch!='"' && ch!='{' && ch!='}' && ch!='\\'; i++);
		if(i>Initial)
			Result+=s.Slice(Initial,i);
		if(i<n) {
			string Escape=ch=='\n'? "\\n"_VS: ch=='\r'? "\\r"_VS: ch=='\t'? "\\t"_VS: ch=='"'? "\\\""_VS: ch=='\\'? "\\\\"_VS: ch=='{'? "\\{"_VS: ch=='}'? "\\}"_VS: False;
			if(Length(Escape)!=0)
				Result+=Escape, i++;
			else if(char32 Char32; auto o=ParseUTF8(s,i,Char32)) {
				if(Char32<=0x1F || Char32>=0x7F&&Char32<=0x9F || Char32==0x2028 || Char32==0x2029)
					// Control characters disallowed in Printable.
					Result+=ToString("{",ToCode(Char32),"}"), i+=o;
				else
					Result+=ToString(Char32), i+=o;
			}
            else
                Result+=ToString("{",ToCode(ch),"}"), i++;
		}
	}
	return Result+"\"";
}
VERSE_NO_INLINE string Verse::ToCode(const array<>& as) {
	if(auto so=Cast<string>(as))
		return ToCode(so.Coerce());
	string rs=False;
	for(nat i=0,n=Length(as); i<n; i++) {
		if(i>0)
			rs+=","_VS;
		rs+=ToCode(as(i));
	}
	return "array{"_VS+rs+"}"_VS;
}
string Verse::ToCode(const future<>& fu) {
	if(auto Ao=Cast<any>(fu)) {
		auto a=Ao.Coerce();
		if(auto ro=Cast<rational>(a)) {
			return ToCode(ro.Coerce());
		}
		else if(auto ao=Cast<array<>>(a)) {
			return ToCode(ao.Coerce());
		}
		//else if(auto oo=Cast<any_option<any>>(a)) {
		//  Until we expose box<option<t>> covariantly via opt/set/map<t>, we can't implement this Cast.
		//}
		/*else if(Cast<float32>(a) || Cast<float64>(a)) {
			VERSE_UNIMPLEMENTED;
		}*/
		else if(auto c8=Cast<char8>(a)) {
			return ToCode(c8.Coerce());
		}
		else if(auto c16=Cast<char16>(a)) {
			return ToCode(c16.Coerce());
		}
		else if(auto c32=Cast<char32>(a)) {
			return ToCode(c32.Coerce());
		}
		else if(auto ko=Cast<syntax>(a)) {
			return ToCode(ko.Coerce());
		}
		else if(auto po=Cast<path>(a)) {
			return ToCode(po.Coerce());
		}
		// get family
		// get unbased value
		// if family, it must be a map, so print it as a class using either outermost t{..} or class(t,u,..)
		// for independent outermosts where classes are fully scoped.
		//else return "native"_VS;
		else return "native'"_VS+NativeNameOf(a)+"'"_VS;
	}
	else return ".."_VS;
}

