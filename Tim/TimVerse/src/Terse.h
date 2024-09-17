//==============================================================================================================================================================
// Terse C++ library: futures, transactions, containers, nominals, asynchronous functional logic evaluation.

#pragma once
#pragma warning(push)

// Macros.
#define VERSE_ERR(...)      (::Verse::Internal::PrintError(__FILE__,__LINE__,__FUNCTION__,__VA_ARGS__), VERSE_DEBUG_BREAK, ::Verse::Exit(1))
#define VERSE_UNEXPECTED    VERSE_ERR("unexpected")
#define VERSE_UNIMPLEMENTED VERSE_ERR("unimplemented")
#define VERSE_UNSUPPORTED   VERSE_ERR("parameter mismatch")
#define VERSE_LOCUS         ::Verse::locus{operator""_VS<__FILE__>(),__LINE__,0,__LINE__,0}
#define VERSE_HERE(Tag)     [=]{return ::Verse::error{VERSE_LOCUS,"R00"_VS,19,"Runtime Stalled"_VS,string_fix_me(Tag)};}
#define VERSE_ENSURE(C)     ((C)? (void)0: VERSE_ERR("Failed VERSE_ENSURE(" #C ")"))
#define VERSE_NO_INLINE     __declspec(noinline)
#ifdef  NDEBUG
#define VERSE_ASSERT(C)     (0)
#else
#define VERSE_ASSERT(C)     ((C)? (void)0: VERSE_ERR("Failed VERSE_ASSERT(" #C ")"))
#endif

// Compiler dependent.
#if __clang__
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#pragma clang diagnostic ignored "-Winconsistent-missing-override"
#define VERSE_FORCE_INLINE
#define __has_trivial_destructor __is_trivially_destructible
#else
#pragma warning(disable: 4068 4100 4180 4127 4324 4702)
#define VERSE_FORCE_INLINE  __forceinline
#endif
#if _MSC_VER
#define VERSE_NO_INLINE     __declspec(noinline)
#define VERSE_DEBUG_BREAK   __debugbreak()
extern "C" void __stdcall   __debugbreak();
#endif

// Globals.
inline void* operator new(unsigned long long size,void* Source) {return Source;}
inline void operator delete(void*,void*) {}

// Language feature dependent baggage from std.
namespace Verse {
	template<class...> struct tuple;
}
namespace std {
	template<class tu> struct tuple_size;
	template<int i,class tu> struct tuple_element;
	template<class ...ts> struct std::tuple_size<Verse::tuple<ts...>> {
		static constexpr auto value=sizeof...(ts);
	};
	template<class ...ts> struct std::tuple_size<const Verse::tuple<ts...>> {
		static constexpr auto value=sizeof...(ts);
	};
	template<class t,class ...ts> struct std::tuple_element<0,Verse::tuple<t,ts...>> {
		using type=t;
	};
	template<int i,class t,class ...ts> struct std::tuple_element<i,Verse::tuple<t,ts...>> {
		using type=typename std::tuple_element<i-1,Verse::tuple<ts...>>::type;
	};
	template<int i,class ...ts> struct std::tuple_element<i,const Verse::tuple<ts...>> {
		using type=typename std::tuple_element<i,Verse::tuple<ts...>>::type;
	};
}

namespace Verse {

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Types.

// Basic types.
using nat     = unsigned __int64;
using nat64   = unsigned __int64; using int64 = __int64;     
using nat32   = unsigned long;    using int32 = __int32;     
using nat16   = unsigned __int16; using int16 = __int16;     
using nat8    = unsigned char;    using int8  = signed char; // C++ guarantees char, signed char, unsigned char are distinct.
using char8   = char8_t;
using char16  = char16_t;
using char32  = char32_t;
using float32 = float;
using float64 = double;

// Low-level types.
namespace Internal {
	struct internal_falsity {internal_falsity()=delete; internal_falsity(const internal_falsity&) {}};
}
struct falsity: Internal::internal_falsity {};
template<class t>     struct option;
template<class ...ts> struct tuple;
template<class t>     struct span;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Platform abstraction, low level.

// Exit.
[[noreturn]] void Exit(nat32);

// Platform time.
nat Clock();
void Sleep(nat64 msec);

// Platform math.
nat8 SumCarry(nat8 cf,nat64 a,nat64 b,nat64* r);
nat8 DifferenceBorrow(nat8 bf,nat64 a,nat64 b,nat64* r);
nat64 ProductCarry(nat64 a,nat64 b,nat64* r_hi);
nat64 ProductCarry(int64 a,int64 b,int64* r_hi);
nat64 TruncatingDivisionCarry(nat64 Dh,nat64 Dl,nat64 d,nat64* r);
nat64 CeilLog2(nat64);

// Platform atomics.
nat8 AndAtomic(nat8 volatile*,nat8);
nat64 AndAtomic(nat64 volatile*,nat64);
nat8 OrAtomic(nat8 volatile*,nat8);
nat64 OrAtomic(nat64 volatile*,nat64);
nat64 AddAtomic(nat64 volatile*,int64);
nat8 CompareExchangeAtomic(nat8 volatile* Target,nat8 NewValue,nat8 IfValue);
nat64 CompareExchangeAtomic(nat64 volatile* Target,nat64 NewValue,nat64 IfValue);
void FenceAtomic();
void FenceGlobal();
bool LinkAtomic(nat8 volatile* Target,nat8 NewValue,nat8* IfValue);
bool LinkAtomic(nat64 volatile* Target,nat64 NewValue,nat64* IfValue);
template<class t> bool LinkAtomic(t* volatile* Target,t* NewValue,t** IfValue) {
	return LinkAtomic((nat64*)Target,(nat64)NewValue,(nat64*)IfValue);
}
template<class t> t* CompareExchangeAtomic(t* volatile* Target,t* NewValue,t* IfValue) {
	return (t*)CompareExchangeAtomic((nat64*)Target,(nat64)NewValue,(nat64)IfValue);
}

// Platform logging and errors.
namespace Internal {
	void PrintHelper(const char*);
	void PrintHelper(const char8*);
	void PrintHelper(char*);
	void PrintHelper(char8*);
	void PrintHelper(nat64);
	void PrintHelper(int64);
	void PrintHelper(nat32);
	void PrintHelper(int32);
	void PrintHelper(char);
	void PrintHelper(char8);
	void PrintHelper(bool);
	void PrintHelper(span<char8>);
	void PrintBreak(bool Line);
}
template<bool Line=true>                     void Print() {return Internal::PrintBreak(Line);}
template<bool Line=true,class t,class ...us> void Print(const t& a,const us&... bs);
namespace Internal {
	template<class... ts> void PrintError(const char* Filename,nat Line,const char* Function,const ts&... TS) {
		Print(), Print(Filename,"(",Line,"): ",Function,": ",TS...);
	}
}

// Internal.
namespace Internal {
	struct thread_startup;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Generic helpers.

// General.
template<class t> constexpr void Swap(t& T0,t& T1) {t T2=T0; T0=T1; T1=T2;}
template<class t> constexpr t Exchange(t& T0,const t& T1) {t T2=T0; T0=T1; return T2;}

// Math.
template<class t> constexpr t Abs(const t& a) {if(a>=0) return a; return -a;}
template<class t> constexpr t Sgn(const t& a) {return a>0? 1: a==0? 0: -1;}
template<class t> t Align(t p,nat pow2) {return (t)((nat(p)+pow2-1)&~(pow2-1));}
template<class t,class u> t Pow(t a,u b) {VERSE_ASSERT(b>=0); t r=1; while(b!=0) {if((b&1)!=0) r*=a; a*=a; b/=2;}; return r;}
inline nat CeilPow2(nat a) {a--; a|=a>>1; a|=a>>2; a|=a>>4; a|=a>>8; a|=a>>16; a|=a>>32; return a+1;}
inline float32 Ratio(float32 D,float32 d) {return D/(+0.0f+d);}
inline float64 Ratio(float64 D,float64 d) {return D/(+0.0+d);}
//float32&float64: Ceil,Floor,Round,Trunc; Exp,Pow,Sqrt,Ln,Log; Lerp(a,b,t),Clamp(v,lo,hi);
//   Sin,Cos,Tan,Cot,Sec,Csc; ArcSin,ArcCos,ArcTan,ArcCot,ArcSec,ArcCsc;
//   Sinh,Cosh,Tanh,Coth,Sech,Csch; ArSinh,ArCosh,ArTanh,ArCoth,ArSech;
//   all ensuring f(-0)=f(+0).

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Constructor guidance. We use these instead of static members so constructor deduction guides work.

// Values.
template<class t> const t& declval(); // Reference so no constructor required; only usable in unevaluated contexts.
template<class t,nat n> constexpr nat CountOf(const t(&)[n]) {return n;}

// Type operations.
namespace Internal {
	template<class t>     struct simplify_type_helper;
	template<class ...ts> struct common_type_0;
}
template<class t>     using  simplify_type = typename Internal::simplify_type_helper<t>::type;
template<class ...ts> using  common_type   = typename Internal::common_type_0<simplify_type<ts>...>::type;

// Template index pack management.
template<nat ...is> struct index_sequence {using base=index_sequence;};
namespace Internal {
	template<class ip0,class ip1>   struct merge_index_sequence;
	template<nat... is0,nat... is1> struct merge_index_sequence<index_sequence<is0...>,index_sequence<is1...>>: index_sequence<(is0+0)...,(is1+sizeof...(is0))...> {};
	template<nat n>                 struct index_sequence_helper:    merge_index_sequence<typename index_sequence_helper<n/2>::base,typename index_sequence_helper<n-n/2>::base> {};
	template<>                      struct index_sequence_helper<0>: index_sequence<>  {};
	template<>                      struct index_sequence_helper<1>: index_sequence<0> {};
}
template<nat n> using make_index_sequence = typename Internal::index_sequence_helper<n>::base;

// Emplace variadic elements.
template<class t>                     void EmplaceElements(t* as) {}
template<class t,class p,class... ps> void EmplaceElements(t* TS,const p& P,const ps&... PS) {new(TS)t(P); EmplaceElements(TS+1,PS...);}

// Remove const references.
namespace Internal {
	template<class t> struct remove_const_ref_helper           {using type=t;};
	template<class t> struct remove_const_ref_helper<const t > {using type=t;};
	template<class t> struct remove_const_ref_helper<const t&> {using type=t;};
}
template<class t> using remove_const_ref = typename Internal::remove_const_ref_helper<t>::type;

// Add const references.
namespace Internal {
	template<class t> struct add_const_ref_helper           {using type=const t&;};
	template<class t> struct add_const_ref_helper<const t&> {using type=const t&;};
	template<class t> struct add_const_ref_helper<      t&> {using type=t&;};
}
template<class t> using add_const_ref = typename Internal::add_const_ref_helper<t>::type;

// Type conditions.
namespace Internal {
	template<bool b,class t,class f> struct if_type_helper            {using Value=t;};
	template<       class t,class f> struct if_type_helper<false,t,f> {using Value=f;};
}
template<bool b,class t,class f=void> using if_type = typename Internal::if_type_helper<b,t,f>::Value;

// Remove pointer from a type.
namespace Internal {
	template<class t> t remove_pointer_helper(const volatile t*);
}
template<class t> using remove_pointer = decltype(Internal::remove_pointer_helper(declval<t>()));

// The overload priority trick.
struct priority4{}; struct priority3:priority4{}; struct priority2:priority3{}; struct priority1:priority2{}; struct priority0:priority1{};

// Text from type name.
namespace Internal {
	// This mess avoids std and works around Visual C++ bug in constexpr span<char8> interior pointers to constexpr strings.
	template<class t> struct native_name_helper_0 {
		const char* Result;
#if 1
		constexpr native_name_helper_0(): Result("TODO") {}
#elif __clang__
		constexpr native_name_helper_0(): Result(__PRETTY_FUNCTION__) {}
#else
		constexpr native_name_helper_0(): Result(__FUNCTION__) {}
#endif
	};
	template<class t> constexpr auto native_name_helper_1=native_name_helper_0<t>{};
}
template<class t> constexpr const char* NativeNameOfType=Internal::native_name_helper_1<t>.Result;

// Help with operator->.
template<class t> struct help_member {t Value; const t* operator->() {return &Value;}};
template<class t> requires requires(t T) {T.operator->();} t    HelpMember(const t& Value) {return Value;};
template<class t,class... us> requires(sizeof...(us)==0)   auto HelpMember(const t& Value,const us&...)->help_member<t> {return help_member<t>{Value};};

// Functions.
template<class f,class ...ps> using callable_image = decltype(declval<f>()(declval<ps>()...));

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Low level concepts.

namespace Internal {
	template<class t,class u> struct is_equal_helper          {static const bool Value=false;};
	template<class t        > struct is_equal_helper<t,t>     {static const bool Value=true;};
	template<class t>         struct is_reference_helper      {static constexpr bool Value=false;};
	template<class t>         struct is_reference_helper<t&>  {static constexpr bool Value=true ;};
	template<class t>         struct is_reference_helper<t&&> {static constexpr bool Value=true ;};
	template<class t>         struct is_const_helper          {static constexpr bool Value=false;};
	template<class t>         struct is_const_helper<const t> {static constexpr bool Value=true ;};
	template<class t> void IsDerivedHelper(const volatile t*);
}
template<class t,class u> concept IsEqual          = Internal::is_equal_helper<t,u>::Value;
template<class u,class t> concept IsDerived        = requires(u* U) {Internal::IsDerivedHelper<t>(U);};
template<class t>         concept IsPolymorphic    = __is_polymorphic(t);
template<class t>         concept IsAbstract       = __is_abstract(t);
template<class t>         concept HasDestructor    = !__has_trivial_destructor(t);
template<class t>         concept IsConst          = Internal::is_const_helper<t>::Value;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Ordering.

// Ordering types.
struct dynamic_ordering {
	constexpr explicit dynamic_ordering(bool Equal0): Bits(Equal0*3) {}
	constexpr explicit dynamic_ordering(): Bits(4) {}
	constexpr dynamic_ordering(const dynamic_ordering& O): Bits(O.Bits) {}
	constexpr bool operator==(decltype(nullptr)) const {return Bits==3;}
	constexpr bool IsIncomparable()              const {return Bits==4;}
private:
	nat8 Bits;
	constexpr explicit dynamic_ordering(decltype(nullptr),nat8 Bits0): Bits(Bits0) {}
	friend struct equality_ordering;
	friend struct partial_ordering;
};
struct equality_ordering: dynamic_ordering {
	constexpr explicit equality_ordering(bool Equal0):
		dynamic_ordering(nullptr,Equal0*3) {}
	constexpr equality_ordering(const equality_ordering& Other):
		dynamic_ordering(Other) {}
	constexpr bool IsUndecidable() {return false;}
private:
	constexpr explicit equality_ordering(decltype(nullptr),nat8 Bits0):
		dynamic_ordering(nullptr,Bits0) {}
	friend struct partial_ordering;
};
struct partial_ordering: equality_ordering {
	constexpr partial_ordering(const partial_ordering& Other):
		equality_ordering(nullptr,Other.Bits) {}
	constexpr partial_ordering(bool LessOrEqual,bool GreaterOrEqual):
		equality_ordering(nullptr,LessOrEqual+GreaterOrEqual*2) {}
	constexpr bool operator< (decltype(nullptr)) const {return Bits==1;}
	constexpr bool operator<=(decltype(nullptr)) const {return Bits&1;}
	constexpr bool operator> (decltype(nullptr)) const {return Bits==2;}
	constexpr bool operator>=(decltype(nullptr)) const {return Bits&2;}
};
struct total_ordering: partial_ordering {
	constexpr total_ordering(bool LessOrEqual,bool GreaterOrEqual):
		partial_ordering(LessOrEqual,GreaterOrEqual) {VERSE_ASSERT(LessOrEqual||GreaterOrEqual);}
};

// Produce common ordering, e.g. ordering of a tensor product of values with different orderings.
template<class t,class u> using common_ordering = 
	if_type<IsEqual<t,dynamic_ordering>||IsEqual<u,dynamic_ordering>,dynamic_ordering,
		if_type<IsEqual<t,equality_ordering>||IsEqual<u,equality_ordering>,equality_ordering,
			if_type<IsEqual<t,partial_ordering>||IsEqual<u,partial_ordering>,partial_ordering,
				total_ordering
			>
		>
	>;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Low-level hashing.

namespace Internal {
	constexpr nat HashPayload = 0;
	constexpr nat HashFalse   = 1;
	constexpr nat HashInteger = 0x9e3779b97f4a7c15;
	constexpr nat HashNext    = 0x7E391A873519BCD1;
	constexpr nat HashKey     = 0xB7E0A4D74130D20F;
}

//==============================================================================================================================================================
// Low level containers.

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Options.

namespace Internal {
	struct construct_cast {};
	struct construct_truth {};
}
template<class target,class source> bool Encase(target* Target,const source& Source,bool* Finished=nullptr);
template<class t> struct option {
	constexpr option()                {this->Exists=0;}
	constexpr option(const tuple<>&)  {this->Exists=0;}
	constexpr option(const option& o) {this->Exists=0; if(o.Exists) new(&this->T)t(o.T), this->Exists=1;}
	template<class u> requires requires(u U) {t(U);} constexpr option(const option<u>& o) {this->Exists=0; if(o) new(&this->T)t(o.T), this->Exists=1;}
	template<class=void> requires(IsEqual<t,tuple<>>) constexpr option(bool b)             {this->Exists=b;}
	template<class source> constexpr option(Internal::construct_cast,const source& Source,bool* Finished=nullptr) {this->Exists=Encase(&this->T,Source,Finished);}
	constexpr ~option() {if(Exists) T.~t();}
	constexpr ~option() requires(!HasDestructor<t>)=default;

	constexpr explicit     operator bool    () const                                 {return this->Exists;}
	constexpr auto         operator->       () const                                 {VERSE_ENSURE(this->Exists); return HelpMember(this->T);}
	constexpr option&      operator=        (const option& o)                        {if(this->Exists) {if(o.Exists) this->T=o.T; else this->Exists=0,this->T.~t();} else if(o.Exists) this->Exists=1,new(&this->T)t(o.T); return *this;}
	constexpr t            Coerce           (const char* Bad="option::Coerce") const {if(this->Exists) return this->T; VERSE_ERR(Bad);}
	constexpr t            Else             (const t& Other) const                   {if(this->Exists) return this->T; return Other;}
	constexpr option<t>    ElseIf           (const option<t>& Other) const           {if(this->Exists) return *this; return Other;}
	constexpr t            Presume          () const                                 {VERSE_ASSERT(this->Exists); return this->T;}
	constexpr const t&     PresumeReference () const                                 {VERSE_ASSERT(this->Exists); return this->T;}
	constexpr t&           PresumeReference ()                                       {VERSE_ASSERT(this->Exists); return this->T;}

private:
	union {t T;};
	bool Exists;
	constexpr option(Internal::construct_truth,const t& T0): T(T0), Exists(true) {}
	template<class u> friend constexpr option<u> Truth(const u& a);
	template<class u> friend struct option;
};
template<class t> constexpr option<t> Truth(const t& T) {return option<t>(Internal::construct_truth{},T);}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Low level arrays of various sorts.

// Span defined by pairs of pointers.
constexpr nat ExposeLength(const char8* S);
template<class t> struct span {
	const t *Begin,*End;
	explicit constexpr span(decltype(nullptr)=nullptr): Begin(nullptr), End(nullptr       ) {}
	constexpr span(const t* Begin0,nat      Length0  ): Begin(Begin0 ), End(Begin0+Length0) {}
	constexpr span(const t* Begin0,const t* End0     ): Begin(Begin0 ), End(End0          ) {}
	template<class=void> requires(IsEqual<t,char8>) explicit constexpr span(const char8* S): span(S,ExposeLength(S)) {}
	template<class=void> requires(IsEqual<t,char8>) explicit span(const char* S): span((const char8*)S) {}
	constexpr const t& operator[](nat i) const {VERSE_ASSERT(Begin+i<End); return Begin[i];}
};		
template<class t> span(t*,nat)->span<t>;
template<class t> span(t*,t*)->span<t>;
template<class t> span(const t*,nat)->span<t>;
template<class t> span(const t*,t*)->span<t>;

// Contiguous range span.
struct range_span {nat RangeCount;};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Sets of enumerations.

// Assist with sets of enums. Must provide comparisons because nat8/nat16/nat32/nat64 conversion dominates template.
nat ExposeHash(nat);
#define VERSE_ENUM_LATTICE_OPEN(t,n) \
	inline constexpr partial_ordering ExposeCompare(t a,t b) {auto ab=n(a)|n(b); return partial_ordering(ab==n(b),ab==n(a));} \
	inline nat ExposeHash(t a) {return ExposeHash(nat(a));} \
	inline constexpr bool operator==(t  a,t b) {return ExposeCompare(a,b)==nullptr;} \
	inline constexpr bool operator<=(t  a,t b) {return ExposeCompare(a,b)<=nullptr;} \
	inline constexpr bool operator< (t  a,t b) {return ExposeCompare(a,b)< nullptr;} \
	inline constexpr bool operator>=(t  a,t b) {return ExposeCompare(a,b)>=nullptr;} \
	inline constexpr bool operator> (t  a,t b) {return ExposeCompare(a,b)> nullptr;} \
	inline constexpr t    operator& (t  a,t b) {return t(n(a)&n(b));} \
	inline constexpr t    operator+ (t  a,t b) {return t(n(a)|n(b));} \
	inline constexpr t&   operator&=(t& a,t b) {return a=t(n(a)&n(b));} \
	inline constexpr t&   operator+=(t& a,t b) {return a=t(n(a)|n(b));} \
	inline constexpr bool operator! (t  a    ) {return !n(a);}
#define VERSE_ENUM_LATTICE_CLOSED(t,n) \
	VERSE_ENUM_LATTICE_OPEN(t,n) \
	inline constexpr t    operator~ (t  a    ) {return t(~n(a));}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Tuples.

// Tuple type.
template<class ...ts> struct tuple;
namespace Internal {
	struct construct_tuple {};
	template<nat i> struct construct_tuple_function {};
}
template<> struct tuple<> {
	inline explicit constexpr operator bool() const {return 0;}
	inline constexpr tuple()=default;
	template<class ...us> auto operator+(const tuple<us...>& bs) const {return bs;}
	template<nat i> falsity get() const;
	template<nat i> falsity& get_reference() const;
	// Somehow having a non-default-constructor here breaks casting, just in Visual C++!
	//template<nat i,class f> tuple(Internal::construct_tuple_function<i>,const f& F) {}
};
template<class t,class ...ts> struct tuple<t,ts...> {
	tuple(const t& a,const ts&... bs): Head(a), Tails(bs...) {}
	template<class=void> requires requires{t(); tuple<ts...>();} tuple(): Head(), Tails() {}
	template<class u,class... us> requires requires(u U,tuple<us...> US) {t(U); tuple<ts...>(US);} tuple(const tuple<u,us...>& tus):
		Head(tus.Head), Tails(tus.Tails) {}
	explicit constexpr operator bool() const {return true;}
	template<class ...us> auto operator+(const tuple<us...>& bs) const {
		return tuple<t,ts...,us...>(Internal::construct_tuple{},Head,Tails+bs);
	}
	template<nat i> auto get() const {
		if constexpr(i==0)
			return Head;
		else
			return Tails.template get<i-1>();
	}
	template<nat i> auto& get_reference() {
		if constexpr(i==0)
			return Head;
		else
			return Tails.template get_reference<i-1>();
	}
	//template<nat i,class f> tuple(Internal::construct_tuple_function<i>,const f& F):
	//	Head(F.template operator()<i,t>()), Tails(construct_tuple_function<f,i+1>) {}
	template<class... us> friend struct tuple;
private:
	t            Head;
	tuple<ts...> Tails;
	tuple(Internal::construct_tuple,const t& a,const tuple<ts...>& bu): Head(a), Tails(bu) {}
};
template<class... ts> tuple(const ts&...)->tuple<ts...>;

// Build a tuple from a template lambda.
namespace Internal {
	template<class f,class tu,class is> struct tuple_from_function;
	template<class f,class... ts,nat... is> struct tuple_from_function<f,tuple<ts...>,index_sequence<is...>> {
		template<nat i> static auto TupleFromFunctionElement(const f& F) {
			return F.template operator()<i>();
		}
		static tuple<ts...> TupleFromFunctionElement(const f& F) {
			return tuple<ts...>{TupleFromFunctionElement<is>(F)...};
		}
	};
	template<nat n,class f,class tu>    struct tuple_typer;
	template<      class f,class... ts> struct tuple_typer<0,f,tuple<ts...>> {using type=tuple<ts...>;};
	//template<nat n,class f,class... ts> struct tuple_typer<n,f,tuple<ts...>> {using type=tuple<decltype(declval<f>().template operator()<n-1>()),ts...>;};
	template<nat n,class f,class... ts> struct tuple_typer<n,f,tuple<ts...>> {using type=typename tuple_typer<n-1,f,tuple<decltype(declval<f>().template operator()<n-1>()),ts...>>::type;};
}
template<nat n,class f> auto StaticFor(const f& F) {
	// Can do inductively without index sequence stuff once some Visual C++ compiler bugs are fixed.
	using tu=typename Internal::tuple_typer<n,f,tuple<>>::type;
	return Internal::tuple_from_function<f,tu,make_index_sequence<std::tuple_size<tu>::value>>::TupleFromFunctionElement(F);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Logic.

using logic = option<tuple<>>;
constexpr tuple<> False;
constexpr const logic True = Truth(False);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Singletons.

template<class t,t v> struct singleton {};

//==============================================================================================================================================================
// High-Level Library.

// Box types.
struct any;
struct real;
struct rational;
struct integer;
struct natural;
struct comparable;
template<class=any>                       struct future;
template<class=struct managed>            struct box;
template<class=future<>(const falsity&)>  struct function;
template<class r,class... ps>             struct function<r(ps...)>;
template<class=comparable,class=future<>> struct map;
template<class=comparable>                struct bag;
template<class=future<>>                  struct array;
template<class>                           struct pin; // Default future<> confuses Visual C++ about pin CTAD.
template<class=future<>>                  struct var;
using string        = array<char8>;
using string8       = array<char8>;
using string16      = array<char16>;
using string32      = array<char32>;
using string_fix_me = string;

// Managed types.
struct managed;
struct managed_ptr;
struct suspension_managed;
struct when_future_suspension;
struct context_managed;
struct iterate_managed;
template<class=future<>(const falsity&)>  struct managed_function;
template<class=future<>>                  struct managed_array;
template<class>                           struct managed_array_flat;
template<class=comparable,class=future<>> struct managed_map;
template<class,class,class>               struct managed_mutable_map;
template<class>                           struct managed_value;
using suspension = box<suspension_managed>;
using context    = box<context_managed>;
using iterate    = box<iterate_managed>;

// Never defined.
template<> struct box<void>;
template<> struct array<void>;

// Constructor tags.
struct construct_constexpr   {};
struct construct_payload     {};
struct construct_no_init     {};
struct construct_exposed     {};
struct construct_value_copy  {};
struct construct_flat        {};
struct construct_function    {};
struct construct_range       {};
struct construct_elements    {};
struct construct_new_var     {};
template<class t> struct construct_boxer {};

// Other types.
struct path;
struct locus;
struct error;
struct step;
struct current_step;
struct syntax;
template<class> struct expose;
struct expose_const   {};
struct expose_mutable {};

// Expose by reference.
expose_mutable ExposeUnique(const suspension_managed&);
expose_mutable ExposeUnique(const iterate_managed&);
template<class k,class v,class entry> expose_mutable ExposeUnique(const managed_mutable_map<k,v,entry>&);

// Internal.
namespace Internal {
	struct integer_local;
	struct integer_immediate {};
	struct ptr_immediate {};
	template<class k>         struct mutable_set_entry;
	template<class k,class v> struct mutable_map_entry;
	void PrintHelper(const future<>&);
	void PrintHelper(const string&);
	void PrintError(const char* Filename,nat Line,const char* Function,const error& Error);
}

// String literals.
template<nat n> struct string_literal {
	char8 String[n];
	constexpr string_literal(const char (&String0)[n]) {
		for(nat i=0; i<n-1; ++i)
			String[i]=String0[i];
		String[n-1]=0;
	}
	constexpr string_literal(const char8 (&String0)[n]) {
		for(nat i=0; i<n-1; ++i)
			String[i]=String0[i];
		String[n-1]=0;
	}
};
template<string_literal Literal> string operator ""_VS();
template<string_literal Literal> path   operator ""_VP();

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Managed type traits.

// Type exposure for type casting and type property extraction.
template<class t> struct default_expose {

	// Properties.
	static constexpr bool Any=false,Copy=false,Box=false,Pin=false,Future=false;

	// Casting only considers input in its current state, and synchronously returns success or failure.
	// Therefore a Cast that succeeds now cannot later fail, but a Cast that fails now could succeed later due to futures resolving.
	static bool ExposeCast(t* Target,const pin<any>& Source,bool* Finished);

	// Falls back to synchronous casting if not overridden.
	static void ExposeCastSuspends(const any& a);

	// Detect comparability.
	static constexpr bool ExposeComparable();
};
template<class t> struct expose: default_expose<t> {};

// Expose built-in integer and natural number types.
template<class t> struct expose_int: default_expose<t> {
	static constexpr t Min() {return t(nat(-1)/2+1);}
	static constexpr t Max() {return t(nat(-1)/2);}
	static bool ExposeCast(t* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature();
};
template<class t> struct expose_nat: default_expose<t> {
	static constexpr t Min() {return t(0 );}
	static constexpr t Max() {return t(nat(-1));}
	static bool ExposeCast(t* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature();
};

// Comparability. !!want to make it a concept but there are currently C++ compiler order or circularity issues
template<class t> constexpr bool IsComparable() {
	return expose<t>::ExposeComparable(); // Are all t comparable statically?
}
template<class t> bool IsComparableDynamic(const t& T);
bool IsComparableDynamic(const pin<any>& T);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Expose basic and non-future<> derived types.

// falsity.
template<> struct expose<falsity>: default_expose<falsity> {
	static bool ExposeCast(falsity* Target,const pin<any>& Source,bool* Finished);
};
inline constexpr total_ordering ExposeCompare(const falsity&,const falsity&) {return total_ordering{true,true};}
inline nat ExposeHash(const falsity&) {VERSE_UNEXPECTED;}

//
// Character types.
//

// char.
template<> struct expose<char>: default_expose<char> {
	static bool ExposeCast(char* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature(); 
};
comparable ExposeValue(char); // Needed, else ExposeValue(char) resolves to ExposeValue(int32).
constexpr total_ordering ExposeCompare(char a,char b) {return{char8(a)<=char8(b),char8(a)>=char8(b)};}
nat ExposeHash(char);

// char8.
template<> struct expose<char8>: default_expose<char8> {
	static bool ExposeCast(char8* Target,const pin<any>& Source,bool* Finished);
	//static path ExposeStaticSignature(); 
};
comparable ExposeValue(char8);
constexpr total_ordering ExposeCompare(char8 a,char8 b ) {return{a<=b,a>=b};}
nat ExposeHash(char8);

// char16.
template<> struct expose<char16>: default_expose<char16> {
	static bool ExposeCast(char16* Target,const pin<any>& Source,bool* Finished);
	//static path ExposeStaticSignature(); 
};
comparable ExposeValue(char16);
constexpr total_ordering ExposeCompare(char16 a,char16 b) {return{a<=b,a>=b};}
nat ExposeHash(char16);

// char32.
template<> struct expose<char32>: default_expose<char32> {
	//static path ExposeStaticSignature();
};
constexpr total_ordering ExposeCompare(char32 a,char32 b) {return{a<=b,a>=b};}
nat ExposeHash(char32);

//
// Numeric types.
//

template<> struct expose<nat8 >: expose_nat<nat8 > {};
template<> struct expose<int8 >: expose_int<int8 > {};
template<> struct expose<nat16>: expose_nat<nat16> {};
template<> struct expose<int16>: expose_int<int16> {};

// nat32. Overloads needed to avoid C++ conversion issues not captured by nat64.
template<> struct expose<nat32>: expose_nat<nat32> {};
natural ExposeValue(nat32);
constexpr total_ordering ExposeCompare(nat32 a,nat32 b) {return{a<=b,a>=b};}
nat ExposeHash(nat32);

// int32. Overloads needed to avoid C++ conversion issues not captured by int64.
template<> struct expose<int32>: expose_int<int32> {};
integer ExposeValue(int32);
constexpr total_ordering ExposeCompare(int32 a,int32 b) {return{a<=b,a>=b};}
nat ExposeHash(int32);

// nat64.
template<> struct expose<nat64>: expose_nat<nat64> {};
natural ExposeValue(nat64);
constexpr total_ordering ExposeCompare(nat64 a,nat64 b) {return{a<=b,a>=b};}
nat ExposeHash(nat64);

// int64.
template<> struct expose<int64>: expose_int<int64> {};
integer ExposeValue(int64);
constexpr total_ordering ExposeCompare(int64 a,int64 b) {return{a<=b,a>=b};}
nat ExposeHash(int64);

// float32.
constexpr partial_ordering ExposeCompare(float32 a,float32 b) {if(a==a||b==b) return{a<=b,a>=b}; return{true,true};}
inline nat ExposeHash(float32 c) {float32 d=+0.0f+c; return ExposeHash(reinterpret_cast<const nat32&>(d));}
template<> struct expose<float32>: default_expose<float32> {
	static path ExposeStaticSignature(); 
};

// float64.
constexpr partial_ordering ExposeCompare(float64 a,float64 b) {if(a==a||b==b) return{a<=b,a>=b}; return{true,true};}
inline nat ExposeHash(float64 c) {float64 d=+0.0+c; return ExposeHash(reinterpret_cast<const nat64&>(d));}
template<> struct expose<float64>: default_expose<float64> {
	static path ExposeStaticSignature(); 
};

//
// Container types.
//

// bool.
template<> struct expose<bool>: default_expose<bool> {
	static bool ExposeCast(bool* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature();
};
box<option<tuple<>>> ExposeValue(bool); //!!better if bag<tuple<>>?

// option<t>.
template<class t> struct expose<option<t>>: default_expose<option<t>> {
	static bool ExposeCast(option<t>* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature();
};
template<class t> nat ExposeHash(const option<t>& O);
template<class t> auto ExposeCallable (const option<t>& TO) {return [&](const t&)->t {VERSE_UNIMPLEMENTED;};}
template<class t> nat  ExposeLength   (const option<t>& TO) {return nat(bool(TO));}
template<class t> void ExposeSet      (const option<t>&);
template<class t> array<t> ExposeKeys (const option<t>&);

// tuple<ts...>.
template<class... ts> struct expose<tuple<ts...>>: default_expose<tuple<ts...>> {
	static path ExposeStaticSignature();
	static tuple<ts...> ExposeCoerce(const pin<any>& Source);
	static bool ExposeCast(tuple<ts...>* Target,const pin<any>& Source,bool* Finished);
	template<nat i> static void ExposeCastSuspendsHelper(const array<>& AS);
	static void ExposeCastSuspends(const any& Source);
};
template<class... ts> nat ExposeHash(const tuple<ts...>& T);
namespace Internal {
	template<class... ts> struct tuple_commonizer {
		using ct=common_type<ts...>;
		ct (*Commonize[sizeof...(ts)+1])(const tuple<ts...>&);
		template<nat i> constexpr void Init() {
			Commonize[i]=[](const tuple<ts...>& T)->ct {
				if constexpr(i<sizeof...(ts))
					return T.template get<i>();
				else
					VERSE_UNEXPECTED;
			};
			if constexpr(i<sizeof...(ts))
				Init<i+1>();
		}
		constexpr tuple_commonizer() {
			Init<0>();
		}
	};
	template<class... ts> constexpr tuple_commonizer<ts...> TupleCommonizer;
}
template<class... ts> auto ExposeCallable(const tuple<ts...>& TS) {
	return [&TS](nat i) {
		VERSE_ENSURE(i<sizeof...(ts));
		return Internal::TupleCommonizer<ts...>.Commonize[i](TS);
	};
}
template<class ...ts> constexpr nat ExposeLength(const tuple<ts...>&) {return sizeof...(ts);}
box<tuple<>> ExposeValue(tuple<>);

// span<t>.
template<class t> struct expose<span<t>>: default_expose<span<t>> {
	static path ExposeStaticSignature();
};
template<class t> array<t> ExposeValue(span<t>);
template<class t> constexpr auto ExposeCallable(const span<t>& TS) {return [&TS](nat i) {return TS[i];};}
template<class t> constexpr nat  ExposeLength(span<t> a) {return nat(a.End-a.Begin);}

// range_span.
template<> struct expose<range_span>: default_expose<range_span> {
	static path ExposeStaticSignature();
};
constexpr auto ExposeCallable(const range_span& S) {return [&](nat i)->nat {VERSE_ENSURE(i<S.RangeCount); return i;};}
constexpr nat  ExposeLength(const range_span& S) {return S.RangeCount;}

// char*, char8*.
constexpr total_ordering ExposeCompare(const char8* as,const char8* bs) {nat i=0; while((bs[i])&(as[i]==bs[i])) i++; return ExposeCompare(as[i],bs[i]);}
constexpr total_ordering ExposeCompare(const char*  as,const char*  bs) {return ExposeCompare((char8*)as,(char8*)bs);}
void ExposeValue(const char*);  // Use static "Hello"_VS or dynamic explicit string(const char*).
void ExposeValue(const char8*); // Use static "Hello"_VS or dynamic explicit string(const char8*).
constexpr auto  ExposeCallable(const char8* S) {return [S](nat i)->char8 {return S[i];};}
constexpr auto  ExposeCallable(const char*  S) {return [S](nat i)->char8 {return S[i];};}
constexpr nat   ExposeLength(const char8* S) {nat i=0; while(S[i]) i++; return i;}
constexpr nat   ExposeLength(const char*  S) {nat i=0; while(S[i]) i++; return i;}

// Managed types.
inline equality_ordering ExposeCompare(const Internal::integer_immediate& A,const Internal::integer_immediate& B) {VERSE_UNEXPECTED;}
inline nat ExposeHash(const Internal::integer_immediate& A) {VERSE_UNEXPECTED;}
path ExposeSignature(Internal::integer_immediate);

inline equality_ordering ExposeCompare(const Internal::ptr_immediate& A,const Internal::ptr_immediate& B) {VERSE_UNEXPECTED;}
inline nat ExposeHash(const Internal::ptr_immediate& A) {VERSE_UNEXPECTED;}
path ExposeSignature(Internal::ptr_immediate);

inline equality_ordering ExposeCompare(const managed* P,const managed* Q) {return equality_ordering{P==Q};}
nat ExposeHash(const managed*);

// Singletons.
template<class t,t T> auto ExposeValue(singleton<t,T>)->decltype(ToAny(T)) {
	return ToAny(T);
}
template<class t,t T> struct expose<singleton<t,T>>: default_expose<singleton<t,T>> {
	static bool ExposeCast(singleton<t,T>* Target,const pin<any>& Source,bool* Finished);
	static path ExposeStaticSignature(); 
};

// Block exposure where it would cause confusion or break container constructors.
void ExposeValue(construct_payload);
void ExposeValue(construct_no_init);
void ExposeValue(construct_value_copy);
void ExposeValue(construct_flat);
void ExposeValue(construct_range);
void ExposeValue(construct_function);
void ExposeValue(decltype(nullptr));

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Expose future<> derived types.
// We specify ExposeComparable explicitly to avoid dependence on future<>-derived constructors and conversions.

// future<>.
template<> struct expose<future<>>: default_expose<future<>> {
	static constexpr bool Any=true,Future=true;
	static bool ExposeCast(future<>* Target,const pin<any>& Source,bool* Finished);
	static void ExposeCastSuspends(const any& a) {}
	static constexpr bool ExposeComparable() {return false;}
};

// future<t>.
template<class t> struct expose<future<t>>: default_expose<future<t>> {
	static constexpr bool Any=true,Future=true;
	static bool ExposeCast(future<t>* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return false;}
};

// any.
template<> struct expose<any>: default_expose<any> {
	static constexpr bool Any=true;
	static bool ExposeCast(any* Target,const pin<any>& Source,bool* Finished);
	static void ExposeCastSuspends(const any& a) {}
	static constexpr bool ExposeComparable() {return false;}
};

// comparable.
template<> struct expose<comparable>: default_expose<comparable> {
	static constexpr bool Any=true;
	static bool ExposeCast(comparable* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return true;}
};

// var<t>.
template<class t> struct expose<var<t>>: default_expose<var<t>> {
	static constexpr bool Any=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(var<t>* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return true;}
};

// box<t>.
template<class t> struct expose<box<t>>: default_expose<box<t>> {
	static constexpr bool Any=true,Copy=true,Box=true;
	static bool ExposeCast(box<t>* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return IsComparable<t>();}
};

// real.
template<> struct expose<real>: default_expose<real> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(real* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return false;}
};

// rational.
template<> struct expose<rational>: default_expose<rational> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(rational* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return true;}
};

// integer.
template<> struct expose<integer>: default_expose<integer> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(integer* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return true;}
};

// natural.
template<> struct expose<natural>: default_expose<natural> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(natural* Target,const pin<any>& Source,bool* Finished);
	static constexpr bool ExposeComparable() {return true;}
};

// function<t>, step, current_step.
template<class t> struct expose<function<t>>: expose<box<managed_function<t>>> {
	static path ExposeStaticSignature();
	static constexpr bool ExposeComparable() {return false;}
};
template<> struct expose<step>: expose<function<current_step()>> {
	static constexpr bool ExposeComparable() {return false;}
};
template<> struct expose<current_step>: expose<function<current_step()>> {
	static constexpr bool ExposeComparable() {return false;}
};

// map<k,v>.
template<class k,class v> struct expose<map<k,v>>: default_expose<map<k,v>> {
	static constexpr bool Any=true,Copy=true;
	//ExposeCast
	static path ExposeStaticSignature();
	static void ExposeCastSuspends(const any& Source);
	static constexpr bool ExposeComparable() {return IsComparable<v>();}
};

// bag<t>.
template<class t> struct expose<bag<t>>: default_expose<bag<t>> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	//ExposeCast
	static void ExposeCastSuspends(const any& Source);
	static constexpr bool ExposeComparable() {return IsComparable<t>();}
};

// array<>.
template<> struct expose<array<>>: default_expose<array<>> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(array<>* Target,const pin<any>& Source,bool* Finished);
	static void ExposeCastSuspends(const any& Source);
	static constexpr bool ExposeComparable() {return false;}
};

// array<t>.
template<class t> struct expose<array<t>>: default_expose<array<t>> {
	static constexpr bool Any=true,Copy=true;
	static path ExposeStaticSignature();
	static bool ExposeCast(array<t>* Target,const pin<any>& Source,bool* Finished);
	static void ExposeCastSuspends(const any& Source);
	static constexpr bool ExposeComparable() {return IsComparable<t>();}
};

// pin<t>.
template<class t> struct expose<pin<t>>: expose<t> {
	static constexpr bool Pin=true;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Exposing types.

// Unique (mutable through box<t>) type detection.
namespace Internal {
	template<class t> struct remove_future_helper            {using Result=t;};
	template<class t> struct remove_future_helper<future<t>> {using Result=typename remove_future_helper<t>::Result;};

	template<class t> concept HasExposeIsComparable = requires(t T) {bool(ExposeIsComparable(T));};
	template<class t> concept HasExposeHash         = requires(t T) {bool(ExposeHash(T));};
};
template<class t> using remove_future = typename Internal::remove_future_helper<t>::Result;

// Comes after all declarations of expose in this namespace. Internalize.
template<class t> concept IsUnique              = requires(t T) {ExposeUnique(T);};
#if 0
template<class t> concept IsUniqueMutable       = requires(t T) {expose_mutable(ExposeUnique(T));};
#else
namespace Internal {
	constexpr bool IsUniqueMutableHelper(const volatile void*) {return false;}
	template<class t,class=decltype(expose_mutable(ExposeUnique(declval<t>())))> constexpr bool IsUniqueMutableHelper(const volatile t*) {return true;}
};
template<class t> concept IsUniqueMutable       = Internal::IsUniqueMutableHelper(static_cast<const t*>(nullptr));
#endif
template<class t> concept HasStaticSignature    = requires {path(expose<t>::ExposeStaticSignature());};
template<class t> concept IsExplicit            = requires(t T) {ExposeExplicit(T);};
template<class t> concept IsAny                 = expose<t>::Any;    // If t is derived from future<>.
template<class t> concept IsAnyCopyable         = expose<t>::Copy;   // If all t are copyable.
template<class t> concept IsBox                 = expose<t>::Box;    // Is box<t>, possibly pinned.
template<class t> concept IsFuture              = expose<t>::Future; // Is future<t>, possibly pinned.
template<class t> concept IsPin                 = expose<t>::Pin;    // Is pin<t>.

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Constructing managed types using ExposeValue(t) if present, else box<t>.

// Override to expose a value of type t to Verse specially, else it will be exposed as box<t>. Expose as void to hide.
template<class t> requires(IsAny<t>) t                  ExposeValue(const t& T) {return T;}
template<class r,class ...ps>        function<r(ps...)> ExposeValue(r F(ps...)) {return F;}

// Get value as an instance of a subclass of future<>.
template<class t> auto ToAny(const t& T)->decltype(ExposeValue(T)) {return ExposeValue(T);}
template<class t,class... ts> requires(sizeof...(ts)==0) box<t> ToAny(const t& T,const ts&...) {return box<t>(T);}
template<class t> using   exposed_type = decltype(ToAny(declval<t>()));
template<class t> concept IsExposed    = !IsEqual<exposed_type<t>,void>;

// Prohibited instantiations. !!see if we can move any to concepts
template<class t> requires IsEqual<t,void> || IsFuture<t>               struct future<future<t>>;
template<class t> requires IsEqual<t,void> || IsAny<t>                  struct box<t>;
template<class t> requires (!IsAny<t>)                                  struct pin<t>;
template<class k>                                                       struct map<k,void>;
template<class k,class v> requires(!IsComparable<k> || IsEqual<v,void>) struct map<k,v>;
template<class t> requires IsEqual<t,void>                              struct array<t>;
template<class t> requires(!IsComparable<t>)                            struct bag<t>;

// Deduction guides. Moved here to satisfy clang.
template<class t> requires IsEqual<box<t>,exposed_type<t>> box(const t&)->box<t>;
template<class t> pin(const t&)->pin<exposed_type<t>>;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Comparison.

// Definition of forward-declared default_expose<t>::ExposeComparable.
namespace Internal {
#if 0
	template<class t> requires requires(t T) {ExposeCompare(T,T);} constexpr bool ComparableHelper(const volatile t*,priority0) {return true;}
	template<class t> requires IsUnique<t> constexpr bool ComparableHelper(const volatile t*,priority1) {return true;}
#else
	template<class t,class=decltype(ExposeCompare(declval<t>(),declval<t>()))> constexpr bool ComparableHelper(const volatile t*,priority0) {return true;}
	template<class t,class=decltype(ExposeUnique(declval<t>()))> constexpr bool ComparableHelper(const volatile t*,priority1) {return true;}
#endif
	template<class... ps,class r> constexpr bool ComparableHelper(r(*)(ps...),priority2) {return false;}
	constexpr bool ComparableHelper(const volatile void*,priority3) {return false;}
}
template<class t> constexpr bool default_expose<t>::ExposeComparable() {
	return Internal::ComparableHelper(static_cast<const t*>(nullptr),priority0{});
}

//
// CompareDynamic for dynamically typed non-suspending comparison; may produce IsIncomparable ordering.
//

// ExposeCompare(t,u) implies CompareDynamic(t,u).
template<class t,class u> constexpr auto CompareDynamic(const t& a,const u& b)->decltype(ExposeCompare(a,b)) {return ExposeCompare(a,b);}
template<class t,class u> auto CompareDynamic(const box<t>& a,const box<u>& b)->decltype(ExposeCompare(declval<t>(),declval<u>())) {
	return ExposeCompare(*a,*b);
}

// ExposeUnique(t) implies equality_ordering CompareDynamic(t,t) providing by-reference equality.
#if 0
template<class t,class u> requires IsUnique<t> && IsUnique<u> 
#else
template<class t,class u,class=decltype((ExposeUnique(declval<t>()),ExposeUnique(declval<u>())))>
#endif
equality_ordering CompareDynamic(const box<t>& T,const box<u>& U);

// Optimized CompareDynamic for future<> subtypes. Rework to ensure unambiguous overload resolution.
dynamic_ordering  CompareDynamic(const future<>& a,const future<>& b);
dynamic_ordering  CompareDynamic(const any& a,const any& b);
equality_ordering CompareDynamic(const comparable& a,const comparable& b);
total_ordering    CompareDynamic(const rational& a,const rational& b);
total_ordering    CompareDynamic(const integer&,const integer&);
total_ordering    CompareDynamic(const string&,const char*);
total_ordering    CompareDynamic(const string&,const char8_t*);
template<class t,class u> equality_ordering CompareDynamic(const var<t>& TP,const var<u>& UP);

//
// Statically safe comparison.
//

// Ordering concepts.
template<class t,class u> concept IsTotalOrdering    = requires(t T,u U) {total_ordering   (CompareDynamic(T,U));};
template<class t,class u> concept IsPartialOrdering  = requires(t T,u U) {partial_ordering (CompareDynamic(T,U));};
template<class t,class u> concept IsEqualityOrdering = requires(t T,u U) {equality_ordering(CompareDynamic(T,U));};

// Ordering comparison functions.
template<class t,class u> requires IsTotalOrdering<t,u>    constexpr total_ordering    CompareTotal   (const t& a,const u& b) {return CompareDynamic(a,b);}
template<class t,class u> requires IsPartialOrdering<t,u>  constexpr partial_ordering  ComparePartial (const t& a,const u& b) {return CompareDynamic(a,b);}
template<class t,class u> requires IsEqualityOrdering<t,u> constexpr equality_ordering CompareEquality(const t& a,const u& b) {return CompareDynamic(a,b);}

// CompareDynamic being at least equality_ordering implies operator==, and automatically operator!=.
template<class t,class u> constexpr auto operator==(const t& a,const u& b)->decltype(equality_ordering(CompareDynamic(a,b))==nullptr) {return CompareDynamic(a,b)==nullptr;}

// CompareDynamic being at least partial_ordering implies ordered comparison operators.
template<class t,class u> constexpr auto operator<=(const t& a,const u& b)->decltype(partial_ordering(CompareDynamic(a,b))<=nullptr) {return CompareDynamic(a,b)<=nullptr;}
template<class t,class u> constexpr auto operator< (const t& a,const u& b)->decltype(partial_ordering(CompareDynamic(a,b))< nullptr) {return CompareDynamic(a,b)< nullptr;}
template<class t,class u> constexpr auto operator>=(const t& a,const u& b)->decltype(partial_ordering(CompareDynamic(a,b))>=nullptr) {return CompareDynamic(a,b)>=nullptr;}
template<class t,class u> constexpr auto operator> (const t& a,const u& b)->decltype(partial_ordering(CompareDynamic(a,b))> nullptr) {return CompareDynamic(a,b)> nullptr;}

//
// General comparison features.
//

// Extrema calculation for no values (if exposed), single values (always), and multiple values (if total ordering).
template<class t> constexpr auto Min()->decltype(expose<t>::Min()) {return expose<t>::Min();}
template<class t> constexpr auto Max()->decltype(expose<t>::Max()) {return expose<t>::Max();}
template<class t> constexpr t    Min(const t& a) {return a;}
template<class t> constexpr t    Max(const t& a) {return a;}
template<class t,class u,class... vs,class w=common_type<t,u,vs...>> requires requires(w W) {total_ordering(CompareDynamic(W,W));}
constexpr w Min(const t& a,const u& b,const vs&... cs) {
	auto c=Min(b,cs...);
	if(a<=c)
		return a;
	return c;
}
template<class t,class u,class... vs,class w=common_type<t,u,vs...>> requires requires(w W) {total_ordering(CompareDynamic(W,W));}
constexpr w Max(const t& a,const u& b,const vs&... cs) {
	auto c=Max(b,cs...); 
	if(a<=c) 
		return c;
	return a;
}

// Low level sorting using stable Bentley-McIlroy 3-way partitioning qsort.
// See: https://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf
namespace Internal {
	template<class t> t DefaultSort(const t& T) {return T;}
};
template<class t,class by=t(*)(const t&),class v=callable_image<by,t>> requires IsTotalOrdering<v,v>
void Sort(t* es,int64 n,by By=Internal::DefaultSort<t>) {
	if(n<2)
		return;
	n--;
	int64 i=-1,j=n,p=-1,q=n;
	t e=es[n];
	for(;;) {
		while(CompareTotal(By(es[++i]),By(e      ))<nullptr);
		while(CompareTotal(By(e      ),By(es[--j]))<nullptr && j!=0);
		if(i>=j) break;
		Swap(es[i],es[j]);
		if(CompareTotal(By(es[i]),By(e    ))==nullptr) Swap(es[++p],es[i  ]);
		if(CompareTotal(By(e    ),By(es[j]))==nullptr) Swap(es[j  ],es[--q]);
	}
	Swap(es[i],es[n]);
	j=i-1,i=i+1;
	for(int64 k=0;   k<p; k++,j--) Swap(es[k],es[j]);
	for(int64 k=n-1; k>q; k--,i++) Swap(es[i],es[k]);
	Sort(es,j+1);
	Sort(es+i,n-i+1);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Hashing. Verse-equal values must have equal hashes. Must lack persistent or Verse-observable consequences, as it may change.

// Hashing help.
namespace Internal {
	template<nat i,class... ts> nat HashDynamicHelper(nat H,const tuple<ts...>& T);
	template<class t> nat HashDynamicMap(const t& TS);
	template<class t> nat HashDynamicSet(const t& S);
	template<class t> nat HashDynamicArray(const t& TS);
	template<class t>         struct hash_container           {};
	template<class k,class v> struct hash_container<map<k,v>> {template<class u> static nat HashDynamic(const u& U) {return HashDynamicMap(U);};};
	template<class t>         struct hash_container<bag<t>  > {template<class u> static nat HashDynamic(const u& U) {return HashDynamicSet(U);};};
	template<class t>         struct hash_container<array<t>> {template<class u> static nat HashDynamic(const u& U) {return HashDynamicArray(U);};};
}

// Hash of a unique future<> payload.
nat PayloadHash(nat n);

// ExposeHash(t) implies HashDynamic(t).
template<class t> requires Internal::HasExposeHash<t> nat HashDynamic(const t& A) {return ExposeHash(A);}

// ExposeUnique<t> implies HashDynamic(t).
template<class t> requires IsUnique<t> nat HashDynamic(const box<t>& A) {return Hash(A.AllocationBase());}

// HashDynamic(t) and IsComparable<t>() implies Hash(t).
template<class t> requires(IsComparable<t>()) auto Hash(const t& T)->decltype(nat(HashDynamic(T))) {return HashDynamic(T);}

// Dynamically typed hashing - runtime error for unsupported types.
template<class... ts> requires(sizeof...(ts)==0) nat HashDynamic(const future<>& F);
template<class t> nat HashDynamic(const var<t>& T);
nat HashDynamic(const pin<any>&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Functions.

// Exposing function objects.
namespace Internal {
	template<class t>                     struct call_type_helper;
	template<class t,class r,class ...ps> struct call_type_helper<r(t::*)(ps...) const> {using type=r(ps...);};
	template<class t,class r,class ...ps> struct call_type_helper<r(t::*)(ps...)      > {using type=r(ps...);};
}
template<class t>                                        auto     Callable(const t& T)->decltype(ExposeCallable(T)) {return ExposeCallable(T);}
template<class t,class... js> requires(sizeof...(js)==0) const t& Callable(const t& T,const js&...) {return T;}
template<class t            > using callable       = typename Internal::call_type_helper<decltype(&remove_const_ref<decltype(Callable(declval<t>()))>::operator())>::type;
template<class t>             using map_domain     = decltype(ExposeKeys(declval<t>())(declval<nat>()));

// Exposing the function hierarchy.
namespace Internal {
	//!!if this were inclusively concepted, we could drop the priority stuff.
	template<class t,class=decltype(nat(ExposeLength(declval<t>()))),class k=map_domain<t>> auto exposed_function_helper(priority0)->map<k,callable_image<callable<t>,k  >>;
	template<class t,class=decltype(nat(ExposeLength(declval<t>())))> auto exposed_function_helper(priority1)->array<callable_image<callable<t>,nat>>;
	template<class t> auto exposed_function_helper(priority2)->function<callable<t>>;
	template<class t> auto exposed_function_helper(priority3)->box<t>;
}
template<class t> using exposed_function_type           = decltype(Internal::exposed_function_helper<t>(priority0{}));
template<class t> using exposed_map_type_deprecate      = decltype(Internal::exposed_function_helper<t>(priority1{}));
template<class t> using exposed_function_type_deprecate = decltype(Internal::exposed_function_helper<t>(priority2{}));

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Containers.

// Get container length.
template<class t> constexpr auto Length(const t&             a)->decltype(ExposeLength(a))            {return ExposeLength(a);}
template<class t> auto           Length(const pin<box<t>>&   a)->decltype(ExposeLength(declval<t>())) {return a->ContainerLength;}
template<class t> auto           Length(const     box<t>&    a)->decltype(ExposeLength(declval<t>())) {return a->ContainerLength;}
template<class k,class v> auto   Length(const pin<map<k,v>>& a)                                       {return a->ContainerLength;}
template<class k,class v> auto   Length(const     map<k,v>&  a)                                       {return a->ContainerLength;}
template<class t> auto           Length(const pin<array<t>>& a)                                       {return a->ContainerLength;}
template<class t> auto           Length(const     array<t>&  a)                                       {return a->ContainerLength;}

// Get container keys.
template<class t> auto           Keys(const t&               a)->decltype(ExposeKeys(a))                 {return ExposeKeys(t);}
template<class t> auto           Keys(const pin<box<t>>&     a)->decltype(ExposeKeys(declval<t>()))      {return a->OnKeys();}
template<class t> auto           Keys(const     box<t>&      a)->decltype(ExposeKeys(declval<t>()))      {return a->OnKeys();}
template<class k,class v> k      Keys(const pin<map<k,v>>&   a)                                          {VERSE_UNIMPLEMENTED;}
template<class k,class v> k      Keys(const     map<k,v>&    a)                                          {VERSE_UNIMPLEMENTED;}
template<class t> array<nat>     Keys(const pin<array<t>>&   a)                                          {VERSE_UNIMPLEMENTED;}
template<class t> array<nat>     Keys(const     array<t>&    a)                                          {VERSE_UNIMPLEMENTED;}

// Detect if declared a bag.
namespace Internal {
	template<class t> auto IsSetHelper(priority0)->decltype(ExposeSet(declval<t>()),true) {return true;}
	template<class t> bool IsSetHelper(priority1) {return false;}
};
template<class t> const bool IsSet=Internal::IsSetHelper<t>(priority0{});

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Signatures.

// expose<t>::ExposeStaticSignature() implies ExposeSignature(const t&).
template<class t> requires(HasStaticSignature<t>) path ExposeSignature(const t&) {
	return path(expose<t>::ExposeStaticSignature());
}

// ExposeSignature(t) implies Signature(t).
template<class t> auto Signature(const t& T)->decltype(static_cast<path>(ExposeSignature(T))) {
	return ExposeSignature(T);
}

// Dynamic signature of any-derived types.
path Signature(const pin<any>& a);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Sound subtyping relationships.

// Here, IsSubtype<t,u> implies that for all t x, u(x) is valid and any(x) is observably equivalent to any(u(x)).
// In other words: it's a genuine subtype, and isn't merely convertible.
template<class t,class u> concept IsSubtype = IsEqual<simplify_type<u>,common_type<t,u>>;

// Common types helpers.
namespace Internal {

	// Simplify type by removing pin.
	template<class t> struct simplify_type_helper         {using type=t;};
	template<class t> struct simplify_type_helper<pin<t>> {using type=simplify_type<t>;};

	// Common base of boxes. Because of crappy C++ implementation of a?b:c, only works when in a direct subtype relationship.
	template<class t,class u> auto common_box_base(priority0)->box<remove_const_ref<decltype(*(declval<bool>()? declval<const t*>(): declval<const u*>()))>>;
	template<class t,class u> auto common_box_base(priority1)->box<remove_const_ref<decltype(*(declval<bool>()? declval<const u*>(): declval<const t*>()))>>;
	template<class t,class u> auto common_box_base(priority2)->any;
	
	// Common tuple.
	template<class,class>         struct merge_tuples;
	template<class t,class... us> struct merge_tuples<t,tuple<us...>>     {using type=tuple<t,us...>;};
	template<class,class>                             struct common_tuple {using type=any;};
	template<>                                        struct common_tuple<tuple<>,tuple<>> {using type=tuple<>;};
	template<class t,class... ts,class u,class... us> struct common_tuple<tuple<t,ts...>,tuple<u,us...>> {
		using type = typename merge_tuples<common_type<t,u>,typename common_tuple<tuple<ts...>,tuple<us...>>::type>::type;
	};

	// Handle real, function.
	// We subtype function<r(ps...)> covariantly in r and contravariantly in ps, assuming succeeds and unbounded effects.
	template<class t,class u                            > struct common_type_7                                            {using type=any;};
	template<                                           > struct common_type_7<real,real>                                 {using type=real;};
	template<class r0,class r1,class ...ps              > struct common_type_7<function<r0(ps... )>,function<r1(ps... )>> {using type=function<common_type<r0,r1>(ps...)>;};
	template<class r0,class r1,class ...ps0,class ...ps1> struct common_type_7<function<r0(ps0...)>,function<r1(ps1...)>> {using type=function<common_type<r0,r1>(const falsity&)>;};

	// Generalize map<u,t> to function<u(t)>, rational to real: all incomparable.
	template<class t>                 struct generalize_7                       {using type=t;};
	template<>                        struct generalize_7<rational>             {using type=real;};
	template<class t,class u>         struct generalize_7<map<u,t>>             {using type=function<t(u)>;};

	// Handle map<u,t>, rational; generalize IsComparable<t> to comparable.
	template<class t,class u>         struct common_type_6                      {using type=if_type<IsComparable<t>()&&IsComparable<u>(),comparable,typename common_type_7<typename generalize_7<t>::type,typename generalize_7<u>::type>::type>;};
	template<               >         struct common_type_6<rational,rational>   {using type=rational;};
	template<class k0,class k1,class v0,class v1> struct common_type_6<map<k0,v0>,map<k1,v1>> {using type=map<common_type<k0,k1>,common_type<v0,v1>>;};

	// Generalize: array<t> to map<nat,t>; integer to rational.
	template<class t>                 struct generalize_6                       {using type=t;};
	template<>                        struct generalize_6<integer >             {using type=rational;};
	template<class t>                 struct generalize_6<array<t>>             {using type=map<nat,t>;};

	// Handle array<t>, integer.
	template<class t,class u>         struct common_type_5                      {using type=typename common_type_6<typename generalize_6<t>::type,typename generalize_6<u>::type>::type;};
	template<               >         struct common_type_5<integer ,integer  >  {using type=integer;};
	template<class t,class u>         struct common_type_5<array<t>,array<u> >  {using type=array<common_type<t,u>>;};

	// Generalize: box<t> where t is an exposed function to exposed_function_type<t>; natural to ineger.
	template<class t>                 struct generalize_5                       {using type=t;};
	template<>                        struct generalize_5<natural>              {using type=integer;};
	template<class t>                 struct generalize_5<box<t>>               {using type=exposed_function_type<t>;};

	// Handle void, box<t>, box<tuple<ts...>>, box<option<t>>.
	template<class t,class u>         struct common_type_4                              {using type=typename common_type_5<typename generalize_5<t>::type,typename generalize_5<u>::type>::type;};
	template<class t>                 struct common_type_4<t        ,void             > {using type=void;};
	template<class u>                 struct common_type_4<void     ,u                > {using type=void;};
	template<class t,class u>         struct common_type_4<box<t>   ,box<u>           > {using type=decltype(common_box_base<t,u>(priority0{}));};
	template<class t,class u>         struct common_type_4<box<option<t>>,box<option<u>>> {using type=box<option<common_type<t,u>>>;};
	template<class t>                 struct common_type_4<box<option<t>>,box<tuple<>>> {using type=box<option<t>>;};
	template<class u>                 struct common_type_4<box<tuple<>>,box<option<u>>> {using type=box<option<u>>;};
	template<class... ts,class... us> struct common_type_4<box<tuple<ts...>>,box<tuple<us...>>> {using type=box<typename common_tuple<tuple<ts...>,tuple<us...>>::type>;};

	// Handle identical types following exposed_type.
	template<class t,class u>         struct common_type_3                            {using type=typename common_type_4<t,u>::type;};
	template<class t>                 struct common_type_3<t           ,t           > {using type=t;};

	// Handle future<t>, then generalize to exposed_type<t> which leaves us with any, its subtypes, and void.
	template<class t,class u>         struct common_type_2                            {using type=typename common_type_3<exposed_type<t>,exposed_type<u>>::type;};
	template<class t,class u>         struct common_type_2<future<t>   ,u           > {using type=future<common_type<t,u>>;};
	template<class t,class u>         struct common_type_2<t           ,future<u>   > {using type=future<common_type<t,u>>;};
	template<class t,class u>         struct common_type_2<future<t>   ,future<u>   > {using type=future<common_type<t,u>>;};
	template<class t>                 struct common_type_2<future<t>   ,future<t>   > {using type=future<t>;};
	template<class t>                 struct common_type_2<t           ,t           > {using type=t;};

	// Handle void, falsity, option, (t,t), arithmetic types via CT macro.
	template<class t,class u>         struct common_type_1                            {using type=typename common_type_2<t,u>::type;};
	template<>                        struct common_type_1<void        ,void        > {using type=void;};
	template<class t>                 struct common_type_1<void        ,t           > {using type=void;};
	template<class t>                 struct common_type_1<t           ,void        > {using type=void;};
	template<>                        struct common_type_1<void        ,falsity     > {using type=void;};
	template<>                        struct common_type_1<falsity     ,void        > {using type=void;};
	template<>                        struct common_type_1<falsity     ,falsity     > {using type=falsity;};
	template<class    t ,class    u > struct common_type_1<option<t>   ,option<u>   > {using type=option<common_type<t,u>>;};
	template<class t>                 struct common_type_1<option<t>   ,tuple<>     > {using type=option<t>;};
	template<class u>                 struct common_type_1<tuple<>     ,option<u>   > {using type=option<u>;};
	template<class... ts,class... us> struct common_type_1<tuple<ts...>,tuple<us...>> {using type=typename common_tuple<tuple<ts...>,tuple<us...>>::type;};

	// Numeric type relationships based on value inclusion, not lossy C++ conversion rules.
	#define CT(t,u,a) template<> struct common_type_1<t,u> {using type=a;}; template<> struct common_type_1<u,t> {using type=a;};
	CT(int8,nat8 ,int16  )
	CT(int8,int16,int16  ) CT(nat8,int16,int16)
	CT(int8,nat16,int32  ) CT(nat8,nat16,nat16) CT(int16,nat16,int32  )
	CT(int8,int32,int32  ) CT(nat8,int32,int32) CT(int16,int32,int32  ) CT(nat16,int32,int32)
	CT(int8,nat32,int64  ) CT(nat8,nat32,nat32) CT(int16,nat32,int64  ) CT(nat16,nat32,nat32) CT(int32,nat32,int64  )
	CT(int8,int64,int64  ) CT(nat8,int64,int64) CT(int16,int64,int64  ) CT(nat16,int64,int64) CT(int32,int64,int64  ) CT(nat32,int64,int64)
	CT(int8,nat64,integer) CT(nat8,nat64,nat64) CT(int16,nat64,integer) CT(nat16,nat64,nat64) CT(int32,nat64,integer) CT(nat32,nat64,nat64) CT(int64,nat64,integer)
	#undef CT

	// Reduce to 2-ary.
	template<>                struct common_type_0<                     > {using type=falsity;};
	template<class t>         struct common_type_0<t                    > {using type=t;};
	template<class t,class u,class... vs> struct common_type_0<t,u,vs...> {using type=typename common_type_0<typename common_type_1<t,u>::type,vs...>::type;};
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Immediates. Move to managed along with other wrapper stuff?

// Immediate versus managed storage.
template<class t> concept IsImmediate =
	!IsUnique<t>      && 
	!HasDestructor<t> && 
	sizeof(t)<=4      && 
	IsEqual<exposed_function_type<t>,box<t>>;
struct alignas(32) immediate_methods {
	path             (*ImmediateSignature)(const pin<any>&);
	dynamic_ordering (*ImmediateCompare)(const pin<any>&,const pin<any>&);
	bool             (*ImmediateIsComparable)(const pin<any>&);
	nat              (*ImmediateHash)(const pin<any>&);
	string           (*ImmediateToString)(const pin<any>&);
	const char*      ImmediateNativeName;
};
extern immediate_methods ImmediateMethodsOfIndex[2048]; // Used by .natvis, don't alter.
namespace Internal {
	template<class t> path             ImmediateMethodsSignature(const pin<any>& a);
	template<class t> dynamic_ordering ImmediateMethodsCompare(const pin<any>& A,const pin<any>& B);
	template<class t> bool             ImmediateMethodsComparable(const pin<any>& A);
	template<class t> nat              ImmediateMethodsHash(const pin<any>& A);
	template<class t> string           ImmediateMethodsToString(const pin<any>& A);
}
template<class t> constexpr immediate_methods ImmediateMethodsOfType() {
	return {
		Internal::ImmediateMethodsSignature<t>,
		Internal::ImmediateMethodsCompare<t>,
		Internal::ImmediateMethodsComparable<t>,
		Internal::ImmediateMethodsHash<t>,
		Internal::ImmediateMethodsToString<t>,
		NativeNameOfType<t>
	};
}
nat NewImmediateMethodsIndex(const immediate_methods&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Private kernel: provides low-level operations.

struct kernel {
	kernel();
	~kernel();

	struct non_value_accessor;
	struct kernel_thread {
		kernel_thread();
		~kernel_thread();
	};

	static volatile inline nat RunningThreadCount{0};

	static managed* Allocate(volatile nat* PayloadP,nat Size,nat Offset,const pin<context>& TargetContext);
	static managed* Allocate(volatile nat* PayloadP,nat Size,nat Offset,nat TargetContext,nat8 MT);
	static managed* AllocationBase(nat Payload);
	static managed* AllocationBaseSpeculative(nat Payload);
	static bool     IsIndexed(const pin<any>& A,nat Index);
	static bool     IsInteger(const future<>& A);
	static bool     IsRational(const future<>& A);
	static bool     IsResolvedFuture(nat NonValueType);
	static bool     IsWaitingFuture(nat NonValueType);
	static bool     IsPointer(nat NonValueType);
	static bool     IsSameBox(const pin<future<>>& a,const pin<future<>>& b);
	static bool     PayloadIsBox(nat P);
	static managed* CastManaged(const pin<future<>>&);
	static nat      PayloadMethodsIndex(nat P);
	static bool     PayloadIsImmediateInteger(nat Payload);
	static bool     PayloadIsManaged(nat P);
	static bool     DecodeNat(nat64* Result,const pin<any>& A);
	static bool     DecodeInt(int64* Result,const pin<any>& A);
	static void     AllocateInitialized(managed*,bool ManagedDestructor);
	static void     AllocateAbandoned(void*);
	static nat      EncodeIntegerDigit(nat);
	static nat      EncodeImmediate(nat Immediate,nat MethodsIndex);
	static nat      DecodeImmediateRaw(nat Payload);
	static nat32    DecodeImmediate32(nat Payload);
	static void     RegisterStatic(managed* Managed,nat Size);
	static void     InitStatic(managed* Managed);
	static void     FixStatic(const box<managed>&);
	static nat      AdvancePinned(const pin<future<>>& Source);
	static bool     AddSuspension(non_value_accessor& Work,const pin<box<when_future_suspension>>& Suspension);
	static nat      LockPayload(const volatile nat* PayloadP);
	static nat      LockPayloadCopy(const volatile nat* PayloadP);
	static void     ExposePayload(nat p);
	static void     StorePayload(volatile nat* PayloadP,nat p);
	static void     StorePayload(volatile nat* PayloadP,nat p,nat8 mt);
	static void     UnlockPayload();
	static nat      ReachedPayload(volatile nat* OuterPayloadP,nat OuterPayload);
	static void     RunThread(Internal::thread_startup* startup);
	static nat8     ReachedPayloadScan(volatile nat* OldPayloadP);
	static void     Scan(managed* Managed,volatile nat8* ManStartMTP);
	static tuple<iterate,step> CloneContext(const iterate& Iterate,const step& Step);
	static pin<context> ContextOf(const managed*);
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Platform abstraction, high level.

// Files.
string LoadTextFile(const string& Filename);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Effects.

// Effects per spec.
enum class fx:nat32 {

	// effects lattice:
	effects         = 0xFFFFFFFF,
    no_effects      = 0x00000000,

	// cardinalities sublattice:
    cardinalities   = 0x1FFFFFFF,
	abstracts       = 0x0FFFFFFF,
	resolves        = 0x07FFFFFF,
	ambiguates      = 0x0AFFFFFF,
	iterates        = 0x13FFFFFF,
	decides         = 0x03FFFFFF,
	succeeds        = 0x02FFFFFF,
	fails           = 0x01FFFFFF,
	contradicts     = 0x00FFFFFF,

	// recurses sublattice:
	recurses        = 0xFFFFFFFF,
	converges       = 0xFF0FFFFF,

	// transacts sublattice:
	transacts       = 0xFFFFFFFF,
	allocates       = 0xFFF1FFFF,
	reads           = 0xFFF2FFFF,
	writes          = 0xFFF4FFFF,
	no_transacts    = 0xFFF0FFFF,

	// imperatives sublattice:
	imperatives     = 0xFFFF7FFF,
	interacts       = 0xFFFF1FFF,
	throws          = 0xFFFF2FFF,
	suspends        = 0xFFFF4FFF,
	no_imperatives  = 0xFFFF0FFF,

	// unifies sublattice:
	unifies         = 0xFFFFFFFF,
	no_unifies      = 0xFFFFF0FF,

	// specifies sublattice:
	specifies       = 0xFFFFFFFF,
	no_specifies    = 0xFFFFFF0F,

	// rejects sublattice:
	rejects         = 0xFFFFFFFF,
	accepts         = 0xFFFFFFF0,
};
using enum fx;
VERSE_ENUM_LATTICE_OPEN(fx,nat32);

// Derived effects per spec.
inline const fx computes           = no_transacts&no_imperatives;
inline const fx top_allows         = succeeds&(interacts+       suspends)&accepts;
inline const fx function_allows    = resolves&(interacts+throws+suspends)&accepts;
inline const fx function_defaults  = succeeds&(no_imperatives           )&accepts;
inline const fx type_defaults      =          (no_imperatives           )&accepts;
inline const fx only_cardinalities =             converges&no_transacts&no_imperatives&no_unifies&accepts;
inline const fx only_succeeds      = succeeds&   converges&no_transacts&no_imperatives&no_unifies&accepts;
inline const fx only_recurses      = contradicts&          no_transacts&no_imperatives&no_unifies&accepts;
inline const fx only_transacts     = contradicts&converges&             no_imperatives&no_unifies&accepts;
inline const fx only_imperatives   = contradicts&converges&no_transacts&               no_unifies&accepts;
inline const fx only_rejects       = contradicts&converges&no_transacts&no_imperatives&no_unifies;

// Effects operations per spec.
inline fx SequenceFx(fx A,fx B) {
	return (A+B) & ((A&B)<=fails? fails: effects);
}
inline fx ProductFx(fx A,fx B) {
	return contradicts + (A+B) & (A<=contradicts || B<=contradicts? contradicts: (A&B)<=fails? fails: effects);
}
inline fx KeepDefaultFx(fx Fx) {
	if(Fx<=no_specifies)
		return no_effects;
	return
		(only_cardinalities<=Fx? effects: contradicts   )&
		(only_recurses     <=Fx? effects: converges     )&
		(only_transacts    <=Fx? effects: no_transacts  )&
		(only_imperatives  <=Fx? effects: no_imperatives);
}
inline fx KeepSpecifierFx(fx Fx) {
	if(Fx<=no_specifies)
		return effects;
	return
		(only_cardinalities<=Fx? no_effects: only_cardinalities)+
		(only_recurses     <=Fx? no_effects: only_recurses     )+
		(only_transacts    <=Fx? no_effects: only_transacts    )+
		(only_imperatives  <=Fx? no_effects: only_imperatives  );
}
inline fx AllowIteratesFx(fx OuterAllowFx=effects) {
	return (OuterAllowFx&contradicts&no_imperatives) + (only_cardinalities&iterates);
}
inline fx ReadyAfterFx(fx PendingFx) {
	return
		(iterates  <= contradicts   +PendingFx? abstracts:       effects)& // Spec: block_iterates
		(reads     <= no_transacts  +PendingFx? allocates+reads: effects)& // Spec: block_writes
		(writes    <= no_transacts  +PendingFx? allocates:       effects)& // Spec: block_reads+block_writes
		(throws    <= no_imperatives+PendingFx? no_imperatives:  effects)& // Spec: block_imperatives
		(suspends  <= no_imperatives+PendingFx? no_imperatives:  effects)& // Spec: block_imperatives
		(interacts <= no_imperatives+PendingFx? no_imperatives:  effects)& // Spec: block_imperatives
		(rejects   <= accepts       +PendingFx? accepts:         effects); // Not in spec; sequences verify-time test(...){...}.
}
inline fx CompliesFx(fx ActualFx,fx SpecifiedFx) {
	return
		(ActualFx            )<=SpecifiedFx? only_succeeds:
		(ActualFx&SpecifiedFx)<=contradicts? only_cardinalities&fails:
		                                     only_cardinalities&abstracts&ActualFx;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Public library interface.

// Memory Management.
void  Collect(bool Verbose=0,bool Final=0);
void  SettleCollector();

// Execution.
error ContextError(const locus&);
void  ContextReport(bool Everything=false);
void  StuckFx(fx);
void  Resume();
bool  IsMultithreading();
bool  AllFxReady(fx Fx);
bool  IsFlexible(const pin<future<>>&);
void  ReadyCommonContext(context);

// Stepped execution.
extern const step BadStep;
current_step UnifyStep(const char* What,const locus& Locus,const future<>& a,const future<>& b,const step& Step0);
current_step ResumeStep(const step& Step0);
current_step ForForkStep(const locus& Locus,option<fx> ForHoldFx,nat i,nat n,const step& Step0,const function<current_step(nat,const step& Step1)>& Steps);
current_step FailStep(const locus& Locus,const char* What,const step& Step0);
current_step ThrowStep(const locus& Locus,const future<>& a,const step& Step0);
current_step ErrStep(const error& a,const step& Step0=BadStep);

// Internal.
namespace Internal {
	iterate LeaveIterate();
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Base class managed: all allocations in managed memory inherit this either directly or through a boxer.

// Wrapper for non-classes.
template<class t> struct managed_value;
template<class t> using  managed_value_class=if_type<__is_class(t),t,managed_value<t>>;

// Manager classes extending managed which we wrap around t When constructing.
template<class t                    ,class... bases> struct managed_box;
template<class t,class              ,class... bases> struct function_box;
template<class t,class r,class... ps,class... bases> struct function_box<t,r(ps...),bases...>;
template<class t,class k,class v    ,class... bases> struct map_box;
template<class t,class u            ,class... bases> struct array_box;

// Internal methods.
namespace Internal {
	template<class t> inline const nat MethodsIndexOf = NewImmediateMethodsIndex(ImmediateMethodsOfType<t>()); // Exposed for natvis.
}

// Base class of managed objects.
struct managed {
	managed();
	managed(const managed&);
	constexpr managed(construct_constexpr) {}
	~managed()=default;
	pin<context> Context() const;
	struct cloner;

	// Virtual member functions. Can't make pure because of need to detect constructibility.
	virtual void        OnDestructor  ();
	virtual void        OnCopy        (managed*) const;
	virtual path        OnSignature   () const;
	virtual bool        OnIsMap       () const;
	virtual bool        OnIsSet       () const;
	virtual bool        OnIsArray     () const;
	virtual bool        OnIsComparable() const;
	virtual bool        OnIsUnique    () const;
	virtual const char* OnNativeName  () const;
	virtual string      OnToString    () const;
	virtual void        OnCloned      (cloner&);
	virtual nat         OnHash        () const;
	virtual dynamic_ordering OnCompare(const pin<any>&) const;

	// Managed helpers here ensure a class with private constructors need only friend managed.
	template<class t,class... ps> static nat EncodeImmediate(const ps&... PS) {
		nat Immediate=0;
		new(&Immediate)t(PS...);
		return kernel::EncodeImmediate(Immediate,Internal::MethodsIndexOf<t>);
	}

	// Managed object construction.
	template<class managed,class t,class ...ps> static auto ConstructorSizeHelper(priority0,const ps&... PS)->decltype(t::OnConstructorSize(PS...)) {return t::OnConstructorSize(PS...);}
	template<class managed,class t,class ...ps> static auto ConstructorSizeHelper(priority1,const ps&...   )->nat                                   {return 0;}
	template<class managed,class t,class ...ps> static nat  ConstructorSize(const ps&... PS) {
		return sizeof(managed)+ConstructorSizeHelper<managed,t,ps...>(priority0{},PS...);
	}

	// Find which manager to use for a given type.
	template<class t,bool man,class f> struct box_finder {};
	template<class t>                     struct box_finder<t,1,box<t>            > {using type=managed_box <t>;};
	template<class t>                     struct box_finder<t,0,box<t>            > {using type=managed_box <t         ,managed                   >;};
	template<class t,class r,class... ps> struct box_finder<t,1,function<r(ps...)>> {using type=function_box<t,r(ps...)                           >;};
	template<class t,class r,class... ps> struct box_finder<t,0,function<r(ps...)>> {using type=function_box<t,r(ps...),managed_function<r(ps...)>>;};
	template<class t,class k,class v>     struct box_finder<t,1,map<k,v>          > {using type=map_box     <t,k,v                                >;};
	template<class t,class k,class v>     struct box_finder<t,0,map<k,v>          > {using type=map_box     <t,k,v     ,managed_map<k,v>          >;};
	template<class t,class u>             struct box_finder<t,1,array<u>          > {using type=array_box   <t,u                                  >;};
	template<class t,class u>             struct box_finder<t,0,array<u>          > {using type=array_box   <t,u       ,managed_array<u>          >;};
	template<class t,class as=exposed_function_type<t>> using boxer = typename box_finder<t,IsDerived<managed_value_class<t>,managed>,as>::type;

	// Member variables.
	union {nat ManagedForward;};
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Synchronous casting.

// Special casting.
nat CastIndex(const future<>&);

// Cast<t> casts to box<t> if t extends managed, else to t.
namespace Internal {
	template<class t> box<t> cast_type_helper(const managed*);
	template<class t> t      cast_type_helper(const void   *);
}
template<class t> using cast_type = decltype(Internal::cast_type_helper<t>(declval<const t*>()));

// Encase: Synchronously attempt to cast and return whether it succeeded, with result emplaced on success.
template<class target,class source> bool Encase(target* Target,const source& Source,bool* Finished);

// Coerce: Synchronous casting, returns value or fatal error.
template<class t,class target=cast_type<t>,class source> target Coerce(const source& Source);

// Cast: Synchronous casting, returns option<c>.
template<class t,class target=cast_type<t>,class source> option<target> Cast(const source& Source) {
	return option<target>(Internal::construct_cast{},Source); // Uses Encase.
}

// IsA: Synchronous casting, returns boolean.
template<class target,class source> bool IsA(const source& Source) {
	return bool(Cast<target>(Source));
}

// Presume: Synchronously presume type with as little checking as possible.
template<class target,class source> target Presume(const source& Source) {
	if constexpr(IsFuture<target> || IsAny<target>&&IsAny<source>&&IsSubtype<source,target>&&!IsBox<target>&&!IsBox<source>)
		return reinterpret_cast<const target&>(Source); // Use directly.
	else
		return Coerce<target>(Source); // When type conversion is required.
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Futures.

template<> struct future<> {
	future();
	future(const future&);
	future(const pin<future>&);
	template<class t> future(const pin<t>& a): future(reinterpret_cast<const pin<future>&>(a)) {}
	template<class a> requires(IsAny<a>? IsAnyCopyable<a>: IsSubtype<a,future<>>) explicit(IsExplicit<a>)
	VERSE_FORCE_INLINE future(const a& A) {
		if constexpr(IsAny<a>)
			new(this)future(construct_value_copy{},A);
		else
			new(this)exposed_type<a>(ToAny(A));
	}
	~future();
	future<>& operator=(const future<>& a) {return *new(this)future<>(a);}
	void Resolve(const future<>&,bool ResolveLocal=true) const;
	bool ResolveBatch(future<>,bool=true) const;
	current_step ResolveStep(const future<>&,const step& Step0,bool ResolveLocal=true) const;
	pin<context> Context() const;
	template<class u=any> requires(IsSubtype<u,any>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u=any> requires(IsSubtype<u,any>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u=any> requires(IsSubtype<u,any>) auto Coerce() const {return Verse::Coerce<u>(*this);}

	mutable volatile nat Payload;
private:
	future(construct_value_copy,const future&);
	VERSE_FORCE_INLINE future(construct_payload,nat p): Payload(p) {}
	VERSE_FORCE_INLINE future(construct_no_init) {}
	friend struct any;
};
template<class t> struct future: future<> {
	future(): future<>() {}
	future(const future& a): future<>(a) {}
	template<class u> requires(IsSubtype<u,future>) future(const u& U): future<>(U) {}
	void Resolve(const future<t>& a,bool ResolveLocal=true) const {future<>::Resolve(a,ResolveLocal);}
	bool ResolveBatch(const future<t>& a,bool ResolveLocal=true) const {return future<>::ResolveBatch(future<any>(a),ResolveLocal);}
	current_step ResolveStep(const future<t>& a,const step& Step0,bool ResolveLocal=true) const;
	template<class u=t> requires(IsSubtype<u,t>) auto IsA   () const {return Verse::Cast  <u>(*this);}
	template<class u=t> requires(IsSubtype<u,t>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u=t> requires(IsSubtype<u,t>) auto Coerce() const {return Verse::Coerce<u>(*this);}
};

// Value functions.
bool           IsZero(const future<>& e);
const char*    NativeNameOf(const pin<any>& a);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// A head-normal value of any type.

struct any: future<> {
	VERSE_FORCE_INLINE any(const any& a): future<>(a) {}
	template<class u> requires(IsSubtype<u,any>) VERSE_FORCE_INLINE any(const u& a): future<>(a) {}
	template<class u> requires(IsSubtype<u,any>) auto IsA   () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,any>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,any>) auto Coerce() const {return Verse::Coerce<u>(*this);}
private:
	any(construct_new_var,const pin<future<>>& a);
	VERSE_FORCE_INLINE any(construct_value_copy,const any& a): future<>(construct_value_copy{},a) {}
	VERSE_FORCE_INLINE any(construct_payload,nat p): future<>(construct_payload{},p) {}
	VERSE_FORCE_INLINE any(construct_no_init): future<>(construct_no_init{}) {}
	friend struct real;
	friend struct comparable;
	template<class t> friend struct box;
	template<class t> friend struct var;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Pinning. 

// Pinned subtype of future<>-derived types for single-threaded use only.
// Unsafe when subject to multithreaded read-write races.
// We use this in box<t>::operator-> and other functions that need to persistently access raw t* because
// the underlying const future<>&, which is kept alive through the presumed liveness of its payload,
// may be mutated while we're working on our copy of the t*, resulting in it being deallocated.
template<class t> struct pin: t {
	pin(const pin& a): t(a) {}
	template<class a> requires(IsSubtype<a,t>) pin(const a& A): t(A) {}
	template<class... ps> requires requires(ps... PS) {t{PS...};} explicit pin(const ps&... PS): t{PS...} {}
	template<class a> requires(IsSubtype<a,t>) pin& operator=(const a& A) {return *new(this)pin(A);}

	// Can use pin<t> in-place for pin<u> where u is a class derived from t. Special casing future&any for yet-undefined classes.
	template<class u> requires(IsDerived<u,t>) operator const pin<u>&() const {return reinterpret_cast<const pin<u>&>(*this);}
	//template<class=void> requires(!IsFuture<t>) operator const pin<any>& () const {return reinterpret_cast<const pin<any>&>(*this);}
	operator const pin<future<>>&() const {return reinterpret_cast<const pin<future<>>&>(*this);}

	// Don't return decltype(auto) to avoid temptation of unsafe *pin(b). Use pin(b).operator->() for that.
	//!!suppress if t derived from managed or lacks public copy constructor.
	template<class t0=t,class boxed=typename t0::boxed> auto operator*() const {
		if constexpr(IsImmediate<boxed>) {
			auto Immediate=kernel::DecodeImmediateRaw(future<>::Payload);
			return boxed(reinterpret_cast<const boxed&>(Immediate));
		}
		else return *reinterpret_cast<const boxed*>(future<>::Payload);
	}
	template<class t0=t,class boxed=typename t0::boxed> auto operator->() const {
		if constexpr(IsImmediate<boxed>) {
			auto Immediate=kernel::DecodeImmediateRaw(future<>::Payload);
			return help_member<boxed>{reinterpret_cast<boxed&>(Immediate)};
		}
		else return reinterpret_cast<boxed*>(future<>::Payload); // If no ExposeUnique, then boxed is const t.
	}
	template<class u> requires(IsSubtype<u,t>) auto IsA   () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,t>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,t>) auto Coerce() const {return Verse::Coerce<u>(*this);}

	//!!this is bad for array<> specifically because its box type doesn't take nat.
	template<class t0=t,class ...ps> auto operator()(const ps&... PS) const->decltype(declval<typename t0::boxed>()(PS...)) {return (*operator->())(PS...);}
	template<class t0=t,class    p > auto operator[](const p&     P ) const->decltype(declval<typename t0::boxed>()(P    )) {return (*operator->())(P    );}
	template<class t0=t> requires requires(t0 T) {ExposeLength(declval<t0>());} explicit operator bool() const {return (*this)->ContainerLength!=0;}

private:
	VERSE_FORCE_INLINE pin(construct_payload,nat Payload): t(construct_payload,Payload) {}
	VERSE_FORCE_INLINE pin(construct_no_init): t(construct_no_init{}) {}
	friend struct kernel;
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Boxed values referencing managed object or immediate value.

extern pin<context> Thread;
template<class t> struct box: any {
	using boxed=if_type<IsUniqueMutable<t>,t,const t>;

	// Referencing constructors.
	VERSE_FORCE_INLINE box(const box& r           ): any(r) {}
	VERSE_FORCE_INLINE box(const pin<box>& r      ): any(r) {}

	// Allocating constructors.
	template<class boxer,class... ps> requires (IsImmediate<t> && requires(ps... PS) {boxer(PS...);})
	box(construct_boxer<boxer>,const ps&... PS):
		any(construct_payload{},managed::EncodeImmediate<t>(PS...)) {}
	template<class boxer,class... ps> requires (!IsImmediate<t> && requires(ps... PS) {boxer(PS...);})
	box(construct_boxer<boxer>,const ps&... PS): any(construct_no_init{}) {
		nat      Size    = managed::ConstructorSize<boxer,t>(PS...);
		nat      Offset  = IsDerived<t,managed>? 0: nat(((const boxer*)nullptr)->BoxedPointer());
		managed* Managed = kernel::Allocate(&Payload,Size,Offset,Thread);
		new(Managed)boxer(PS...);
		kernel::AllocateInitialized(Managed,HasDestructor<boxer>);
	}
	template<class... ps> requires requires(ps... PS) {t(PS...);} explicit box(const ps&... PS):
		box(construct_boxer<typename managed::template boxer<t>>{},PS...) {}

	// Allocate or copy from box<u> subtype.
	template<class u> requires(IsSubtype<box<u>,box>) box(const pin<box<u>>& U): any(construct_no_init{}) {
		if constexpr(!IsImmediate<t> && IsDerived<u,t>) {
			// Reference managed pointer with offset obtained from static upcast.
			auto NewPayload=&static_cast<const t&>(*reinterpret_cast<const u*>(U.Payload));
			new(this)box(reinterpret_cast<const box&>(NewPayload));
		}
		else {
			// Make a copy since immediate or subtype is not derived.
			new(this)box(t(*U));
		}
	}
	template<class u> requires(IsSubtype<box<u>,box<t>>) box(const box<u>& a): box(pin(a)) {}

	// Allocate or copy from const t&, referencing in-place if managed and dynamic_cast-reachable. Ensures *this -> box is efficient.
	template<class u> requires(IsDerived<u,t>) box(const u& U): any(construct_no_init{}) {
		if(IsDerived<u,managed> || !IsImmediate<t>&&kernel::AllocationBaseSpeculative(nat(&U))) {
			// Statically cast U* in managed memory to T*.
			auto NewPayload = static_cast<const t*>(&U);
			new(this)box(reinterpret_cast<const box&>(NewPayload));
		}
		else if constexpr(IsAbstract<u>) {
			// Can't handle it.
			VERSE_ERR("can't make managed copy of user-defined user-allocated abstract class not derived from managed");
		}
		else if constexpr(IsEqual<t,u>) {
			// Construct a new allocation. May slice, as we can't allocate or copy polymorphically without boxer.
			new(this)box(construct_boxer<typename managed::template boxer<t>>{},U);
		}
		else {
			// Construct then convert.
			new(this)box(box<u>(U));
		}
	}

	// Operations.
	auto operator*()  const {auto P=pin(*this); return *P;}//!!suppress if t derived from managed / lacks public copy constructor
	auto operator->() const {return pin(*this);}
	template<class u> requires requires(u U) {box(U);} box& operator=(const u& U) {return *new(this)box(U);}
	template<class... ps> auto operator()(const ps&... PS) const->decltype(Callable(declval<t>())(PS...)) {auto F=pin(*this); return Callable(*F.operator->())(PS...);}
	template<class    p > auto operator[](const p &     P) const->decltype(Callable(declval<t>())(P    )) {auto F=pin(*this); return Callable(*F.operator->())(P    );}
	template<class t0=t> requires requires(t0 T) {ExposeLength(T);} explicit operator bool() const {auto F=pin(*this); return  ExposeLength(*F.operator->());}
	template<class t0=t> requires requires(t0 T) {ExposeLength(T);} bool     operator!    () const {auto F=pin(*this); return !ExposeLength(*F.operator->());}
	template<class u> requires(IsSubtype<u,box>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,box>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,box>) auto Coerce() const {return Verse::Coerce<u>(*this);}
	managed* AllocationBase() const {
		if constexpr(IsDerived<t,managed>) 
			return (managed*)Payload;
		else
			return kernel::AllocationBase(Payload);
	}
	pin<context> Context() const;

private:
	VERSE_FORCE_INLINE box(construct_payload,nat p): any(construct_payload{},p) {}
	VERSE_FORCE_INLINE box(construct_no_init      ): any(construct_no_init{}) {}
	template<class> friend struct function;
	template<class> friend struct array;
	template<class> friend struct pin;
};

// Boxing of static values without allocation overhead.
struct static_registrar {
	static_registrar(managed* Managed,nat Size) {
		kernel::RegisterStatic(Managed,Size);
	}
};
template<class t> struct static_boxer {
	static_registrar                                Registrar;
	alignas(32) typename managed::template boxer<t> Managed;
	template<class... ps> static_boxer(const ps&... PS):
		Registrar(&Managed,sizeof(static_boxer)), Managed(PS...) {
		kernel::InitStatic(&Managed);
	}
	template<class u> u GetBox0() const {
		// Performance critical and bogus as it only works for functions and things inheriting managed.
		auto n = nat(&Managed);
		return reinterpret_cast<u&>(n);
	}
	template<class u> u GetBox1() const {
		auto n = nat(Managed.BoxedPointer());
		return reinterpret_cast<u&>(n);
	}
};
template<class t,class... ps> requires requires(ps... PS) {t(PS...);}
auto StaticBox(const ps&... PS) {
	static auto T=static_boxer<t>(PS...); // May be complicated due to thread synchronization check.
	return pin(T.template GetBox1<exposed_type<t>>());
};
template<class t> auto StaticFunction(const t& T) {
	static auto B=static_boxer<t>(T);
	return pin(B.template GetBox1<decltype(function(T))>());
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Arithmetic.

// Real numbers. Currently limited to the rationals, but may support constructive reals in the future.
struct real: any {
	VERSE_FORCE_INLINE real(): any(nat8(0)) {}
	VERSE_FORCE_INLINE real(const real& a): any(a) {}
	template<class u> requires(IsSubtype<u,rational>) VERSE_FORCE_INLINE real(const u& U): any(U) {}
	VERSE_FORCE_INLINE real& operator=(const real& a) {return *new(this)real(a);}
	template<class u> requires(IsSubtype<u,real>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,real>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,real>) auto Coerce() const {return Verse::Coerce<u>(*this);}

private:
	VERSE_FORCE_INLINE real(construct_payload,nat p): any(construct_payload{},p) {}
	friend struct rational;
};

// Rational numbers.
struct rational: real {
	VERSE_FORCE_INLINE rational(): real() {}
	VERSE_FORCE_INLINE rational(const rational& a): real(a) {}
	template<class u> requires(IsSubtype<u,rational>) VERSE_FORCE_INLINE rational(const u& U): real(U) {}
	VERSE_FORCE_INLINE rational& operator=(const rational& a) {return *new(this)rational(a);}
	template<class u> requires(IsSubtype<u,rational>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,rational>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,rational>) auto Coerce() const {return Verse::Coerce<u>(*this);}
	friend rational  operator+ (const rational& a);
	friend rational  operator- (const rational& a);
	friend rational  operator+ (const rational& a,const rational& b);
	friend rational  operator- (const rational& a,const rational& b);
	friend rational  operator* (const rational& a,const rational& b);
	friend rational  operator/ (const rational& a,const rational& b);
	friend rational  operator% (const rational& a,const rational& b);
	rational&        operator+=(const rational& b);
	rational&        operator-=(const rational& b);
	rational&        operator*=(const rational& b);
	rational&        operator/=(const rational& b);
	friend integer   Floor(const rational& a);
	friend integer   Ceil(const rational& a);
	friend integer   Trunc(const rational& a);
	friend integer   Round(const rational& a);
	friend integer   Denominator(const rational& a);
	friend integer   Numerator(const rational& a);
	friend rational  Ratio(const rational& a,const rational& b);

private:
	VERSE_FORCE_INLINE rational(construct_payload,nat p): real(construct_payload{},p) {}
	friend struct integer;
};

// Math.
template<class t,class u> auto Quotient(const t& T,const u& U) {auto ro=EuclideanDivision(T,U); if(!ro) VERSE_ERR("division by zero"); return ro.Coerce().template get<0>();}
template<class t,class u> auto Mod     (const t& T,const u& U) {auto ro=EuclideanDivision(T,U); if(!ro) VERSE_ERR("division by zero"); return ro.Coerce().template get<1>();}

// Integers.
struct integer: rational {
	inline integer(): rational() {}
	VERSE_FORCE_INLINE integer(const integer& a): rational(a) {}
	template<class u> requires(IsSubtype<u,integer>) VERSE_FORCE_INLINE integer(const u& U): rational(U) {}
	VERSE_FORCE_INLINE integer& operator=(const integer& a) {return *new(this)integer(a);}
	template<class u> requires(IsSubtype<u,integer>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,integer>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,integer>) auto Coerce() const {return Verse::Coerce<u>(*this);}
	friend integer  operator+ (const integer& a);
	friend integer  operator- (const integer& a);
	friend integer  operator+ (const integer& a,const integer& b);
	friend integer  operator- (const integer& a,const integer& b);
	friend integer  operator* (const integer& a,const integer& b);
	friend integer  operator/ (const integer& a,const integer& b) {return Quotient(a,b);}
	friend integer  operator% (const integer& a,const integer& b) {return Mod(a,b);}
	friend integer  operator| (const integer& a,const integer& b);
	friend integer  operator~ (const integer& a);
	friend integer  operator& (const integer& a,const integer& b);
	friend integer  operator^ (const integer& a,const integer& b);
	integer         operator++(int);
	integer         operator--(int);
	integer&        operator++();
	integer&        operator--();
	integer&        operator+=(const integer& b);
	integer&        operator-=(const integer& b);
	integer&        operator*=(const integer& b);
	integer&        operator/=(const integer& b);
	integer&        operator%=(const integer& b);
	integer&        operator&=(const integer& b);
	integer&        operator^=(const integer& b);
	integer&        operator|=(const integer& b);
	friend option<tuple<integer,integer>> EuclideanDivision(integer D,integer d);

private:
	VERSE_FORCE_INLINE integer(construct_payload,nat p): rational(construct_payload{},p) {}
	friend struct natural;
	friend integer ExposeValue(int64);
};

// Natural numbers.
struct natural: integer {
	inline natural(): integer() {}
	VERSE_FORCE_INLINE natural(const natural& a): integer(a) {}
	VERSE_FORCE_INLINE natural(nat   a): integer(a) {}
	VERSE_FORCE_INLINE natural(nat32 a): integer(a) {}
	template<class u> requires(IsSubtype<u,natural>) VERSE_FORCE_INLINE natural(const u& U): integer(U) {}
	VERSE_FORCE_INLINE natural& operator=(const natural& a) {return *new(this)natural(a);}
	template<class u> requires(IsSubtype<u,natural>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,natural>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,natural>) auto Coerce() const {return Verse::Coerce<u>(*this);}
	/*
	friend natural  operator+ (const natural& a);
	friend natural  operator+ (const natural& a,const natural& b);
	friend natural  operator* (const natural& a,const natural& b);
	friend natural  operator/ (const natural& a,const natural& b);
	friend natural  operator% (const natural& a,const natural& b);
	friend natural  operator| (const natural& a,const natural& b);
	friend natural  operator& (const natural& a,const natural& b);
	friend natural  operator^ (const natural& a,const natural& b);
	natural         operator++(int);
	natural&        operator++();
	natural&        operator+=(const natural& b);
	natural&        operator*=(const natural& b);
	natural&        operator/=(const natural& b);
	natural&        operator%=(const natural& b);
	natural&        operator&=(const natural& b);
	natural&        operator^=(const natural& b);
	natural&        operator|=(const natural& b);
	*/
	friend option<tuple<natural,natural>> EuclideanDivision(natural D,natural d);

private:
	VERSE_FORCE_INLINE natural(construct_payload,nat P): integer(construct_payload{},P) {}
	friend natural ExposeValue(nat64);
};

// Math.
inline natural Abs(const integer& n) {integer r=n>=0? n: -n; return reinterpret_cast<const natural&>(r);}
inline natural Abs(const natural& n) {return n;}
template<class t,class u> common_type<t,u> GCD(const t& T,const u& U) {if(T==0) return Abs(U); if(U==0) return Abs(T); return GCD(U,Mod(Abs(T),Abs(U)));}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Functions.

// A function: it inherits any, has return-type covariance and parameter-type contravariance.
template<class r,class ...ps> struct function<r(ps...)>: box<managed_function<r(add_const_ref<ps>...)>> {
	using base=managed_function<r(add_const_ref<ps>...)>;

	// Copying constructors.
	function(const function&       F): box<base>(F) {}
	function(const pin<function>&  F): box<base>(F) {}
	function(const box<base>&      F): box<base>(F) {}
	function(const pin<box<base>>& F): box<base>(F) {}
	template<class u> requires(IsAny<u> && (IsEqual<function,function<>>&&IsSubtype<u,function<>> || IsEqual<exposed_function_type_deprecate<u>,function>))
	function(const u& U):
		// Either we're function<> and T is a function, or T has compatible virtual operator().
		box<base>(reinterpret_cast<const base&>(*kernel::CastManaged(U))) {}

	// Allocating constructors supporting implicit conversions, even if not a subtype, even if overloaded.
	template<class t,class... js> requires(sizeof...(js)==0 && requires(t T,ps... PS) {r(Callable(T)(PS...));})
	function(const t& T,const js&...):
		box<base>(construct_boxer<managed::boxer<t,function>>{},T) {}
	function(r T(ps...)): function([T](const ps&... PS)->r {return T(PS...);}) {}

	// Providing exact operator() rather than inheriting box<t>::operator() so callable works.
	r operator()(const ps&... PS) const {auto F=pin(*this); return Callable(*F.operator->())(PS...);}
	//void operator->() const;
};
template<class r,class... ps> function(r(ps...))->function<r(ps...)>;
template<class f>             function(const f&)->function<callable<f>>;

// A managed_function: a managed abstract base classes to contain functions.
template<> struct managed_function<>: managed {
	nat ContainerLength;
	managed_function(nat ContainerLength0=expose<nat>::Max()): ContainerLength(ContainerLength0) {}
	constexpr managed_function(construct_constexpr,nat ContainerLength0):
		managed(construct_constexpr{}), ContainerLength(ContainerLength0) {}
	path                      OnSignature () const override;
	virtual future<>          OnCallAssert(const future<>& P) const=0;
	virtual current_step      OnCallStep  (const future<>& R,const future<>& P,const step& Step0) const=0;
	virtual array<comparable> OnKeys      () const;
	virtual future<>          operator()  (const Internal::internal_falsity&) const {VERSE_UNEXPECTED;}

	// Helpers exposed to subclasses.
	static current_step ArrayCallStep(const array<>& AS,const future<>& R,const future<>& P,const step& Step0);
	static current_step MapCallStep(const map<>& Self,const future<>& R,const future<>& P,const step& Step0);
};
template<class r,class ...ps> struct managed_function<r(ps...)>: managed_function<> {
	managed_function(nat Count0=expose<nat>::Max()): managed_function<>(Count0) {}
	future<>     OnCallAssert(const future<>& P) const override;
	current_step OnCallStep(const future<>& R,const future<>& P,const step& Step0) const override;
	virtual r operator()(const ps&... PS) const=0; // Overriding if r(const ps&...) equals future<>(const falsity&), but we can't specify.
};

// Steps for running code in continuation-passing style.
// We use step to manage potential future steps, and current_step to return the step to run next,
// with the current_step(step) conversion checking !IsReady at the return site, so the debugger breaks
// in the function where the problem occurred regardless of compiler copy-elision choices.
struct step: function<current_step()> {
	using function::function;
};
struct current_step: function<current_step()> {
	template<class... ts> requires(requires(ts... TS) {function<current_step()>(TS...);}) current_step(const ts&... TS);
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Tables: ordered container mapping unique keys to values.

// Managed map type.
template<class k,class v> struct map: box<if_type<IsEqual<k,future<>>&&IsEqual<v,future<>>,managed_function<>,managed_map<k,v>>> {
	using base=if_type<IsEqual<k,future<>>&&IsEqual<v,future<>>,managed_function<>,managed_map<k,v>>;

	// Copying constructors.
	map(const map&      T): box<base>(T) {}
	map(const box<map>& T): box<base>(T) {}
	map(tuple<> = False);
	template<class u> requires(IsAny<u> && IsEqual<base,managed_map<>> && IsSubtype<u,map<>> || IsEqual<exposed_map_type_deprecate<u>,map>) map(const u& U):
		box<base>(reinterpret_cast<const base&>(*kernel::CastManaged(U))) {}

	// Allocating constructors.
	template<class t,class... js> requires(sizeof...(js)==0 && IsSubtype<t,map>) map(const t& T,const js&...): 
		box<base>(construct_boxer<managed::boxer<t,map>>{},T) {}

	// Providing exact operator() rather than inheriting box's variadic template so callable works.
	v operator()(const k& Key) const {auto F=pin(static_cast<const box<base>&>(*this)); return F->operator()(Key);}
};

// Managed map base.
template<class k,class v> struct managed_map: managed_function<v(k)> {
	managed_map(nat Count0=0): managed_function<v(k)>(Count0) {}
	current_step      OnCallStep    (const future<>& R,const future<>& P,const step& Step0) const override {VERSE_UNIMPLEMENTED;}
	bool              OnIsArray     () const override {return false;} // TODO: Check if Keys are sequential indices.
	bool              OnIsSet       () const override {return 0;}     // TODO: Check if mapping is identity.
	array<comparable> OnKeys        () const override;
	virtual array<k> Keys() const=0;
};

// Correct but inefficient map implementation. A hash array mapped trie would be nicer.
template<class k,class v> struct managed_map_also: managed_map<k,v> {
	k        Key;
	v        Value;
	map<k,v> Then;
	managed_map_also(const k& Key0,const v& Value0,const box<managed_map<k,v>>& Then0):
		managed_map<k,v>(Then->ContainerLength+1), Key(Key0), Value(Value0), Then(Then0) {}
	current_step OnCallStep(const future<>& R,const future<>& P,const step& Step0) const override {
		return MapCallStep(*this,R,P,Step0);
	}
	v operator()(const k& K) const override {
		if(K==Key)
			return Value;
		return Then(K);
	}
	array<k> Keys() const override {
		return array{Key}+Then->Keys();
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Sets, tables that are identity functions.

// Managed bag type. Ordered by construction, not equivalent to a mathematical set.
template<class t> struct bag: any {};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Arrays.

// Forward declare.
template<class f,class r=callable_image<f,nat>> if_type<IsEqual<r,void>,void,array<r>> For(nat n,const f& F);

// Array.
template<class t> struct array: box<if_type<IsEqual<t,future<>>,managed_function<>,managed_array<t>>> {
	using base=if_type<IsEqual<t,future<>>,managed_function<>,managed_array<t>>;

	// Copying construtors.
	array(tuple<> = False);
	array(const array& A):
		box<base>(A) {}
	array(const pin<array>& A):
		box<base>(A) {}
	array(construct_payload,nat Payload):
		box<base>(construct_payload{},Payload) {}
	template<class u> requires(IsEqual<t,future<>>) array(const array<u>& US):
		box<base>(US) {}
	template<class u> requires(IsAny<u> && (IsEqual<t,future<>> && IsSubtype<u,array<>> /*|| IsEqual<exposed_function_type<u>,array>*/)) array(const u& U):
		box<base>(reinterpret_cast<const base&>(*kernel::CastManaged(U))) {}

	// Allocating constructors.
	template<class u> requires(!IsEqual<t,future<>> && IsEqual<exposed_type<u>,array>) array(const u& U):
		box<base>(ToAny(U)) {}
	template<class u> requires(!IsEqual<t,future<>> && IsSubtype<u,array> && !IsEqual<exposed_type<u>,array>) array(const u& U):
		box<base>(construct_boxer<managed::boxer<u,array>>{},U) {}

	// We have these explicit constructors because ExposeValue blocks them from being used implicitly.
	template<class=void> requires(IsEqual<t,char8>) explicit array(const char*  s): array(Verse::For(ExposeLength(s),[s](nat i)->char8 {return s[i];})) {}
	template<class=void> requires(IsEqual<t,char8>) explicit array(const char8* s): array(Verse::For(ExposeLength(s),[s](nat i)->char8 {return s[i];})) {}

	// Constructing elements.
	template<class f,class=decltype(declval<f>()(declval<t*>()))> explicit array(construct_flat,nat N,const f& F): box<base>(construct_no_init{}) {
		if(N>0)
			new(this)box<managed_array_flat<t>>(N,F);
		else
			new(this)array(False);
	}
	template<class... ps,class=decltype((t(declval<ps>()),...))> explicit array(const ps&... PS): 
		// WARNING: The 1-element case array(const p&) resolves to the copy constructor when p's type belongs to both array<t> and t,
		// for example with array<any>. !!Can fix using initializer_list?
		array(construct_flat{},sizeof...(PS),[&](t* AS) {EmplaceElements(AS,PS...);}) {
	}
	template<class... ps> explicit array(construct_elements,const ps&... PS): 
		array(construct_flat{},sizeof...(PS),[&](t* AS) {EmplaceElements(AS,PS...);}) {}
	template<class=void> requires(IsSubtype<nat,t>) explicit array(construct_range,nat ContainerLength):
		box<base>(construct_boxer<managed::boxer<range_span,array>>{},ContainerLength) {}

	// Operations.
	array& operator+=(const array<t>& b);
	explicit operator bool() const {return  Length(*this);}
	bool     operator!    () const {return !Length(*this);}
	t operator()(nat i) const {
		// Providing exact operator() rather than inheriting box's variadic template so callable works.
		// Clever inheritance approach makes this a fiasco.
		auto F=pin(static_cast<const box<base>&>(*this));
		if constexpr(IsEqual<t,future<>>)
			return F->OnCallAssert(i);
		else
			return F->operator()(i);
	}
	t operator[](nat i) const {
		return (*this)(i);
	}
	array Slice(nat Start,nat Stop) const {
		VERSE_ASSERT(Start<=Stop && Stop<=Length(*this));
		return Verse::For(Stop-Start,[&](nat i) {return operator[](Start+i);});
	}
	array Slice(nat Start) const {
		return Slice(Start,Length(*this));
	}
	array EndSlice(nat Stop,nat Start) const {
		auto n=Length(*this);
		return Slice(n-Start,n-Stop);
	}
	array EndSlice(nat Stop) const {
		auto n=Length(*this);
		return Slice(0,n-Stop);
	}
	t End(nat i) const {
		return (*this)[Length(*this)-1-i];
	}
	template<class f,class r=callable_image<f,t>> if_type<IsEqual<r,void>,void,array<r>> For(const f& F) const {
		auto n=Length(*this);
		if constexpr(!IsEqual<r,void>)
			return array<r>(construct_function{},n,[&](nat i) {return F(this->operator[](i));});
		for(nat i=0; i<n; i++)
			F(operator[](i));
	}
	array Reverse() const {
		auto n=Length(*this);
		return array(n,[&](nat i) {return operator[](n-i-1);});
	}
	t Concatenate() const {
		if(Length(*this)>0)
			return operator[](0)+Slice(1).Concatenate();
		return False;
	}
	template<class t0=t> requires(IsComparable<t0>()) option<nat> Find(const t& Value,nat i=0) {
		for(auto n=Length(*this); i<n; i++)
			if(operator[](i)==Value)
				return Truth(i);
		return False;
	}
	template<class t0=t> requires(IsComparable<t0>()) option<nat> Finds(const array& Sequence,nat i=0) {
		for(nat j,n=Length(Sequence),m=Length(*this)-n; i<m; i++) {
			for(j=0; j<n && operator[](i+j)==Sequence[j]; j++);
			if(j==n)
				return Truth(i);
		}
		return False;
	}
	template<class by=t(*)(const t&)> requires requires(t* TP,int64 i,by By) {Sort(TP,i,By);}
	array Sort(by By=Internal::DefaultSort<t>) const {
		auto n=Length(*this);
		return array(construct_flat{},n,[&](t* BS) {
			for(nat i=0; i<n; i++)
				new(&BS[i])t(this->operator[](i));
			Verse::Sort(BS,n,By);
		});
	}
private:
	template<class f,class=decltype(t(declval<f>()(declval<nat>())))> explicit array(construct_function,nat n,const f& F):
		array(construct_flat{},n,[&](t* Elements) {for(nat i=0; i<n; i++) new(Elements+i)t(F(i));}) {}
	template<class> friend struct array;
	template<class f,class r> friend if_type<IsEqual<r,void>,void,array<r>> For(nat n,const f& F);
};
array                      (                                 )->array<falsity>;
array                      (const char*                      )->array<char8>;
array                      (const char8*                     )->array<char8>;
template<class    f > array(construct_function,nat,const f& F)->array<decltype(F(declval<nat>()))>;
template<class... ps> array(construct_elements,const ps&...  )->array<common_type<ps...>>;
template<class... ps> array(const ps&...                     )->array<common_type<ps...>>;
template<class    p > array(const p&                         )->array<p>; // Explicit 1-ary case required by clang, don't know why.

template<class t,class u,class rt=common_type<t,u>> array<rt> operator+(const array<t>& a,const array<u>& b) {
	nat an=Length(a),bn=Length(b),n=an+bn;
	//template<class t> struct managed_array_rope: managed_array<t> {
	//array<t> Heads,Tails;
	if(n>0) {
		//if(n>30) // Figure out how to build a rope that roughly balances
		//static nat m=0; if(n>m) m=n,Print(n);
		return For(n,[&](nat i)->rt {
			if(i<an) return a(i);
			else     return b(i-an);
		});
	}
	else return array<rt>(False);
}
template<class t> array<t>& array<t>::operator+=(const array<t>& bs) {
	auto as=*this;
	return *new(this)array(as+bs);
}
string operator+(const string&,const char*);
string operator+(const string&,const char8*);

// Array iteration.
struct array_end_iterator {};
template<class t> struct array_iterator {
	array<t> as;
	nat Index, ContainerLength;
	bool operator==(array_end_iterator) {return Index==ContainerLength;}
	void operator++() {Index++;}
	t operator*() const {return as[Index];}
};
template<class t> auto begin(const array<t>& as) {return array_iterator<t>{as,0,Length(as)};}
template<class t> auto end  (const array<t>& as) {return array_end_iterator();}

// Managed array base.
template<class t> struct managed_array: managed_map<nat,t> {
	constexpr explicit managed_array(nat Count0=0): managed_map<nat,t>(Count0) {}
	current_step OnCallStep(const future<>& R,const future<>& P,const step& Step0) const override;
	bool         OnIsArray() const override {return true;}
	array<nat> Keys() const override {return array<nat>(construct_range{},this->ContainerLength);}
};
template<class t> nat ExposeLength(const managed_array<t>& a) {return a.ContainerLength;}

// Array of flat memory.
template<class t> struct alignas(alignof(t)>=8? alignof(t): 8) managed_array_flat: managed_array<t> {
	managed_array_flat(const managed_array_flat& A): managed_array<t>(A) {
		for(nat i=0; i<managed_function<>::ContainerLength; i++)
			new((t*)(this+1)+i)t(A(i));
	}
	~managed_array_flat() {
		for(nat i=0,n=this->ContainerLength; i<n; i++) 
			((t*)(this+1))[i].~t();
	}
	future<> OnCallAssert(const future<>& P) const override {
		return future<>((*this)(CastIndex(P)));
	}			
	t operator()(const nat& i) const override {
		if(i<managed_function<>::ContainerLength)
			return ((t*)(this+1))[i];
		VERSE_ERR("index out of bounds");
	}
	template<class f> requires requires(f F,t* TP) {F(TP);} managed_array_flat(nat n,const f& F): managed_array<t>(n) {
		F((t*)(this+1));
	}
	template<class f> static nat OnConstructorSize(nat n,const f&) {
		return sizeof(t)*n;
	}
	// Broken in Visual C++. ~managed_array_flat() requires(!HasDestructor<t>)=default;
};

// Array as rope (a binary tree we can maintain sparsely).
/*template<class t> struct managed_array_rope: managed_array<t> {
	array<t> Heads,Tails;
	future<> OnCallAssert(const future<>& P) const override {
		return (*this)(CastIndex(P));
	}			
	t operator() (const nat& i) const override {
		if(auto AS=pin(Heads); i<AS->ContainerLength)
			return AS(i);
		else if(auto BS=pin(Tails); true)
			return BS(i-Heads->ContainerLength);
	}
	managed_array_rope(const array<t>& Heads0,const array<t>& Tails0): Heads(Heads0), Tails(Tails0) {}
};*/

// Array functions.
template<class f,class r> if_type<IsEqual<r,void>,void,array<r>> For(nat n,const f& F) {
	if constexpr(IsEqual<r,void>)
		for(nat i=0; i<n; i++)
			F(i);
	else
		return array(construct_function{},n,F);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// A fully known comparable value of any type.

// All values of comparable can be compared for at least equality.
struct comparable: any {
	VERSE_FORCE_INLINE comparable(const comparable& C): any(C) {}
	template<class u> requires(IsSubtype<u,comparable>) VERSE_FORCE_INLINE comparable(const u& U): any(U) {}
	VERSE_FORCE_INLINE comparable(construct_payload,nat p): any(construct_payload{},p) {}
	template<class u> requires(IsSubtype<u,comparable>) auto IsA   () const {return Verse::IsA   <u>(*this);}
	template<class u> requires(IsSubtype<u,comparable>) auto Cast  () const {return Verse::Cast  <u>(*this);}
	template<class u> requires(IsSubtype<u,comparable>) auto Coerce() const {return Verse::Coerce<u>(*this);}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Transactional variables.

// Assist with traversing a nonvalue, an inline future<t> or var<t> stored within managed memory.
struct kernel::non_value_accessor {
	pin<future<>> Source,Target;
	nat NonValueType;
	non_value_accessor(const future<>& Source0);
	void Advance();
	bool CompareExchangeInterior(const future<>& NewValue,nat NewNonValueType);
	bool CompareExchangeVar(const future<>& Value);
	//bool RedirectVar(const future<>& NewVar);
	void ReadVar() const;
	future<> Waiter() const;
};

// Typed mutable variable.
template<class t> struct var: any {
	var(const var& a): any(a) {}
	template<class... ps> requires requires(ps... PS) {t{PS...};} explicit var(const ps&... PS): any(construct_new_var{},t{PS...}) {}
	t operator*() const {
		kernel::non_value_accessor Work(*this);//can remove is-copyable overhead if pinned_fwd ctor specialized for t
		Work.ReadVar();
		return Presume<t>(TypedValue(Work));
	}
	auto operator->() const {
		kernel::non_value_accessor Work(*this);
		Work.ReadVar();
		if constexpr(IsAny<t>)
			return TypedValue(Work);
		else
			return Presume<t>(TypedValue(Work)); // Return t.
	}
	const var& operator=(const var&) const=delete; // Would be unsound when overwriting inline var.
	const var& operator=(const t& new_value) const {
		kernel::non_value_accessor Work(*this);
		while(!Work.CompareExchangeVar(new_value));
		return *this;
	}
	const var Clone() const {
		return var(**this);
	}
	template<class f> auto Update(const f& F) const->decltype(t(F(declval<t>()))) {
		kernel::non_value_accessor Work(*this);
		for(;;)
			if(auto Value=F(Presume<t>(TypedValue(Work))); Work.CompareExchangeVar(Value))
				return Value;
	}
	template<class f> t Exchange(const f& F) const {
		kernel::non_value_accessor Work(*this);
		for(;;)
			if(auto Value=Presume<t>(TypedValue(Work)); Work.CompareExchangeVar(F(Value)))
				return Value;
	}

	template<class=void> requires requires(t A    ) {A+1;} t operator++(int)        const {return Exchange([&](const t& a) {return a+1;});}
	template<class=void> requires requires(t A    ) {A-1;} t operator--(int)        const {return Exchange([&](const t& a) {return a-1;});}
	template<class=void> requires requires(t A,t B) {A+1;} t operator++()           const {return Update  ([&](const t& a) {return a+1;});}
	template<class=void> requires requires(t A,t B) {A-1;} t operator--()           const {return Update  ([&](const t& a) {return a-1;});}
	template<class=void> requires requires(t A,t B) {A+B;} t operator+=(const t& b) const {return Update  ([&](const t& a) {return a+b;});}
	template<class=void> requires requires(t A,t B) {A-B;} t operator-=(const t& b) const {return Update  ([&](const t& a) {return a-b;});}
	template<class=void> requires requires(t A,t B) {A*B;} t operator*=(const t& b) const {return Update  ([&](const t& a) {return a*b;});}
	template<class=void> requires requires(t A,t B) {A/B;} t operator/=(const t& b) const {return Update  ([&](const t& a) {return a/b;});}
	template<class=void> requires requires(t A,t B) {A%B;} t operator%=(const t& b) const {return Update  ([&](const t& a) {return a+b;});}
	template<class=void> requires requires(t A,t B) {A&B;} t operator&=(const t& b) const {return Update  ([&](const t& a) {return a&b;});}
	template<class=void> requires requires(t A,t B) {A^B;} t operator^=(const t& b) const {return Update  ([&](const t& a) {return a^b;});}
	template<class=void> requires requires(t A,t B) {A|B;} t operator|=(const t& b) const {return Update  ([&](const t& a) {return a|b;});}
	static const auto& TypedValue(const kernel::non_value_accessor& Work) {
		return reinterpret_cast<const pin<exposed_type<t>>&>(Work.Target);
	}
};
template<class t,class u> equality_ordering CompareDynamic(const var<t>& TP,const var<u>& UP) {
	kernel::non_value_accessor TW(TP), UW(UP);
	return equality_ordering(TW.Source.Payload==UW.Source.Payload);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Box implementation.

// Ensure zero-sized structures fall within allocation's bounds.
namespace Internal {
	template<class t,class base,class boxer> const t* BoxedPointerHelper(const boxer& Wrapper) {
		if constexpr(sizeof(boxer)!=sizeof(base))
			return &static_cast<const t&>(Wrapper);
		else
			return (t*)(void*)&Wrapper;
	}
};

// Wrap a value as a class deriving from managed.
template<class v> struct managed_value: managed {
	v Value;
	template<class... ps> constexpr managed_value(const ps&... PS): Value(PS...) {}
	operator const v&() const {return Value;}
};
namespace Internal {
	template<class t> bool OnIsComparableHelper(const t& T) {
		if constexpr(IsComparable<t>())
			return true;
		else if constexpr(HasExposeIsComparable<t>)
			return ExposeIsComparable(T);
		else
			return false;
	}
	template<class t,class self> dynamic_ordering OnCompareHelper(const t& T,const self* Self,const pin<any>& B) {
		auto BMP=kernel::CastManaged(B);
		if constexpr(IsComparable<t>()&&!IsUnique<t>)
			if(auto BVP=dynamic_cast<const self*>(BMP))
				return ExposeCompare(T,static_cast<const t&>(*BVP));
			else
				return dynamic_ordering(false);
		else if(IsUnique<t> || BMP&&BMP->OnIsUnique())
			return dynamic_ordering(Self==BMP); // Correct even if BMP is nullptr.
		else
			return dynamic_ordering();
	}
	template<class t,class self> nat OnHashHelper(const t& T,const self* Self) {
		if constexpr(IsUnique<t>)
			return PayloadHash(nat(Self));
		else if constexpr(HasExposeHash<t>)
			return ExposeHash(T);
		else
			VERSE_UNIMPLEMENTED;
	}
	template<class t,class self> string OnToStringHelper(const t& T,const self* Self);
}

// Wrapper around all managed allocations.
template<class t,class... bases> struct managed_box final: bases..., managed_value_class<t> {
	template<class ...ps> constexpr managed_box(const ps&... PS): managed_value_class<t>(PS...) {}
	const t*         BoxedPointer  (                 ) const          {return Internal::BoxedPointerHelper<t,managed>(*this);}
	void             OnDestructor  (                 )       override {this->~managed_box();}
	void             OnCopy        (managed* Target  ) const override {new(Target)managed_box(*this);}
	const char*      OnNativeName  (                 ) const override {return NativeNameOfType<t>;}
	string           OnToString    (                 ) const override {return Internal::OnToStringHelper    (static_cast<const t&>(*this),this);}
	path             OnSignature   (                 ) const override;
	bool             OnIsUnique    (                 ) const override {return IsUnique<t>;}
	bool             OnIsComparable(                 ) const override {return Internal::OnIsComparableHelper(static_cast<const t&>(*this));}
	dynamic_ordering OnCompare     (const pin<any>& B) const override {return Internal::OnCompareHelper     (static_cast<const t&>(*this),this,B);}
	nat              OnHash        (                 ) const override {return Internal::OnHashHelper        (static_cast<const t&>(*this),this);}
};
template<class t,class r,class... ps,class... bases> struct function_box<t,r(ps...),bases...> final: bases..., managed_value_class<t> {
	// Same as above but can be called, like a lambda. Our virtual operator() routes calls to the lambda.
	//static_assert(sizeof...(bases) || IsDerived<t,managed_function<r(ps...)>>); // Disabled to support return type covariance for parameter being internal_falsity.
	template<class ...qs> constexpr function_box(const qs&... QS): managed_value_class<t>(QS...) {}
	const t*         BoxedPointer  (                 ) const          {return Internal::BoxedPointerHelper<t,managed_function<r(ps...)>>(*this);}
	void             OnDestructor  (                 )       override {this->~function_box();}
	void             OnCopy        (managed* Target  ) const override {new(Target)function_box(*this);}
	const char*      OnNativeName  (                 ) const override {return NativeNameOfType<t>;}
	string           OnToString    (                 ) const override {return Internal::OnToStringHelper    (static_cast<const t&>(*this),this);}
	bool             OnIsUnique    (                 ) const override {return IsUnique<t>;}
	bool             OnIsComparable(                 ) const override {return Internal::OnIsComparableHelper(static_cast<const t&>(*this));}
	dynamic_ordering OnCompare     (const pin<any>& B) const override {return Internal::OnCompareHelper     (static_cast<const t&>(*this),this,B);}
	nat              OnHash        (                 ) const override {return Internal::OnHashHelper        (static_cast<const t&>(*this),this);}
	r operator()(const ps&... PS) const override {
		if constexpr(sizeof...(bases))
			return Callable(static_cast<const t&>(*this))(PS...); // Call into user type.
		else
			return t::operator()(PS...); // Call non-virtually in t subtype of managed_function<r(ps...)>.
	}
};
template<class t,class k,class v,class... bases> struct map_box final: bases..., managed_value_class<t> {
	// Same as above but map.
	//static_assert(sizeof...(bases) || IsDerived<t,managed_map<k,v>>);
	template<class ...ps> constexpr map_box(const ps&... PS): managed_value_class<t>(PS...) {managed_function<>::ContainerLength=Length(static_cast<const t&>(*this));}
	const t*    BoxedPointer(               ) const          {return Internal::BoxedPointerHelper<t,managed_map<k,v>>(*this);}
	void        OnDestructor(               )       override {this->~map_box();}
	void        OnCopy      (managed* Target) const override {new(Target)map_box(*this);}
	const char* OnNativeName(               ) const override {return NativeNameOfType<t>;}
	string      OnToString  (               ) const override {VERSE_UNIMPLEMENTED;}//!!todo and test
	bool        OnIsUnique  (               ) const override {return false;}
	bool        OnIsMap     (               ) const override {return true;}
	bool        OnIsSet     (               ) const override {
		return IsSet<t>; //also must check dynamically; it's decidable
	}
	bool OnIsComparable() const override {
		VERSE_UNIMPLEMENTED;
	}
	dynamic_ordering OnCompare(const pin<any>& B) const override {
		VERSE_UNIMPLEMENTED;
	}
	nat OnHash() const override {
		//return Internal::HashDynamicMap(static_cast<const t&>(*this)); // Needs hash-generic begin/end or somesuch.
		VERSE_UNIMPLEMENTED;
	}
	v operator()(const k& Key) const override {
		if constexpr(sizeof...(bases))
			return Callable(static_cast<const t&>(*this))(Key); // Call into user type.
		else
			return t::operator()(Key); // Call non-virtually in v subtype of managed_function<r(ps...)>.
	}
	array<k> Keys() const override {
		return ExposeKeys(static_cast<const t&>(*this));
	}
};
template<class t,class v,class... bases> struct array_box final: bases..., managed_value_class<t> {
	// Same as above but array.
	//static_assert(sizeof...(bases) || IsDerived<t,managed_array<v>>);
	template<class ...ps> constexpr array_box(const ps&... PS): managed_value_class<t>{PS...} {managed_function<>::ContainerLength=Length(static_cast<const t&>(*this));}
	const t*    BoxedPointer(               ) const          {return Internal::BoxedPointerHelper<t,managed_array<v>>(*this);}
	void        OnDestructor(               )       override {this->~array_box();}
	void        OnCopy      (managed* Target) const override {new(Target)array_box(*this);}
	const char* OnNativeName(               ) const override {return NativeNameOfType<t>;}
	string      OnToString  (               ) const override {
		auto VP=&static_cast<const managed&>(*this); // TODO: Figure out how to express more cleanly.
		return ToString(reinterpret_cast<const array<>&>(VP));
	}
	bool        OnIsUnique  (            ) const override {return false;}
	bool        OnIsMap     (            ) const override {return true;}
	bool        OnIsSet     (            ) const override {
		//must check dynamically
		return true;
	}
	bool OnIsComparable() const override {
		if(IsComparable<v>())
			return true;
		for(nat i=0; i<this->ContainerLength; i++)
			if(!IsComparableDynamic((*this)(i)))
				return false;
		return true;
	}
	dynamic_ordering OnCompare(const pin<any>& B) const override {
		if(auto BSO=Cast<array<>>(B); !BSO)
			return dynamic_ordering(false);
		else if(auto BS=BSO.Coerce(); BS->ContainerLength!=this->ContainerLength)
			return dynamic_ordering(false);
		else
			for(nat i=0; i<this->ContainerLength; i++)
				if(auto c=CompareDynamic(future<>((*this)(i))/*For explicit future<> ctors but loses some efficiency*/,BS[i]); c!=nullptr)
					return c;
		return dynamic_ordering(true);
	}
	nat OnHash() const override {
		return Internal::HashDynamicArray(static_cast<const t&>(*this));
	}
	v operator()(const nat& Index) const override {
		if constexpr(sizeof...(bases))
			return Callable(static_cast<const t&>(*this))(Index); // Call into user type.
		else
			return t::operator()(Index); // Call non-virtually in t subtype of managed_function<r(ps...)>.
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Immediate casting.

// Casting combinator, optimizes special cases.
template<class target> concept HasExposeEncase = requires(target* Target,pin<any> Source,bool* Finished) {
	bool(expose<target>::ExposeCast(Target,Source,Finished));
};
template<class target> concept HasExposeCoerce = requires(pin<any> Source) {
	target(expose<target>::ExposeCoerce(Source));
};
template<class t> bool IsResolved(const pin<t>& T) {
	if constexpr(IsFuture<t>)
		return kernel::IsResolvedFuture(kernel::AdvancePinned(T));
	else
		return true;
}
template<class target,class source> bool Encase(target* Target,const source& Source,bool* Finished) {
	if constexpr(IsSubtype<source,target>)
		return new(Target)target(Source), true;
	else if(const auto& B=pin(Source); !IsResolved(B))
		return Finished? *Finished=false: false;
	else
		return expose<target>::ExposeCast(Target,reinterpret_cast<const pin<any>&>(B),Finished);
}
template<class t,class target,class source> target Coerce(const source& Source) {
	if constexpr(IsSubtype<source,target>)
		return target(Source);
	else if(const auto& B=pin(Source); !IsResolved(B))
		VERSE_ERR("Coerce unready");
	else if constexpr(HasExposeCoerce<target>)
		return expose<target>::ExposeCoerce(reinterpret_cast<const pin<any>&>(B));
	else if(auto D=option<target>(Internal::construct_cast{},Source))
		return D.PresumeReference(); // Extra copy, hence composite types should provide ExposeCoerce.
	else
		VERSE_ERR("Coerce failed");
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Interfaces.

struct disposable {
	bool IsLive;
	disposable(): IsLive(true) {}
	virtual void Dispose() {
		IsLive=false;
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Text and code.

// General operations.
bool IsValidUTF8(const string& s);
nat ParseUTF8(const string& S,nat Index,char32& Char32);

// Declare all ExposeToString in this namespace.
string ExposeToString(char);
string ExposeToString(char8);
string ExposeToString(char16);
string ExposeToString(char32);
string ExposeToString(const string8&);
string ExposeToString(const string16&);
string ExposeToString(const string32&);
string ExposeToString(const integer&);
string ExposeToString(nat);
string ExposeToString(int64);
string ExposeToString(nat32);
string ExposeToString(int32);
string ExposeToString(const array<>&);
string ExposeToString(const path&);
string ExposeToString(const locus&);
string ExposeToString(const error&);
string ExposeToString(const char*);
string ExposeToString(const char8*);
string ExposeToString(span<char8>);
string ExposeToString(fx Fx);
string ExposeToString(const box<syntax>&);
template<class t> string ExposeToString(const array<>&);
template<class t> string ExposeToString(const option<t>&);
template<class t> string ExposeToString(const var<bag<t>>&);
template<class k,class v> string ExposeToString(const var<map<k,v>>&);
template<class t> concept HasExposeToString = requires(t T) {string(ExposeToString(T));};
template<class t> concept Printable         = requires(t T) {string(ToString(T));};

// ToString and helpers.
string ToString();
string ToString(const future<>&);
template<class t> requires(HasExposeToString<t>) string ToString(const t& T) {
	return ExposeToString(T);
}
template<class p,class q,class... rs> string ToString(const p& P,const q& Q,const rs&... RS) {
	return ToString(P)+ToString(Q,RS...);
}
string ToStringBase(const integer&,nat Base=10,nat MinDigits=1);

// ToCode.
string ToCode(char);
string ToCode(char8);
string ToCode(char16);
string ToCode(char32);
string ToCode(nat);
string ToCode(int64);
string ToCode(nat32);
string ToCode(int32);
string ToCode(fx Fx,fx DefaultFx=effects);
string ToCode(const path&);
string ToCode(const integer&);
string ToCode(const rational&);
string ToCode(const box<syntax>&);
template<class ...ts> string ToCode(const tuple<ts...>& a) {
	return ToCode(any(a));
}
template<class t> string ToCode(option<t> a) {
	return a? ToString("truth{",ToCode(a.Coerce()),"}"): "false"_VS;
}
string ToCode(const string&);
string ToCode(const array<>&);
string ToCode(const future<>&);

// Low-level conversion and access.
struct string_as_utf8 {
	string_as_utf8(string chs);
	~string_as_utf8();
	nat    Length;
	char8* UTF8;
private:
	char8 Storage[256];
};
struct string_as_utf16 {
	string_as_utf16(string);
	~string_as_utf16();
	nat     Length;
	char16* UTF16;
private:
	char16 Storage[256];
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Errors and error location description.

struct path {
	string Value;
	explicit path(const string& Value0);
	friend total_ordering ExposeCompare(const path& P0,const path& P1);
	friend nat ExposeHash(const path& P);
};
struct locus {
	string Filename; nat StartLine,StartColumn,StopLine,StopColumn;
	locus(const string& Filename0=False): Filename(Filename0), StartLine(0), StartColumn(0), StopLine(0), StopColumn(0) {}
	locus(const string& Filename0,nat StartLine0,nat StartColumn0,nat StopLine0,nat StopColumn0):
		Filename(Filename0), StartLine(StartLine0), StartColumn(StartColumn0), StopLine(StopLine0), StopColumn(StopColumn0) {}
	explicit operator bool() const {
		return Filename || StartLine || StartColumn || StopLine || StopColumn;
	}
	locus Else(const locus& Next) const {
		return *this? *this: Next;
	}
	friend equality_ordering ExposeCompare(const locus& A,const locus& B);
};
struct error {
	locus  Locus;
	string ErrorCode;
	nat8   Priority;
	string Message, Internal;
	friend equality_ordering ExposeCompare(const error& A,const error& B);
};
struct syntax: managed {
	locus Locus; 
	syntax(const locus& Locus0): Locus(Locus0) {}
	struct atom;
	struct identifier;
	struct call;
	struct invoke;
	struct escape;
	struct clause;
};
[[noreturn]] void Err(const error&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Threading.

struct Internal::thread_startup {
	virtual ~thread_startup() {}
	virtual void ThreadStartup()=0;
	void PreThreadStartup();
};
template<class t,class f> future<t> RunThread(const f& F) {
	struct thread_startup_lambda: Internal::thread_startup {
		future<t> Result;
		f         F;
		thread_startup_lambda(const future<t>& Result0,const f& F0): Result(Result0), F(F0) {kernel::RunThread(this);}
		void ThreadStartup() override {Result.Resolve(F());}
	};
	future<t> Result;
	new thread_startup_lambda{Result,F}; // Can't touch this, as new thread owns startup and may delete it at any time.
	return Result;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Suspensions and Contexts.

// Suspensions.
struct suspension_managed: managed_function<current_step()> {
	bool  IsReady;        // Ready to run OnRunSuspensionStep.
	fx    SuspendedFx;    // Maximum set of effects this suspension's code may have.
	fx    WhenFx;         // Nested WhenReadyFx we're waiting for, if any.
	step  NextResumeStep; // When suspended, tracks resume order.
	explicit suspension_managed(fx SuspendedFx0=only_succeeds,fx WhenFx0=no_effects);
	void Suspend();
	void ReadySuspensionBatch();
	step ReadySuspensionStep(const step& Step0);
	current_step operator()() const;
	virtual current_step OnRunSuspensionStep(const step& Step0);
	virtual error OnDescribe() const;
	virtual fx OnRefineFx(bool& Narrowed) {return SuspendedFx;}
	friend string ExposeToString(const suspension_managed& S) {
		return ToString(S.OnDescribe());
	}
};
suspension SuspendStep(const function<current_step(const step&)>& F);

// Contexts.
struct context_managed: suspension_managed {
	nat8       IsCommitted, IsCommittable;
	const fx   AllowFx;
	fx         ParentPendingFx; // When in or below this context, stores all thread pending effects prior to this context.
	fx         LocalPendingFx;  // Effects pending locally in this context only up through the suspensions stored in ContextResumeStep.
	fx         StuckFx;         // Effects held locally in this context when stuck; only way out is failing. Orthogonal to HoldFx.
	fx         HoldFx;          // Hold these effects notionally after all of our suspensions; iterate-context only.
	const nat  Depth;
	suspension ContextLastSuspension;
	step       ContextResumeStep;

	// Constructor.
	context_managed(bool IsCommitted0,bool IsCommittable0,fx AllowFx0,fx HoldFx0,nat Depth0);

	// Overriadble interface.
	current_step OnRunSuspensionStep(const step& Step0) override;
	error OnDescribe() const override;
	virtual error OnDescribeSuspension(const suspension&,bool& Recurse) const;

	// Internal interface.
	virtual current_step ContextThrows(const locus& Locus,const future<>& A,const step& Step0) const {VERSE_UNEXPECTED;}
	virtual current_step ContextErrs(const error& E,const step& Step0) const {VERSE_UNEXPECTED;}
	virtual option<step> EnterContext(const char* What);

	// Helpers.
	template<class f,class... ps> auto Run(const f& F,const ps&... PS)->decltype(F(PS...));
	current_step RunStep(const step& Step0,const function<current_step(const step&)>& F);
	pin<iterate> Visibility() const;
	bool HasSuspensions() const;
	step TerminateStep(const step& Step0);
};
current_step RunSuspensionsStep(const step& Step0);

// Iteration contexts.
struct iterate_managed: context_managed {
	nat8                        IsReleased;
	step                        ParentStep;
	option<tuple<iterate,step>> IterateFork;

	// Constructors.
	iterate_managed(tuple<>,bool IsCommittable0,fx AllowFx0,nat Depth0,fx HoldFx0,const step& ParentStep0):
		context_managed(false,IsCommittable0,AllowFx0,HoldFx0,Depth0),
		IsReleased(false), ParentStep(ParentStep0) {}

	// Overridable interface.
	current_step OnRunSuspensionStep(const step& Step0) override;
	virtual void OnCommit();
	virtual void OnAbandon();
	current_step OnLeftStep();
	virtual void OnRefineIterateFx();

	// Internal interface.
	virtual current_step ContextSucceeds(const step& Step0) const;
	virtual current_step ContextFails(bool CommitWrites,const step& Step0) const;
	
	// Helpers.
	tuple<bool,fx> RefineIterateSuspendedFx(fx NewChildFx,bool UpdateHoldFx);
	virtual bool Stopped();
};
bool operator<=(const pin<context>& A,const pin<context>& B); // Expose properly when we have nicer order interface.
bool operator<(const pin<context>& A,const pin<context>& B); // Expose properly when we have nicer order interface.
current_step EnterRecursiveStep(const context& Target,const step& Step0);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Comparison.

template<nat i=0,class ...ts,class ...us> constexpr auto ExposeCompare(const tuple<ts...>& TS,const tuple<us...>& US) {
	if constexpr(i==sizeof...(ts) || i==sizeof...(us))
		return ExposeCompare(sizeof...(ts),sizeof...(us));
	else {
		const auto O = CompareDynamic(TS.template get<i>(),US.template get<i>());
		using      o = common_ordering<remove_const_ref<decltype(O)>,decltype(ExposeCompare<i+1>(TS,US))>;
		if(O!=nullptr)
			return o(O);
		else
			return o(ExposeCompare<i+1>(TS,US));
	}
}
template<class t> constexpr total_ordering ExposeCompare(const option<t>& a,const tuple<>& b) {
	return total_ordering(!a,true);
}
template<class t,class u> constexpr auto ExposeCompare(const option<t>& a,const option<u>& b)->decltype(CompareDynamic(declval<t>(),declval<u>())) {
	return !bool(a) || !bool(b)?
		CompareTotal(int8(bool(a)),int8(bool(b))):
		CompareDynamic(a.PresumeReference(),b.PresumeReference());
}
template<class t> bool IsComparableDynamic(const t& T) {
	if constexpr(IsComparable<t>())
		return true;
	else if(auto AO=Cast<any>(T))
		return IsComparableDynamic(static_cast<const pin<any>&>(AO.Coerce()));
	else
		return false;
}
template<class t,class u> auto CompareDynamic(const pin<array<t>>& AS,const pin<array<u>>& BS)->decltype(CompareDynamic(declval<t>(),declval<u>())) {
	nat an=Length(AS),bn=Length(BS),n=an<=bn? an: bn;
	for(nat i=0; i<n; i++)
		if(auto c=CompareDynamic(AS(i),BS(i)); c!=nullptr)
			return c;
	return CompareTotal(an,bn);
}
template<class t,class u> auto CompareDynamic(const array<t>& AS,const array<u>& BS)->decltype(CompareDynamic(declval<t>(),declval<u>())) {
	return CompareDynamic(pin(AS),pin(BS));
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Mutable sets and tables. Ordered by first insertion so it works for anything comparable, even if unordered.

template<class self,class k> struct mutable_base_entry {
	self *HashNext, *EntryNext, **EntryPreviousLink;
	k Key;
	mutable_base_entry(self* HashNext0,self** EntryPreviousLink0,const k& Key0,construct_flat=construct_flat{}):
		HashNext(HashNext0), EntryNext(*EntryPreviousLink0), EntryPreviousLink(EntryPreviousLink0), Key(Key0) {}
	mutable_base_entry(self* HashNext0,self** EntryPreviousLink0,const self* Source):
		HashNext(HashNext0), EntryNext(*EntryPreviousLink0), EntryPreviousLink(EntryPreviousLink0), Key(Source->Key) {}
	bool   IsRemoved()              const {return !EntryPreviousLink;}
	bool   IsForwarded()            const {return nat(HashNext)&1;}
	self*  Successor()              const {return (self*)(nat(HashNext)&~1ULL);}
	k      GetValue()               const {return Key;}
	void   EmplaceValue()           const {}
	void   ForwardEntry(self* NewSuccessor) {HashNext=(self*)((nat)NewSuccessor|1);}
};
template<class k> struct Internal::mutable_set_entry: mutable_base_entry<Internal::mutable_set_entry<k>,k> {
	using mutable_base_entry<Internal::mutable_set_entry<k>,k>::mutable_base_entry;
};
template<class k,class v> struct Internal::mutable_map_entry: mutable_base_entry<Internal::mutable_map_entry<k,v>,k> {
#if 1
	union {var<v> ValuePtr;};
	mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const k& Key0,construct_flat):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Key0) {}
	mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const mutable_map_entry* Self):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Self->Key), ValuePtr(*Self->ValuePtr) {}
	template<class... ps> mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const k& Key0,const ps&... PS):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Key0), ValuePtr(PS...) {}
	~mutable_map_entry() {ValuePtr.~var<v>();}
	v GetValue() const {return *ValuePtr;}
	template<class v1=v,class... ps> requires requires(ps... PS) {v(v1{PS...});} void EmplaceValue(const ps&... PS) {
		if constexpr(IsEqual<v1,v>)
			new(&ValuePtr)var<v>{PS...};
		else
			new(&ValuePtr)var<v>{v1(PS...)};
	}
#else
	union {v Value;};
	mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const k& Key0,construct_flat):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Key0) {}
	mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const mutable_map_entry* Self):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Self->Key), Value(Self->Value) {}
	template<class... ps> mutable_map_entry(mutable_map_entry* HashNext0,mutable_map_entry** TablePreviousLink0,const k& Key0,const ps&... PS):
		mutable_base_entry<Internal::mutable_map_entry<k,v>,k>(HashNext0,TablePreviousLink0,Key0), Value(PS...) {}
	~mutable_map_entry() requires(!HasDestructor<v>)=default;
	~mutable_map_entry() {Value.~v();}
	v GetValue() const {return Value;}
	template<class v1=v,class... ps> requires requires(ps... PS) {v(v1{PS...});} void EmplaceValue(const ps&... PS) {
		if constexpr(IsEqual<v1,v>)
			new(&Value)v{PS...};
		else
			new(&Value)v{v1(PS...)};
	}
#endif
};
template<class k,class v,class entry> struct managed_mutable_map: managed {
	static constexpr nat  PadCapacity = 2;
	entry                            **MapHash, *MapEntries, *MapFirst, **MapLastLink;
	nat                              MapCount, AddNext, Capacity;
	option<box<managed_mutable_map>> Forwarded; // Hold for GC because entries may rely on it.
	entry*                           HashPad[PadCapacity];
	explicit managed_mutable_map(nat Capacity0,nat Count0=0,nat AddNext0=0) {
		Init(Capacity0,Count0,AddNext0);
	}
	explicit managed_mutable_map(): managed_mutable_map(PadCapacity,0,PadCapacity) {}
	explicit managed_mutable_map(const managed_mutable_map& Source): managed(Source) {
		// Defer hash map calculation to OnCloned, where Keys may differ due to cloning.
		Init(Source.Capacity,Source.MapCount);
		for(auto e=Source.MapFirst; e; e=e->EntryNext)
			*MapLastLink = new(MapEntries + AddNext++)entry(nullptr,MapLastLink,e),
			MapLastLink  = &(*MapLastLink)->EntryNext;
	}
	~managed_mutable_map() {
		for(nat i=0; i<AddNext; i++) // Not driven by MapFirst/EntryNext as removed elements may persist through iterators.
			MapEntries[i].~entry();
	}
	void Init(nat Capacity0,nat Count0=0,nat AddNext0=0) {
		MapHash     = (entry**)((nat8*)this + sizeof(managed_mutable_map) - PadCapacity*sizeof(HashPad)                          );
		MapEntries  = (entry *)((nat8*)this + sizeof(managed_mutable_map) - PadCapacity*sizeof(HashPad) + Capacity0*sizeof(entry*));
		MapFirst    = nullptr;
		MapLastLink = &MapFirst;
		MapCount    = Count0;
		AddNext     = AddNext0;
		Capacity    = Capacity0;
		for(nat i=0; i<Capacity0; i++)
			MapHash[i]=0;
	}
	// Presumed broken in Visual C++. ~managed_mutable_map() requires(!HasDestructor<entry>)=default;
	void OnCloned(cloner&) override {
		for(auto e=MapFirst; e; e=e->EntryNext) {
			auto h=Hash(e->Key)&(Capacity-1); // Rehash as Keys may change during cloning.
			e->HashNext=MapHash[h];
			MapHash[h]=e;
		}
	}
	const char* OnNativeName()   const override       {return "mutable_map_base";}
	static nat  OnConstructorSize(nat Capacity,nat=0) {return sizeof(managed_mutable_map) + Capacity*sizeof(entry*) + Capacity*sizeof(entry);}
	static nat  OnConstructorSize()                   {return sizeof(managed_mutable_map) +          sizeof(entry*);}
};
template<class k,class v,class entry> struct mutable_map_base: any {
	using storage=box<managed_mutable_map<k,v,entry>>;
	static_assert(alignof(entry)<=alignof(managed_mutable_map<k,v,entry>));
	struct cursor {
		// Supports stable iteration over map we're arbitrarily mutating, via k->v^:mt instead of k->v:mt^.
		// A cursor remains valid and doesn't move when the element under it is removed.
		//    !! hence only ++ should forward, others should handle it being bad. Coerce vs Get.
		// A cursor on a key that's removed and re-added still sees the original, not the new one.
		storage Storage;      // Hold for GC for now.
		mutable entry* Entry; // Be var<v> Value, guaranteed interior in some entry, forwarding automatic.
		cursor(const mutable_map_base& Table0,entry* Entry0): Storage(*Table0.AsStorage()), Entry(Entry0) {}
		bool  operator==(const cursor& C)   const {if(Entry) Forward(); if(C.Entry) C.Forward(); return Entry==C.Entry;}
		bool  operator==(decltype(nullptr)) const {if(Entry) Forward(); return !Entry;}
		auto  operator*()                   const {VERSE_ENSURE(Entry); return tuple<k,cursor>{Entry->Key,*this};}
		explicit operator bool()            const {return Entry;} // What about PreviousLink?
		void  Forward()                     const {while(Entry && Entry->IsForwarded()) Entry=Entry->Successor();}
		k     Key()                         const {return Entry->Key;}
		v     ReadValue()                   const {Forward(); VERSE_ENSURE(Entry); return            Entry->GetValue() ;} // Want auto-increment?
		auto  operator->()                  const {Forward(); VERSE_ENSURE(Entry); return HelpMember(Entry->GetValue());}
		void  operator++()                  const {VERSE_ENSURE(Entry); do {Forward(); Entry=Entry->EntryNext;} while(Entry && Entry->IsRemoved());}
		void  Remove()                      const {VERSE_ENSURE(Entry); if(Entry->EntryPreviousLink) mutable_map_base(Storage).Remove(Key());} // To remove from hash.
		bool  Has()                         const {return Entry && Entry->EntryPreviousLink;}
		option<v> Get()                     const {return Entry && Entry->EntryPreviousLink? Truth(Entry->GetValue()): False;}
		template<class... ps> void Set(const ps&... PS) const {
			VERSE_ENSURE(Entry);
			return Storage.Context()->Run([&]{Entry->EmplaceValue(PS...);}); // Doesn't destruct existing.
		}
	};
	var<storage>&       AsStorage() {return reinterpret_cast<var<storage>&>(*this);}
	const var<storage>& AsStorage() const {return reinterpret_cast<const var<storage>&>(*this);}
	storage Rehash(const storage& S,nat NewCapacity,bool Forward) const {
		return Context()->Run([&]{
			auto NewStorage = storage(NewCapacity,S->MapCount);
			for(auto E=S->MapFirst; E; E=E->EntryNext) {
				auto h     = Hash(E->Key)&(NewCapacity-1);
				auto Entry = *NewStorage->MapLastLink = NewStorage->MapHash[h] = new(NewStorage->MapEntries + NewStorage->AddNext++)entry(NewStorage->MapHash[h],NewStorage->MapLastLink,E);
				NewStorage->MapLastLink = &(*NewStorage->MapLastLink)->EntryNext;
				if(Forward)
					E->ForwardEntry(Entry);
			}
			return NewStorage;
		});
	}
	void AddEnsureSpace(storage& S,nat& h,const k& Key) const {
		if(S->AddNext==S->Capacity) {
			nat  NewCapacity   = S->Capacity*2; // Container with add-remove cycle will continue to grow in capacity.
			auto NewStorage    = Rehash(S,NewCapacity,true);
			if(S->Capacity>managed_mutable_map<k,v,entry>::PadCapacity) 
				S->Forwarded   = Truth(NewStorage);
			h                  = Hash(Key)&(NewCapacity-1);
			AsStorage() = S    = NewStorage;
		}
		S->MapCount++;
	}
	entry* EndAllocate(storage& S,nat h,const k& Key) const {
		AddEnsureSpace(S,h,Key);
		auto Entry = S->MapHash[h] = *S->MapLastLink = new(S->MapEntries + S->AddNext++)entry(S->MapHash[h],S->MapLastLink,Key,construct_flat{});
		S->MapLastLink = &Entry->EntryNext;
		return Entry;
	}
	entry* StartAllocate(storage& S,nat h,const k& Key) const {
		AddEnsureSpace(S,h,Key);
		auto Entry = S->MapHash[h] = S->MapFirst = new(S->MapEntries + S->AddNext++)entry(S->MapHash[h],&S->MapFirst,Key,construct_flat{});
		if(S->MapLastLink==&S->MapFirst)
			S->MapLastLink=&Entry->EntryNext;
		return Entry;
	}
public:
	mutable_map_base();
	mutable_map_base(const storage& Storage0): any(var<storage>(Storage0)) {}

	// General var<t> interface.
	// TODO: Update,Exchange from var.
	mutable_map_base operator=(tuple<>) {
		return Context()->Run([&]{return *new(this)mutable_map_base;});
	}
	//TODO: mutable_map_base operator=(const map<k,v>& Table);

	// Specialized var<map<k,v>> interface.
	bool Has(const k& Key) const {
		auto S=pin(*AsStorage());
		for(auto e=S->MapHash[Hash(Key)&(S->Capacity-1)]; e; e=e->HashNext)
			if(e->Key==Key)
				return true;
		return false;
	}
	option<v> Get(const k& Key) const {
		auto S=pin(*AsStorage());
		for(auto e=S->MapHash[Hash(Key)&(S->Capacity-1)]; e; e=e->HashNext)
			if(e->Key==Key)
				return Truth(e->GetValue());
		return False;
	}
	v operator[](const k& Key) const {
		auto S=pin(*AsStorage());
		for(auto e=S->MapHash[Hash(Key)&(S->Capacity-1)]; e; e=e->HashNext)
			if(e->Key==Key)
				return e->GetValue();
		VERSE_ERR("map access coercion failed");
	}
	//template<class f,class... ps> requires requires(v V,ps... PS,f F) {v(PS...); v(F(V));}
	//void Update(const f& F,const ps&... PS) const;
	template<class v1=v,class... ps> auto GetInit(const k& Key,const ps&... PS) const->decltype(v(v1{PS...})) {
		auto S=pin(*AsStorage());
		nat h=Hash(Key)&(S->Capacity-1);
		entry* e;
		for(e=S->MapHash[h]; e; e=e->HashNext)
			if(e->Key==Key)
				break;
		if(!e) {
			e=EndAllocate(S,h,Key);
			Context()->Run([&]{
				e->template EmplaceValue<v1>(PS...);
			});
		}
		return e->GetValue();
	}
	template<class... ps> requires requires(entry Entry,ps... PS) {Entry.EmplaceValue(PS...);} bool Set(const k& Key,const ps&... PS) const {
		auto S      = pin(*AsStorage());
		nat  h      = Hash(Key)&(S->Capacity-1);
		bool Found  = false;
		entry* e;
		for(e=S->MapHash[h]; e; e=e->HashNext)
			if(e->Key==Key) {
				e->~entry();//!!thread race
				Found = true;
				goto Out;
			}
		e=EndAllocate(S,h,Key);
		Out: return Context()->Run([&]{e->EmplaceValue(PS...);}), Found;
	}
	template<class... ps> requires requires(entry Entry,ps... PS) {Entry.EmplaceValue(PS...);} bool SetStart(const k& Key,const ps&... PS) const {
		auto S      = pin(*AsStorage());
		nat  h      = Hash(Key)&(S->Capacity-1);
		bool Found  = false;
		entry* e;
		for(e=S->MapHash[h]; e; e=e->HashNext)
			if(e->Key==Key) {
				e->~entry();//!!thread race
				Found = true;
				goto Out;
			}
		e=StartAllocate(S,h,Key);
		Out: return Context()->Run([&]{e->EmplaceValue(PS...);}), Found;
	}
	option<v> Remove(const k& Key) const {
		auto S=pin(*AsStorage());
		nat h=Hash(Key)&(S->Capacity-1);
		for(entry** ep=&S->MapHash[h]; *ep; ep=&(*ep)->HashNext) {
			if(auto e=*ep; e->Key==Key) {
				*ep                                 = e->HashNext;
				*e->EntryPreviousLink               = e->EntryNext;
				if(e->EntryNext)
					e->EntryNext->EntryPreviousLink = e->EntryPreviousLink;
				else
					AsStorage()->MapLastLink        = e->EntryPreviousLink;
				e->EntryPreviousLink                = nullptr; // Identify as removed for iteration.
				AsStorage()->MapCount--;
				// No entry::~entry(), as entry may still be referenced by cursor.
				return Truth(e->GetValue());
			}
		}
		return False;
	}
	nat ReadCount() const {
		return AsStorage()->MapCount;
	}
	auto begin() const {return cursor(*this,AsStorage()->MapFirst);}
	auto end()   const {return nullptr;}
};
template<class k,class v> struct var<map<k,v>>: mutable_map_base<k,v,Internal::mutable_map_entry<k,v>> {
	using mutable_map_base<k,v,Internal::mutable_map_entry<k,v>>::mutable_map_base;
	using mutable_map_base<k,v,Internal::mutable_map_entry<k,v>>::operator=;
	var<map<k,v>> Clone() const {
		return var<map<k,v>>(this->Rehash(*this->AsStorage(),this->AsStorage()->Capacity,false));
	}
	const var<map<k,v>>& operator+=(const var<map<k,v>>& AT) const {
		for(auto[K,VC]:AT)
			this->Set(K,VC.Coerce());
		return *this;
	}
};
template<class k> struct var<bag<k>>: mutable_map_base<k,k,Internal::mutable_set_entry<k>> {
	using mutable_map_base<k,k,Internal::mutable_set_entry<k>>::mutable_map_base;
	using mutable_map_base<k,k,Internal::mutable_set_entry<k>>::operator=;
	var<bag<k>> Clone() const {
		return var<bag<k>>(this->Rehash(*this->AsStorage(),this->AsStorage()->Capacity,false));
	}
	const var<bag<k>>& operator+=(const var<bag<k>>& AS) const {
		for(auto[K,_]:AS)
			this->Set(K);
		return *this;
	}
};

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Globals.
// C++ inline static initialization order guarantees these lifetime will circumscribe lifetimes
// of user values dependent them, both for globals and thread locals.

inline              kernel                Kernel;
inline thread_local kernel::kernel_thread KernelThread{};

inline const suspension   NoSuspension     = StaticBox<suspension_managed>();
inline const step         BadStep          = StaticFunction([]()->current_step{VERSE_ERR("BadStep");});
inline const auto         TopContext       = StaticBox<iterate_managed>(False,false,top_allows,0,effects,BadStep);
inline pin<context>       Thread(TopContext); //!! Make thread_local, always distinct from TopContext.
namespace Internal {
	inline const auto FixStaticZero=(kernel::FixStatic(NoSuspension),kernel::FixStatic(BadStep),kernel::FixStatic(TopContext),0);
}
inline const step       LeaveIterateStep = StaticFunction([]()->current_step{return Internal::LeaveIterate()->OnLeftStep();});
inline const auto       FalseBox         = StaticBox<tuple<>>();
inline const auto       FalseOptionBox   = StaticBox<option<tuple<>>>();
inline const auto       TrueBox          = StaticBox<option<tuple<>>>(True);
inline const auto       FalseMutableMap  = StaticBox<managed_mutable_map<tuple<>,tuple<>,Internal::mutable_set_entry<tuple<>>>>();

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Basic stepping.

// Enter synchronously into any context.
template<class f,class... ps> auto context_managed::Run(const f& F,const ps&... PS)->decltype(F(PS...)) {
	auto SavedContext = Thread;
	bool SavedIsReady = IsReady;
	IsReady           = false;
	Thread            = *this;
	if constexpr(IsEqual<decltype(F(PS...)),void>) {
		F(PS...);
		Thread = SavedContext;
		if(IsReady)
			ReadyCommonContext(*this);
		else
			IsReady = SavedIsReady;
	}
	else {
		const auto& ResultPreventingCopyElision = F(PS...);
		Thread      = SavedContext;
		if(IsReady)
			ReadyCommonContext(*this);
		else
			IsReady = SavedIsReady;
		return ResultPreventingCopyElision;
	}
}

// Run all steps in the sequence specified by F.
void RunSteps(const function<current_step(const step&)>& F);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Asynchronous stepped operations.

// Iteration stepping.
template<class t,class f> current_step ForStep(const t& as,const step& Step0,const f& F,nat i=0) {
	if(i<Length(as))
		return F(as[i],[=]()->current_step {return ForStep(as,Step0,F,i+1);});
	else
		return Step0;
}
template<class f> current_step ForStep(nat n,const step& Step0,const f& F,nat i=0) {
	if(i<n)
		return F(i,[=]()->current_step {return ForStep(n,Step0,F,i+1);});
	else
		return Step0;
}

// Function maker, produces observable result while running.
template<class f>
requires requires(f F,future<> Future,step Step) {current_step(F(Future,Future,Step));}
function<> MakeStepFunction(const f& F) {
	struct managed_step_function: managed_function<> {
		f F;
		future<> OnCallAssert(const future<>& P) const override {
			VERSE_ERR("managed_step_function.OnCallAssert coverage");
			future<> R;
			RunSteps([=,Self=box(*this)](const step& Step0)->current_step {
				return Self->OnCallStep(R,P,Step0);
			});
			return R;
		}
		current_step OnCallStep(const future<>& R,const future<>& P,const step& Step0) const override {
			return F(R,P,Step0);
		}
		managed_step_function(const f& F0): managed_function<>(), F(F0) {}
	};
	return box<managed_step_function>(F);
}

// Suspension waiting for a future.
struct when_future_suspension: suspension_managed {
	context  FutureContext;        // Thread the awaited future resides in.
	future<> SuspensionFuture;     // The awaited future.
	future<> FutureNextSuspension; // Next link in future's list of all waiting suspensions.
	when_future_suspension(const future<>& SuspensionFuture0,const future<>& FutureNextSuspension0,fx Fx0): 
		suspension_managed(Fx0), FutureContext(SuspensionFuture0.Context()), SuspensionFuture(SuspensionFuture0), FutureNextSuspension(FutureNextSuspension0) {}
	void OnCloned(cloner&);
};

// Wait for a future to become ready, then step into handler.
template<class describe,class t,class callback>
requires requires(describe Describe,callback Callback,t T,step Step) {error(Describe()); current_step(Callback(T,Step));}
current_step WhenResolveStep(const describe& Describe,fx Fx,const future<t>& Source,const step& Step0,const callback& Callback) {
	static_assert(!IsFuture<t>);
	struct when_step_suspension: when_future_suspension {
		describe Describe;
		callback Callback; // Must be captured and stored here because incoming value may be stack-allocated.
		when_step_suspension(const future<>& SuspensionFuture0,const future<>& FutureNextSuspension0,const describe& Describe0,const callback& Callback0,fx Fx0):
			when_future_suspension(SuspensionFuture0,FutureNextSuspension0,Fx0), Describe(Describe0), Callback(Callback0) {}
		current_step OnRunSuspensionStep(const step& Step1) override {return Callback(Presume<t>(SuspensionFuture),Step1);}
		error OnDescribe() const override {return Describe();}
	};
	kernel::non_value_accessor Work(Source);
	if(kernel::IsResolvedFuture(Work.NonValueType))
		return Callback(Presume<t>(Source),Step0);
	auto Suspension=box<when_step_suspension>(Work.Source,Work.Waiter(),Describe,Callback,Fx);
	if(!kernel::AddSuspension(Work,Suspension))
		return Suspension->Suspend(), Step0;
	else
		return Suspension->OnRunSuspensionStep(Step0);
}
template<class describe,class t,class callback>
requires requires(callback Callback,t T,step Step) {current_step(Callback(T,Step));}
auto WhenResolveStep(const describe&,fx Fx,const t& T,const step& Step0,const callback& Callback)->decltype(current_step(Callback(T),Step0)) {
	return Callback(T,Step0);
}

// Wait for future to become ready then run F.
template<bool Batch=false,class describe,class t,class f,class r=callable_image<f,t>>
requires requires(describe Describe) {error(Describe());}
if_type<IsEqual<r,void>,void,future<remove_future<r>>> WhenResolve(const describe& Describe,fx Fx,const future<t>& Source,const f& F) {
	static_assert(!IsFuture<t>);
	struct when_suspension: when_future_suspension {
		describe                                                  Describe;
		if_type<IsEqual<r,void>,tuple<>,future<remove_future<r>>> Result;
		f                                                         F;
		when_suspension(const future<>& SuspensionFuture0,const future<>& FutureNextSuspension0,const describe& Describe0,const f& F0,fx Fx0): 
			when_future_suspension(SuspensionFuture0,FutureNextSuspension0,Fx0), Describe(Describe0), F(F0) {}
		current_step OnRunSuspensionStep(const step& Step0) override {
			if constexpr(IsEqual<r,void>)
				return F(Presume<t>(SuspensionFuture)), !Batch? current_step(Step0): ResumeStep(Step0);
			else
				return Result.ResolveStep(F(Presume<t>(SuspensionFuture)),!Batch? Step0: [=]{
					return ResumeStep(Step0);
				});
		}
		error OnDescribe() const override {return Describe();}
	};
	kernel::non_value_accessor Work(Source);
	if(kernel::IsResolvedFuture(Work.NonValueType))
		return F(Presume<t>(Source));
	auto Suspension=pin<box<when_suspension>>(Work.Source,Work.Waiter(),Describe,F,Fx);
	if(!kernel::AddSuspension(Work,Suspension))
		Suspension->Suspend();
	else
		Suspension->OnRunSuspensionStep(BadStep);
	if constexpr(!IsEqual<r,void>)
		return Suspension->Result;
}
template<class describe,class t,class f> requires requires(f F,t T) {F(T);}
auto WhenResolve(const describe& Describe,fx Fx,const t& T,const f& F) {
	return F(T);
}

// Wait for effects ready to run.
template<class describe,class callback>
requires requires(describe Describe,callback Callback,step Step) {error(Describe()); current_step(Callback(Step));}
current_step WhenFxStep(const describe& Describe,fx WhenFx,fx WhenHoldFx,const step& Step0,const callback& Callback) {
	struct when_fx_suspension: suspension_managed {
		describe Describe;
		callback Callback;
		when_fx_suspension(const describe& Describe0,fx WhenFx0,fx HoldFx0,const callback& Callback0):
			suspension_managed(HoldFx0,WhenFx0), Describe(Describe0), Callback(Callback0) {}
		current_step OnRunSuspensionStep(const step& Step1) override {
			VERSE_ENSURE(AllFxReady(WhenFx));
			return Callback(Step1);
		}
		error OnDescribe() const override {return Describe();}
	};
	if(AllFxReady(WhenFx))
		return Callback(Step0);
	else
		return box<when_fx_suspension>(Describe,WhenFx,WhenHoldFx,Callback)->Suspend(), Step0;
}

// Enqueue error upon next resume or stall.
void ErrorBatch(const error& Error);
void Stuck(fx Fx,const error& Error);
template<class describe> requires requires(describe Describe) {error(Describe());}
void Stuck(fx Fx,const describe& Describe) {
	WhenResolve(Describe,Fx,future<>(),[=](const any&)->current_step {VERSE_UNEXPECTED;});
}

// Run code in a new context and step into a handler when it succeeds, fails, throws.
namespace Internal {
	template<class t> current_step IterateStepFailsUnexpected(const t&,const step& Step0) {VERSE_UNEXPECTED;}
	template<class base> current_step IterateStepErr(const box<base>& Base,const error& Error,const step& Step0) {return ErrStep(Error,Step0);}
}
template<
	class base       = iterate_managed,
	class base_init,
	class describe,
	class fold,
	class domain_init,
	class on_domain,
	class on_succeeds,
	class on_fails   = decltype(&Internal::IterateStepFailsUnexpected<fold>),
	class on_throws  = decltype(&ThrowStep),
	class on_errs    = decltype(&Internal::IterateStepErr<base>),
	class domain     = decltype(declval<domain_init>()())
>
requires requires(describe Describe,box<base> Base,on_domain OnDomain,on_succeeds OnSucceeds,on_fails OnFails,on_throws OnThrows,on_errs OnErrs,domain Domain,fold Fold,step Next(const fold&,const step&),step Step0,future<> Future,locus Locus,error Error) {
	error(Describe(Base));
	current_step(OnDomain(Domain));
	current_step(OnSucceeds(Domain,Fold,Next,Step0));
	current_step(OnFails(Fold,Step0));
	current_step(OnThrows(Locus,Future,Step0));
	current_step(OnErrs(Base,Error,Step0));
}
current_step IterateStep(
	const describe&    Describe,
	const base_init&   BaseInit,
	const fold&        FoldInit,
	bool               IsCommittable,
	fx                 AllowFx,
	fx                 HoldFx,
	const step&        Step0,
	const domain_init& DomainInit,
	const on_domain&   OnDomain,
	const on_succeeds& OnSucceeds,
	const on_fails&    OnFails  = &Internal::IterateStepFailsUnexpected<fold>,
	const on_throws&   OnThrows = &ThrowStep,
	const on_errs&     OnErrs   = &Internal::IterateStepErr<base>
) {
	struct iterate_step: base {
		describe    Describe;
		fold        Fold;
		on_succeeds OnSucceeds;
		on_fails    OnFails;
		on_throws   OnThrows;
		on_errs     OnErrs;
		domain      Domain;
		iterate_step(const base_init& BaseInit,bool IsCommittable0,const describe& Describe0,const fold& FoldInit0,fx AllowFx0,fx HoldFx0,const step& ParentStep0,const domain_init& DomainInit0,const on_succeeds& OnSucceeds0,const on_fails& OnFails0,const on_throws& OnThrows0,const on_errs& OnErrs0): 
			base(BaseInit,IsCommittable0,AllowFx0,Thread->Depth+1,HoldFx0,ParentStep0),
			Describe(Describe0), Fold(FoldInit0), OnSucceeds(OnSucceeds0), OnFails(OnFails0), OnThrows(OnThrows0), OnErrs(OnErrs0),
			Domain(iterate_managed::Run(DomainInit0)) {}
		current_step ContextSucceeds(const step& Step1) const override {
			VERSE_ENSURE(Thread==iterate_managed::Context());
			auto NextStep = [Self=box(*this)](const fold& NextFold,const step& Step2) {
				return Self->Fold=NextFold, Self->ContextFails(true,Step2);
			};
			return OnSucceeds(Domain,Fold,NextStep,Step1);
		}
		current_step ContextFails(bool CommitWrites,const step& Step1) const override {
			VERSE_ENSURE(Thread==iterate_managed::Context());
			if(iterate_managed::IterateFork && iterate_managed::IsCommittable) {
				auto[ForkContext,RunForkStep]                     = iterate_managed::IterateFork.Coerce();
				ForkContext->ParentStep                           = Step1;
				ForkContext.template Coerce<iterate_step>()->Fold = Fold;
				return RunForkStep();
			}
			else return OnFails(Fold,Step1);
		}
		current_step ContextThrows(const locus& Locus,const future<>& A,const step& Step1) const override {return OnThrows(Locus,A,Step1);}
		current_step ContextErrs  (const error& E                      ,const step& Step1) const override {return OnErrs(box<base>(*this),E,Step1);}
		error OnDescribe()                                                                 const override {return const_cast<iterate_step*>(this)->Run(Describe,box(*this));}
	};
	auto Child=box<iterate_step>(BaseInit,IsCommittable,Describe,FoldInit,AllowFx,HoldFx,Step0,DomainInit,OnSucceeds,OnFails,OnThrows,OnErrs);
	VERSE_ENSURE(!Child->EnterContext("IterateStep"));
	return OnDomain(Child->Domain);
}

// Run code in a new context and run a callback when it succeeds, fails, throws.
namespace Internal {
	void IterateFailsUnexpected();
	void IterateThrowsUnexpected(const future<>&);
	template<class base> void IterateErrsUnexpected(const box<base>&,const error&) {VERSE_UNEXPECTED;}
}
template<
	class base         = iterate_managed,
	class base_init,
	class describe,
	class on_domain,
	class on_succeeds,
	class on_fails     = decltype(&Internal::IterateFailsUnexpected),
	class on_throws    = decltype(&Internal::IterateThrowsUnexpected),
	class on_errs      = decltype(&Internal::IterateErrsUnexpected<base>)
>
requires requires(box<base> Base,on_domain OnDomain,on_succeeds OnSucceeds,on_fails OnFails,on_throws OnThrows,future<> Future,on_errs OnErrs,error Error) {
	OnDomain();
	OnSucceeds();
	OnFails();
	OnThrows(Future);
	OnErrs(Base,Error);
}
void Iterate(
	const describe&    Describe,
	const base_init&   BaseInit,
	fx                 AllowFx,
	fx                 HoldFx,
	const on_domain&   OnDomain,
	const on_succeeds& OnSucceeds,
	const on_fails&    OnFails       = &Internal::IterateFailsUnexpected,
	const on_throws&   OnThrows      = &Internal::IterateThrowsUnexpected,
	const on_errs&     OnErrs        = &Internal::IterateErrsUnexpected<base>
) {
	RunSteps([=](const step& Step0)->current_step {
		return IterateStep<base>([=](const iterate&){return Describe();},BaseInit,False,false,AllowFx,HoldFx,Step0,
			[=](                                                      )               {return False;},
			[=](tuple<>                                               )               {return OnDomain();},
			[=](tuple<>,tuple<>,const auto& NextStep,const step& Step1)->current_step {return OnSucceeds(), Step1;},
			[=](tuple<>,                             const step& Step1)->current_step {return OnFails(), Step1;},
			[=](const locus& Locus,const future<>& a,const step& Step1)->current_step {return OnThrows(a), Step1;},
			[=](const box<base>& I,const error& a,   const step& Step1)->current_step {return OnErrs(I,a), Step1;}
		);
	});
}

// Start casting that suspends.
namespace Internal {
	template<class t,class u> void NativeCastSuspends(const u& a0) {
		if constexpr(!IsSubtype<u,t>) {
			WhenResolve(VERSE_HERE("NativeCastSuspends<t>"),only_cardinalities&abstracts,a0,[=](const any& a1) {expose<t>::ExposeCastSuspends(a1);});
		}
	}
}

// Start testing type membership; once decided we step into succeeds or fails handler.
template<class target,class describe,class source,class on_succeeds>
requires requires(describe Describe,on_succeeds OnSucceeds,target Target,step Step) {
	error(Describe());
	OnSucceeds(Target,Step);
}
current_step WhenCastStep(const describe& Describe,fx Fx,const source& Source,const option<future<>>& FailTarget,const step& Step0,const on_succeeds& OnSucceeds,bool=false,bool=false) {
	if constexpr(IsSubtype<source,target>)
		return OnSucceeds(target(Source),Step0);
	bool Finished=true;
	if(auto C=option<target>(Internal::construct_cast{},Source,&Finished)) // Succeeded synchronously.
		return OnSucceeds(C.PresumeReference(),Step0);
	else if(Finished) // Failed synchronously.
		return FailStep(locus{},"WhenCastStep-immediate",Step0);
	else // Unready. Use IterateStep to delimit multiple suspensions we can abandon upon failure.
		return IterateStep([=](const iterate&){return Describe();},False,False,false,only_cardinalities&decides,Fx,Step0, // Start over and wait.
			[=](                                                      )               {return False;},
			[=](tuple<>                                               )->current_step {return Internal::NativeCastSuspends<target>(Source), LeaveIterateStep;},
			[=](tuple<>,tuple<>,const auto& NextStep,const step& Step1)->current_step {return OnSucceeds(Presume<target>(Source),Step1);},
			[=](tuple<>,                             const step& Step1)->current_step {return FailStep(locus{},"WhenCastStep-suspended",Step1);}
		);
}

// Start testing type membership, and when decided, call succeeds or fails callback.
namespace Internal {
	template<class t> t WhenCastFailsUnexpected() {VERSE_UNEXPECTED;}
	current_step WhenCastThrowsUnexpected(const future<>&);
	current_step WhenCastErrsUnexpected(const iterate&,const error&);
}
template<class t,class source,class describe,class on_succeeds,
	class st        = decltype(declval<on_succeeds>()(declval<t>())),
	class on_fails  = decltype(&Internal::WhenCastFailsUnexpected<st>),
	class r         = decltype(declval<bool>()? declval<on_succeeds>()(declval<t>()): declval<on_fails>()())
>
auto WhenCast(const describe& Describe,fx Fx,const source& Source,const on_succeeds& OnSucceeds,const on_fails& OnFails=&Internal::WhenCastFailsUnexpected<st>) {
	using rt = if_type<IsEqual<r,void>,void,future<remove_future<r>>>;
	if constexpr(IsSubtype<source,t>)
		return OnSucceeds(t(Source));
	else {
		bool Finished=true;
		if(auto C=option<t>(Internal::construct_cast{},Source,&Finished)) // Succeeded synchronously.
			return rt(OnSucceeds(C.PresumeReference()));
		else if(Finished) // Failed synchronously.
			return rt(OnFails());
		else {
			// Unready, so start a suspending cast.
			future<> B=Source; // Want to eliminate this and just use Source, but it causes bugs.
			if constexpr(IsEqual<r,void>) {
				Iterate(Describe,False,only_cardinalities&decides,Fx,
					[=]()->current_step {return Internal::NativeCastSuspends<t>(B), LeaveIterateStep;},
					[=] {OnSucceeds(Presume<t>(B));},
					[=] {OnFails();},
					&Internal::WhenCastThrowsUnexpected,
					&Internal::WhenCastErrsUnexpected
				);
			}
			else {
				future<remove_future<r>> Result;
				Iterate(Describe,False,only_cardinalities&decides,Fx,
					[=]()->current_step {return Internal::NativeCastSuspends<t>(B), LeaveIterateStep;},
					[=] {Result.Resolve(OnSucceeds(Presume<t>(B)));},
					[=] {Result.Resolve(OnFails());},
					&Internal::WhenCastThrowsUnexpected,
					&Internal::WhenCastErrsUnexpected
				);
				return rt(Result);
			}
		}
	}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Interoperating with function parameters: single argument passed as-is, multiple-argument as tuple.

namespace Internal {
	template<class  ip,class... ps> struct parameter_tuple_helper;
	template<nat ...is,class... ps> struct parameter_tuple_helper<index_sequence<is...>,ps...> {
		using type=tuple<ps...>;
		template<class f        > static auto Call    (                  const f& F,           const tuple<ps...>& P) {return F(        P.template get<is>()...);}
		template<class f,class r> static auto CallStep(const step& Step0,const f& F,const r& R,const tuple<ps...>& P) {return F(Step0,R,P.template get<is>()...);}
	};
	template<nat i,class p> struct parameter_tuple_helper<index_sequence<i>,p> {
		using type=p;
		template<class f        > static auto Call    (                  const f& F,           const p& P) {return F(        P);}
		template<class f,class r> static auto CallStep(const step& Step0,const f& F,const r& R,const p& P) {return F(Step0,R,P);}
	};
}
template<class... ps> using parameter_tuple = typename Internal::parameter_tuple_helper<make_index_sequence<sizeof...(ps)>,remove_const_ref<ps>...>;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Expose casting for library types.

// Synchronous derived from future<>.
template<class t> bool default_expose<t>::ExposeCast(t* Target,const pin<any>& Source,bool* Finished) {
	if(auto C=Cast<box<t>>(Source))
		return new(Target)t(*C.Coerce()), true;
	return false;
}
template<class t> bool expose<box<t>>::ExposeCast(box<t>* Target,const pin<any>& Source,bool* Finished) {
	if constexpr(IsPolymorphic<t>) { // Polymorphic managed, so dynamic_cast to t to get new offset pointer.
		if(t* TP=dynamic_cast<t*>(kernel::CastManaged(Source)))
			return new(Target)box<t>(reinterpret_cast<const pin<box<t>>&>(TP)), true;
	}
	else if constexpr(!IsImmediate<t>) { // Non-polymorphic, so try dynamic_cast to boxer and get contents.
		if(auto MP=dynamic_cast<typename managed::template boxer<t>*>(kernel::CastManaged(Source))) {
			auto TP=MP->BoxedPointer();
			return new(Target)box<t>(reinterpret_cast<const pin<box<t>>&>(TP)), true;
		}
	}
	else if(kernel::IsIndexed(Source,Internal::MethodsIndexOf<t>)) { // Succeeds iff exact types match.
		return new(Target)box<t>(reinterpret_cast<const pin<box<t>>&>(Source)), true;
	}
	return false;
}
template<class t> bool expose<future<t>>::ExposeCast(future<t>* Target,const pin<any>& Source,bool* Finished) {
	if(auto C=option<t>(Internal::construct_cast,Source,Finished))
		return new(Target)future<t>(C.PresumeReference()), true;
	return false;
}
inline bool expose<future<>>::ExposeCast(future<>* Target,const pin<any>& Source,bool* Finished) {
	return new(Target)future<>(Source), true;
}
inline bool expose<any>::ExposeCast(any* Target,const pin<any>& Source,bool* Finished) {
	return new(Target)any(Source), true;
}
inline bool expose<comparable>::ExposeCast(comparable* Target,const pin<any>& Source,bool* Finished) {
	if(IsComparableDynamic(Source))
		return new(Target)comparable(reinterpret_cast<const comparable&>(Source)), true;
	else
		return false;
}
inline bool expose<real>::ExposeCast(real* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsRational(Source))
		return new(Target)real(reinterpret_cast<const pin<real>&>(Source)), true;
	return false;
}
inline bool expose<rational>::ExposeCast(rational* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsRational(Source))
		return new(Target)rational(reinterpret_cast<const pin<rational>&>(Source)), true;
	return false;
}
inline bool expose<integer>::ExposeCast(integer* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsInteger(Source))
		return new(Target)integer(reinterpret_cast<const pin<integer>&>(Source)), true;
	return false;
}
inline bool expose<natural>::ExposeCast(natural* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsInteger(Source))
		if(reinterpret_cast<const pin<integer>&>(Source)>=0)
			return new(Target)natural(reinterpret_cast<const pin<natural>&>(Source)), true;
	return false;
}
inline bool expose<array<>>::ExposeCast(array<>* Target,const pin<any>& Source,bool* Finished) {
	if(auto BO=Cast<managed>(Source); BO && BO->OnIsArray())
		return new(Target)array<>(reinterpret_cast<const pin<array<>>&>(BO.PresumeReference())), true;
	return false;
}
template<class t> bool expose<array<t>>::ExposeCast(array<t>* Target,const pin<any>& Source,bool* Finished) {
	if(auto BSO=Cast<array<>>(Source)) {
		auto BS=/*pin::operator() screws up for array<> specifically*/BSO.Coerce();

		// Allocate a new array, fill it in, and either return or abandon it.
		bool Success = true; 
		auto n       = BS->ContainerLength;
		new(Target)array<t>(construct_flat{},n,[&](t* ES) {
			for(nat i=0; i<n; i++)
				if(!Encase(ES+i,BS(i),Finished)) {
					for(nat j=0; j<=i; j++)
						ES[j].~t();
					kernel::AllocateAbandoned(ES); // ALT: make a managed stub for sync&async casted array<t> with ctor-scoreboard.
					Success=false;
					return;
				}
		});
		if(Success)
			return true;
		Target->~array<t>();
	}
	return false;
}
template<class t> bool expose<var<t>>::ExposeCast(var<t>* Target,const pin<any>& Source,bool* Finished) {
	//!! Best we can do, but unsafe as it doesn't capture variance. Could refuse this cast and have dynamic_ptr for this case.
	if(kernel::IsPointer(kernel::AdvancePinned(Source)))
		return new(Target)var<t>(reinterpret_cast<const pin<var<t>>&>(Source)), true;
	return false;
}

// Synchronous not derived from future<>.
inline bool expose<char>::ExposeCast(char* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsIndexed(Source,Internal::MethodsIndexOf<char32>))
		if(char32 ch=kernel::DecodeImmediate32(Source.Payload); ch<=0x7F)
			return new(Target)char(char(ch)), true;
	return false;
}
inline bool expose<char8>::ExposeCast(char8* Target,const pin<any>& Source,bool* Finished) {
	char32 ch=kernel::DecodeImmediate32(Source.Payload);
	if(kernel::IsIndexed(Source,Internal::MethodsIndexOf<char32>)&&ch<=0x7F || kernel::IsIndexed(Source,Internal::MethodsIndexOf<char8>))
		return new(Target)char8(char8(ch)), true;
	return false;
}
inline bool expose<char16>::ExposeCast(char16* Target,const pin<any>& Source,bool* Finished) {
	if(kernel::IsIndexed(Source,Internal::MethodsIndexOf<char32>))
		if(char32 ch=kernel::DecodeImmediate32(Source.Payload); ch<=0xFFFF)
			return new(Target)char16(char16(ch)), true;
	return false;
}
template<class t> bool expose_int<t>::ExposeCast(t* Target,const pin<any>& Source,bool* Finished) {
	if(int64 i; kernel::DecodeInt(&i,Source))
		if(auto ti=t(i); i==(int64)ti)
			return *Target=ti, true;
	return false;
}
template<class t> bool expose_nat<t>::ExposeCast(t* Target,const pin<any>& Source,bool* Finished) {
	if(nat64 n; kernel::DecodeNat(&n,Source))
		if(auto tn=t(n); n==(nat64)tn)
			return *Target=tn, true;
	return false;
}
template<class t> bool expose<option<t>>::ExposeCast(option<t>* Target,const pin<any>& Source,bool* Finished) {
	if(auto C=Cast<box<option<t>>>(Source)) // Fast path.
		return new(Target)option<t>(*C.PresumeReference()), true;
	if(auto FO=Cast<function<>>(Source); FO && FO->OnIsMap()) {
		if(FO->ContainerLength==0)
			return new(Target)option<t>(), true;
		if(FO->ContainerLength==1) { // Way off the fast path here. Rethink also avoiding intermediate copies if huge.
			if(auto K=FO->OnKeys()[0]; *new(Target)option<t>(Internal::construct_cast{},K,Finished))
				if(FO->OnIsSet() || CompareDynamic(FO->OnCallAssert(K),K)==nullptr)
					return true;
			Target->~option<t>();
		}
	}
	return false;
}
inline bool expose<bool>::ExposeCast(bool* Target,const pin<any>& Source,bool* Finished) {
	if(auto O=Cast<option<tuple<>>>(Source))
		return *Target=bool(O.Coerce()), true;
	return false;
}
namespace Internal {
	template<nat i,class ...ts> bool TupleExposeEncaseHelper(tuple<ts...>* Target,const array<>& AS,bool* Finished) {
		if constexpr(i==sizeof...(ts))
			return true;
		else {
			if(Encase(&Target->template get_reference<i>(),AS[i],Finished)) {
				if(TupleExposeEncaseHelper<i+1>(Target,AS,Finished))
					return true;
				using et=typename std::tuple_element<i,tuple<ts...>>::type;
				Target->template get_reference<i>().~et();
			}
			return false;
		}
	}
};
template<class... ts> bool expose<tuple<ts...>>::ExposeCast(tuple<ts...>* Target,const pin<any>& Source,bool* Finished) {
	if(auto ASO=Cast<array<>>(Source))
		if(auto& AS=ASO.PresumeReference(); Length(AS)==sizeof...(ts))
			return Internal::TupleExposeEncaseHelper<0>(Target,AS,Finished);
	return false;
}
template<class... ts> tuple<ts...> expose<tuple<ts...>>::ExposeCoerce(const pin<any>& Source) {
	if(auto ASO=Cast<array<>>(Source))
		if(auto& AS=ASO.PresumeReference(); Length(AS)==sizeof...(ts))
			return StaticFor<sizeof...(ts)>([&]<nat i> {using t=typename std::tuple_element<i,tuple<ts...>>::type; return Coerce<t>(AS[i]);});
	VERSE_ERR("Coerce failed");
}
inline bool expose<falsity>::ExposeCast(falsity* Target,const pin<any>& Source,bool* Finished) {
	return false;
}

// Asynchronous.
template<class t> void default_expose<t>::ExposeCastSuspends(const any& Source) {
	if(!Cast<t>(Source))
		StuckFx(only_cardinalities&fails);
}
inline void expose<array<>>::ExposeCastSuspends(const any& Source) {
	if(auto BO=Cast<managed>(Source); !BO || !BO->OnIsArray())
		StuckFx(only_cardinalities&fails);
}
template<class ...ts> template<nat i> void expose<tuple<ts...>>::ExposeCastSuspendsHelper(const array<>& AS) {
	if constexpr(i<sizeof...(ts))
		if(Internal::NativeCastSuspends<typename std::tuple_element<i,tuple<ts...>>::type>(AS[i]); !(Thread->LocalPendingFx<=fails))
			ExposeCastSuspendsHelper<i+1>(AS);
}
template<class ...ts> void expose<tuple<ts...>>::ExposeCastSuspends(const any& Source) {
	if(auto ASO=Cast<array<>>(Source))
		if(auto& AS=ASO.PresumeReference(); Length(AS)==sizeof...(ts))
			return ExposeCastSuspendsHelper<0>(AS);
	StuckFx(only_cardinalities&fails);
}
template<class t> void expose<array<t>>::ExposeCastSuspends(const any& Source) {
	if(auto ASO=Cast<array<>>(Source)) {
		auto AS=/*pin, but need to fix pin array<> first?*/(ASO.Coerce());
		for(nat i=0,n=AS->ContainerLength; i<n; i++)
			if(Internal::NativeCastSuspends<t>(AS(i)); !(Thread->LocalPendingFx<=fails))
				return;
	}
	else StuckFx(only_cardinalities&fails);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Miscellaneous forward-declared template definitions.

// String literal implementation.
template<string_literal Literal> string operator ""_VS() {
	static static_boxer<span<char8>> StaticBox(Literal.String,sizeof(Literal.String)-1);
	return StaticBox.GetBox0<string>();
};
template<string_literal Literal> path operator ""_VP() {
	return path(operator""_VS<Literal>());
};

// Cloning.
struct managed::cloner {
	pin<iterate> SourceIterate, TargetIterate;
	cloner(const pin<iterate>& Iterate0): SourceIterate(Iterate0), TargetIterate(Iterate0) {}
	template<class t> requires(IsSubtype<t,future<>>) const t& operator[](const t& T) {return Fixup(T), T;}
private:
	virtual void Fixup(const future<>& A0)=0;
};

// Futures.
template<class t> current_step future<t>::ResolveStep(const future<t>& a,const step& Step0,bool ResolveLocal) const {
	return future<>::ResolveStep(a,Step0,ResolveLocal);
}

// Boxes.
template<class t> pin<context> box<t>::Context() const {
	return kernel::ContextOf(AllocationBase());
}

// Functions.
template<class r,class ...ps> future<> managed_function<r(ps...)>::OnCallAssert(const future<>& P) const {
	if constexpr((IsExposed<ps> && ...)) {
		using pt=parameter_tuple<ps...>;
		if constexpr(IsEqual<r,void>)
			return pt::Call(*this,Coerce<typename pt::type>(P)), False;
		else
			return future<>(pt::Call(*this,Coerce<typename pt::type>(P)));
	}
	VERSE_UNEXPECTED;
}
template<class r,class ...ps> current_step managed_function<r(ps...)>::OnCallStep(const future<>& R,const future<>& P,const step& Step0) const {
	if constexpr((IsExposed<ps> && ...)) {
		using pt=parameter_tuple<ps...>;
		return WhenCastStep<typename pt::type>(VERSE_HERE("OnCallStep"),effects,P,False,Step0,
			[=,Self=box(*this)](const typename pt::type& PT,const step& Step1)->current_step {
				if constexpr(IsEqual<r,void>)
					return pt::Call(Self,PT), UnifyStep("OnCallStep-function",locus{},R,False,Step1);
				else
					return UnifyStep("OnCallStep-function",locus{},R,future<>(pt::Call(Self,PT)),Step1);
			}
		);
	}
	VERSE_UNEXPECTED;
}
#if 0
inline function<current_step()> MaybeRun(const step& S) {
	VERSE_ENSURE(!Thread->IsReady);
	if(*(nat*)&S!=*(nat*)&BadStep)//IsSameBox!!
		return S();
	return S;
}
template<class... ts> step MaybeRun(const ts&... TS) {
	VERSE_ENSURE(!Thread->IsReady);
	return step(TS...)();
}
template<class... ts> requires(requires(ts... TS) {function<current_step()>(TS...);})
current_step::current_step(const ts&... TS):
	function<current_step()>(MaybeRun(TS...)) {}
#else
template<class... ts> requires(requires(ts... TS) {function<current_step()>(TS...);})
current_step::current_step(const ts&... TS): function<current_step()>(TS...) {
	VERSE_ENSURE(!Thread->IsReady);
}
#endif

// Tables.
template<class k,class v> map<k,v>::map(tuple<>): box<base>(construct_payload{},FalseBox.Payload) {}
template<class k,class v> array<comparable> managed_map<k,v>::OnKeys() const {
	VERSE_UNIMPLEMENTED;//return Keys();
}
template<class t> array<t> ExposeKeys(const option<t>& o) {
	return o? array<t>{construct_elements{},o.Coerce()}: False;
}
template<class k,class v,class entry> mutable_map_base<k,v,entry>::mutable_map_base():
	// Problem: Though FalseMutableMap is static, var is not.
	any(var<storage>(reinterpret_cast<const storage&>(FalseMutableMap))) {}

// Arrays.
template<class t> array<t>::array(tuple<>): box<base>(construct_payload{},FalseBox.Payload) {}
template<class t> array<t> ExposeValue(span<t> a) {
	return For(ExposeLength(a),[p=a.Begin](nat i) {return p[i];});
}
template<class t> current_step managed_array<t>::OnCallStep(const future<>& R,const future<>& P,const step& Step0) const {
	return managed_function<>::ArrayCallStep(box(*this),R,P,Step0);
}

// Hash implementations.
template<class... ts> requires(sizeof...(ts)==0) nat HashDynamic(const future<>& F) {
	if(auto A=Cast<any>(F))
		return HashDynamic(pin(A.PresumeReference()));
	VERSE_ERR("HashDynamic: unresolved");
}
template<class t> nat HashDynamic(const var<t>& T) {
	auto TP=pin(T);
	kernel::AdvancePinned(TP);
	return PayloadHash(TP.Payload);
}
template<class... ts> nat ExposeHash(const tuple<ts...>& T) {
	return Internal::HashDynamicHelper<0>(Internal::HashFalse,T);
}
template<class t> nat Internal::HashDynamicArray(const t& TS) {
	nat H=Internal::HashFalse;
	const auto& CallableTS=Callable(TS);
	for(nat i=0,n=Length(TS); i<n; i++)
		H = H*Internal::HashNext + HashDynamic(CallableTS(i));
	return H;
}
template<class t> nat Internal::HashDynamicMap(const t& TS) {
	nat H=Internal::HashFalse,i=0;
	for(auto[K,VC]:TS) // Hash is consistent for tables thare are arrays or sets.
		if(auto VH=HashDynamic(VC.Coerce()); true)
			H = H*Internal::HashNext + VH + Internal::HashKey*(HashDynamic(K)-HashDynamic(i++));
	return H;
}
template<class t> nat Internal::HashDynamicSet(const t& S) {
	nat H=Internal::HashFalse,i=0;
	for(auto[K,_]:S) // Hash is consistent for sets that are arrays.
		if(auto VH=HashDynamic(K); true)
			H = H*Internal::HashNext + VH + Internal::HashKey*(VH-HashDynamic(i++));
	return H;
}
template<class t> nat ExposeHash(const option<t>& O) {
	nat H=Internal::HashFalse;
	if(O)
		if(auto VH=HashDynamic(O.Coerce()); true)
			return H*Internal::HashNext + VH + Internal::HashKey*(VH-HashDynamic(0));
	return H;
}
template<nat i,class... ts> nat Internal::HashDynamicHelper(nat H,const tuple<ts...>& T) {
	if constexpr(i<sizeof...(ts))
		return HashDynamicHelper<i+1>(H*HashNext + HashDynamic(T.template get<i>()),T);
	else
		return H;
}

// Comparison.
template<class t,class u,class> equality_ordering CompareDynamic(const box<t>& T,const box<u>& U) {
	return equality_ordering(kernel::IsSameBox(T,U));
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Comes last so they see all overloads in this namespace.

// Signature.
namespace Internal {
	template<class t> auto SignatureHelper(const t& a)->decltype(static_cast<path>(ExposeSignature(a))) {return ExposeSignature(a);}
	template<class t,class... ts> requires(sizeof...(ts)==0) path SignatureHelper(const t& a,ts...) {
		if constexpr(IsSubtype<exposed_type<t>,function<future<>(const falsity&)>>)
			return "/Verse.org/function"_VP;
		else
			return path(string_fix_me(NativeNameOfType<t>));//!!bad
	}
}
template<class t,class... ds> path managed_box<t,ds...>::OnSignature() const {
	return Internal::SignatureHelper<t>(*this);
}
template<class t        > path expose_int<t>       ::ExposeStaticSignature() {return "/Verse.org/real"_VP;}
template<class t        > path expose_nat<t>       ::ExposeStaticSignature() {return "/Verse.org/real"_VP;}
template<class t        > path expose<var<t>>      ::ExposeStaticSignature() {return "/Verse.org/pointer"_VP;}
template<class t        > path expose<option<t>>   ::ExposeStaticSignature() {return "/Verse.org/function"_VP;}
template<class... ts    > path expose<tuple<ts...>>::ExposeStaticSignature() {return "/Verse.org/function"_VP;}
template<class t        > path expose<span<t>>     ::ExposeStaticSignature() {return "/Verse.org/function"_VP;}
template<class t        > path expose<bag<t>>      ::ExposeStaticSignature() {return "/Verse.org/function"_VP;}
template<class t        > path expose<array<t>>    ::ExposeStaticSignature() {return "/Verse.org/function"_VP;}
template<class k,class v> path expose<map<k,v>>    ::ExposeStaticSignature() {return "/Verse.org/function"_VP;}

// ExposeToString.
template<class t> string ToStringSeparated(const array<t>& AS,const string& Separator=","_VS) {
	string S=False;
	for(nat i=0,n=Length(AS); i<n; i++) {
		if(i)
			S+=Separator;
		S+=ToString(AS[i]);
	}
	return S;
}
template<class t> string ExposeToString(const array<t>& AS) {
	return ToStringSeparated(AS,False);
}
template<class t> string ExposeToString(const option<t>& o) {
	if(o)
		return ToString("truth{"_VS,o.Coerce(),"}"_VS);
	else
		return ToString("false");
}
template<class t> string ExposeToString(const var<bag<t>>& TSP) {
	string Result;
	for(auto[T,_]:TSP)
		Result=ToString(Result,Length(Result)? ","_VS: False,ToString(T));
	return ToString("bag{"_VS,Result,"}"_VS);
}
template<class k,class v> string ExposeToString(const var<map<k,v>>& KVMP) {
	string Result;
	for(auto[K,VC]:KVMP)
		Result=ToString(Result,Length(Result)? ","_VS: False,string(ToString(K)),"=>",string(ToString(VC.ReadValue())));
	return ToString("map{"_VS,Result,"}"_VS);
}
template<class t,class self> string Internal::OnToStringHelper(const t& T,const self* Self) {
	if constexpr(HasExposeToString<t>)
		return ExposeToString(T);
	else
		return string(Self->OnNativeName()); //inefficient
}

// Printing.
namespace Internal {
	template<class t> auto PrintHelper(const t& T)->decltype(void(ToString(T))) {
		PrintHelper(ToString(T));
	}
}
template<bool Line,class a,class ...bs> void Print(const a& A,const bs&... BS) {
	Internal::PrintHelper(A); Print<Line>(BS...);
}

// Immediates.
template<class t> path Internal::ImmediateMethodsSignature(const pin<any>& a) {
	const auto& a1=reinterpret_cast<const pin<box<t>>&>(a);
	return Internal::SignatureHelper(*a1);
}
template<class t> dynamic_ordering Internal::ImmediateMethodsCompare(const pin<any>& A,const pin<any>& B) {
	if constexpr(IsEqual<t,Internal::integer_immediate>)
		return
			kernel::PayloadIsImmediateInteger(B.Payload)? dynamic_ordering{A.Payload==B.Payload}:
			IsComparableDynamic(B)?                       dynamic_ordering(false):
														  dynamic_ordering();
	else if constexpr(IsComparable<t>())
		return kernel::IsIndexed(B,Internal::MethodsIndexOf<t>)? ExposeCompare(*reinterpret_cast<const pin<box<t>>&>(A),*reinterpret_cast<const pin<box<t>>&>(B)):
			IsComparableDynamic(B)?                            dynamic_ordering(false):
															   dynamic_ordering();
	return dynamic_ordering();
}
template<class t> nat Internal::ImmediateMethodsHash(const pin<any>& A) {
	if constexpr(IsEqual<t,Internal::integer_immediate>)
		return PayloadHash(A.Payload);
	else if constexpr(IsComparable<t>())
		return ExposeHash(*reinterpret_cast<const pin<box<t>>&>(A));
	VERSE_UNEXPECTED;
}
template<class t> string Internal::ImmediateMethodsToString(const pin<any>& A) {
	if constexpr(IsEqual<t,Internal::integer_immediate>) {
		int64 Value=0;
		VERSE_ENSURE(kernel::DecodeInt(&Value,A));
		return ExposeToString(Value);
	}
	else if constexpr(HasExposeToString<t>)
		return ExposeToString(*reinterpret_cast<const pin<box<t>>&>(A));
	else
		return "native"_VS;
}
template<class t> bool Internal::ImmediateMethodsComparable(const pin<any>& A) {
	if constexpr(IsComparable<t>())
		return true;
	else if constexpr(HasExposeIsComparable<t>)
		return ExposeIsComparable(*reinterpret_cast<const pin<box<t>>&>(A));
	else
		return false;
}

// Singletons.
template<class t,t T> bool expose<singleton<t,T>>::ExposeCast(singleton<t,T>* Target,const pin<any>& Source,bool* Finished) {
	if(auto O=Cast<T>(Source); O && O.Coerce()==T)
		return new(Target) singleton<t,T>(), true;
	return false;
}
template<class t,t T> path expose<singleton<t,T>>::ExposeStaticSignature() {
	return Signature(T);
}

}
#pragma warning(pop)
