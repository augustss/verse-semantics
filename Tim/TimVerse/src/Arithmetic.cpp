//==============================================================================================================================================================
// Arithmetic.

#include "Terse.h"
#pragma warning (disable: 4100 4293)
#if __clang__
#pragma clang diagnostic ignored "-Warray-bounds"
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#endif
using namespace Verse;

// Configuration.
#define VERSE_TEST_INTEGER 0
#if VERSE_TEST_INTEGER
using base=nat8;  using signed_base=int8;  static const nat PayloadDigitBits=7;
#else
using base=nat64; using signed_base=int64; static const nat PayloadDigitBits=48;
#endif

// Payload flags and bits.
static const nat PayloadIntegerShift=3,PayloadImmediateInteger=1;
static const nat PayloadGuard=(1LL<<PayloadDigitBits<<PayloadIntegerShift)+4;
static bool IsSmall(int64 i) {return i==((i<<(64-PayloadDigitBits))>>(64-PayloadDigitBits));}
static bool IsSmall(nat64 i) {return i<(1ULL<<(PayloadDigitBits-1));}
nat kernel::EncodeIntegerDigit(nat n) {return PayloadImmediateInteger|n<<PayloadIntegerShift;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Arithmetic helpers.

// Small integer helpers used for testing.
nat8 SumCarry        (nat8 cf,nat8 a,nat8 b,nat8* r)  {nat16 v=(nat32)a+b+cf; *r   =(nat8)v;      return (nat8)(v>>8);}
nat8 DifferenceBorrow(nat8 bf,nat8 a,nat8 b,nat8* r)  {nat16 v=(nat32)a-b-bf; *r   =(nat8)v;      return (v>>8)!=0;}
nat8 ProductCarry    (nat8 a ,nat8 b,nat8* r_hi)      {nat16 v=(nat32)a*b;    *r_hi=(nat8)(v>>8); return (nat8)v;}
namespace Verse {
	nat8 TruncatingDivisionCarry(nat8 Dh,nat8 Dl,nat8 d,nat8* r) {auto D=((nat32)Dh<<8)+Dl,q=D/d; VERSE_ASSERT((q>>8)==0); *r=nat8(D-d*q); return nat8(q);}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Variable precision integer encoding into any.

struct managed_integer: managed {
	nat Count;
	union {int64 Digit; base Digits[1];};
	managed_integer(const managed_integer& Source): managed(Source), Count(Source.Count) {
		if constexpr(sizeof(base)!=sizeof(nat64))
			Digit=0;
		for(nat i=0; i<Count; i++)
			Digits[i]=Source.Digits[i];
	}
	template<class u> inline managed_integer(nat Count0,const u& init): Count(Count0) {
		init(*this);
		Finish();
	}
    managed_integer(int64 i): Count(sizeof(nat)/sizeof(base)), Digit(i) {
		if constexpr(sizeof(base)!=sizeof(nat))
            Finish();
    }
    managed_integer(nat64 i): Count(sizeof(nat64)/sizeof(base)+1), Digit(int64(i)) {
        Digits[sizeof(nat64)/sizeof(base)]=0;
        Finish();
    };
	managed_integer(construct_constexpr,int64 i): managed(construct_constexpr{}), Count(sizeof(nat)/sizeof(base)), Digit(i) {
		if constexpr(sizeof(base)!=sizeof(nat))
            Finish();
    }
	static nat OnConstructorSize(int64) {
		return 0;
	}
    static nat OnConstructorSize(nat64) {
		return sizeof(nat);
	}
	template<class u> static nat OnConstructorSize(nat Count,const u&) {
		return sizeof(base)*(Count-1);
	}
	base operator[](nat i) const {
		// Sign-extended digit stream.
		return i<Count? Digits[i]: SignExtend(Digits[Count-1]);
	} 
    void Finish() {
		while(Count>1 && SignExtend(Digits[Count-2])==Digits[Count-1]) Count--;
	}
	base Sign() const {
		return SignExtend(Digits[Count-1]);
	}
	static base SignExtend(base a) {
		return base((signed_base)a>>(sizeof(base)*8-1));
	}
	friend total_ordering ExposeCompare(const managed_integer& A,const managed_integer& B) {
		nat  n = Max(A.Count,B.Count);
		auto c = CompareTotal(B[n],A[n]); // Compare sign digit contravariantly, rest covariantly.
		for(nat i=n-1; c==nullptr && i+1; i--)
			c=CompareTotal(A[i],B[i]);
		return c;
	}
	friend nat ExposeHash(const managed_integer& A) {
		return HashDynamic(A.Digit);
	}
};
template<> struct expose<managed_integer>: default_expose<managed_integer> {
	static path ExposeStaticSignature() {
		return "/Verse.org/real"_VP;
	}
};

// Stack-optimized managed_integer storage.
struct Verse::Internal::integer_local  {
	pin<integer> Pinned;
    managed_integer Local;
    const managed_integer& Big;
	VERSE_NO_INLINE integer_local(const integer& Source): Pinned(Source), Local(construct_constexpr{},int64(Pinned.Payload)>>PayloadIntegerShift), 
		Big(kernel::PayloadIsImmediateInteger(Pinned.Payload)? Local: *reinterpret_cast<const pin<box<managed_integer>>&>(Pinned).operator->()) {}
    const managed_integer* operator->() {return &Big;}
};

// Integer ExposeValue.
VERSE_NO_INLINE static integer ExposeAnyBig(int64 i) {const auto& a=box<managed_integer>(i); return reinterpret_cast<const integer&>(a);}
VERSE_NO_INLINE static natural ExposeAnyBig(nat64 i) {const auto& a=box<managed_integer>(i); return reinterpret_cast<const natural&>(a);}
integer Verse::ExposeValue(int64 i) {if(IsSmall(i)) return integer(construct_payload{},kernel::EncodeIntegerDigit(i)); else return ExposeAnyBig(i);}
natural Verse::ExposeValue(nat64 i) {if(IsSmall(i)) return natural(construct_payload{},kernel::EncodeIntegerDigit(i)); else return ExposeAnyBig(i);}
integer Verse::ExposeValue(int32 i) {return ExposeValue(int64(i));}
natural Verse::ExposeValue(nat32 i) {return ExposeValue(nat64(i));}
bool kernel::IsInteger(const future<>& a) {
	return kernel::PayloadIsImmediateInteger(a.Payload) || IsA<managed_integer>(a);
}

// Big integer operations.
template<nat8 op> static void BitIntegers(managed_integer& r,const managed_integer& a,const managed_integer& b) {
	base ad,bd;
	for(nat i=0,n=r.Count; i<n; i++)
		ad=a[i], bd=b[i], r.Digits[i]=op==0? ad&bd: op==1? ad^bd: ad|bd;
}
static void AddIntegers(managed_integer& r,const managed_integer& a,const managed_integer& b) {
	nat8 c=0;
	for(nat i=0,n=r.Count; i<n; i++)
		c=SumCarry(c,a[i],b[i],&r.Digits[i]);
}
static void SubtractIntegers(managed_integer& r,const managed_integer& a,const managed_integer& b,nat Offset=0) {
	nat8 c=0;
	for(nat i=0,n=r.Count-Offset; i<n; i++)
		c=DifferenceBorrow(c,a[Offset+i],b[i],&r.Digits[Offset+i]);
}
static void PadDigits(managed_integer& r,nat Offset,nat Count) {
	while(r.Count-Offset<=Count)
		r.Digits[r.Count]=r[r.Count], r.Count++;
}
static void AddMultiplyIntegers(managed_integer& r,nat Offset,base mc,const managed_integer& a) {
	PadDigits(r,Offset,a.Count);
	nat8 c=0; base ph=0,pl,tmp;
	for(nat i=0; i<=a.Count; i++) {
		c  = SumCarry(0,r.Digits[Offset+i],ph,&tmp);      // Add carried digit to current digit to produce updated digit.
		pl = ProductCarry(mc,a[i],&ph);                   // Compute product digit lo and hi.
		ph = ph+c+SumCarry(0,pl,tmp,&r.Digits[Offset+i]); // Write sum of updated digit and product low digit, generate carry digit for next iteration. Can't overflow.
	}
}
static void SubtractMultiplyIntegers(managed_integer& r,base mc,const managed_integer& a) {
	PadDigits(r,0,a.Count);
	nat8 b=0; base ph=0,pl,tmp;
	for(nat i=0; i<r.Count; i++) {
		b  = DifferenceBorrow(0,r.Digits[i],ph,&tmp);
		pl = ProductCarry(mc,a[i],&ph);
		ph = ph+b+DifferenceBorrow(0,tmp,pl,&r.Digits[i]);
	}
}
static void MultiplyIntegers(managed_integer& r,const managed_integer& a,const managed_integer& b) {
	r.Digits[0]=0,r.Count=1;
	for(nat Offset=0; Offset<a.Count; Offset++)
		AddMultiplyIntegers(r,Offset,a[Offset],b);
	if(a[a.Count])
		SubtractIntegers(r,r,b,a.Count); // When negative a, subtract b*base^an. Thus our total space requirement is a->Count+b->Count+1.
}
static integer Canonize(const pin<box<managed_integer>>& I) {
	if(I->Count>1 || !IsSmall(I->Digit))
		return reinterpret_cast<const integer&>(I);
	return I->Digit;
}
VERSE_NO_INLINE                   static integer Add      (const integer& a0,const integer& b0) {Internal::integer_local a(a0),b(b0); return Canonize(box<managed_integer>(Max(a->Count,b->Count)+1,[&](managed_integer& r) {AddIntegers(r,a.Big,b.Big);}));}
VERSE_NO_INLINE                   static integer Subtract (const integer& a0,const integer& b0) {Internal::integer_local a(a0),b(b0); return Canonize(box<managed_integer>(Max(a->Count,b->Count)+1,[&](managed_integer& r) {SubtractIntegers(r,a.Big,b.Big);}));}
VERSE_NO_INLINE                   static integer Multiply (const integer& a0,const integer& b0) {Internal::integer_local a(a0),b(b0); return Canonize(box<managed_integer>(a->Count+b->Count       ,[&](managed_integer& r) {MultiplyIntegers(r,a.Big,b.Big);}));}
template<nat8 op> VERSE_NO_INLINE static integer Bit      (const integer& a0,const integer& b0) {Internal::integer_local a(a0),b(b0); return Canonize(box<managed_integer>(Max(a->Count,b->Count),[&](managed_integer& r) {BitIntegers<op>(r,a.Big,b.Big);}));}

// Integer ordering.
total_ordering Verse::CompareDynamic(const integer& a,const integer& b) {
	if(auto ap=a.Payload,bp=b.Payload; ap&bp&PayloadImmediateInteger)
		return CompareTotal(int64(ap),int64(bp));
	Internal::integer_local al(a),bl(b);
	return ExposeCompare(al.Big,bl.Big);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Integer class implementation.

VERSE_FORCE_INLINE int64   I(const integer& A) {return int64(A.Payload);}
VERSE_FORCE_INLINE integer Verse::operator+ (                 const integer& b) {return b;}
VERSE_FORCE_INLINE integer Verse::operator- (                 const integer& b) {int64 r=( 0^3)-I(b);             if(((r^(r+r))&PayloadGuard)==0) return integer(construct_payload{},r); return Subtract(0,b);}
VERSE_FORCE_INLINE integer Verse::operator+ (const integer& a,const integer& b) {int64 r=(I(a)+I(b))^3;           if(((r^(r+r))&PayloadGuard)==0) return integer(construct_payload{},r); return Add(a,b);}
VERSE_FORCE_INLINE integer Verse::operator- (const integer& a,const integer& b) {int64 r=(I(a)^3)-I(b);           if(((r^(r+r))&PayloadGuard)==0) return integer(construct_payload{},r); return Subtract(a,b);}
VERSE_FORCE_INLINE integer Verse::operator& (const integer& a,const integer& b) {int64 r=I(a)&I(b);               if(r&1)                         return integer(construct_payload{},r); return Bit<0>(a,b);}
VERSE_FORCE_INLINE integer Verse::operator^ (const integer& a,const integer& b) {int64 ap=I(a),bp=I(b),r=ap^bp^1; if(ap&bp&1)                     return integer(construct_payload{},r); return Bit<1>(a,b);}
VERSE_FORCE_INLINE integer Verse::operator| (const integer& a,const integer& b) {int64 ap=I(a),bp=I(b),r=ap|bp;   if(ap&bp&1)                     return integer(construct_payload{},r); return Bit<2>(a,b);}
VERSE_FORCE_INLINE integer Verse::operator~ (                 const integer& b) {int64 r=((-1LL<<PayloadIntegerShift)^3)-I(b); if(((r^(r+r))&PayloadGuard)==0) return integer(construct_payload{},r); return Subtract(-1,b);}
integer  Verse::operator* (const integer& a,const integer& b) {
    nat ap=a.Payload, bp=b.Payload, rh, r=ProductCarry(ap-1,(bp-1)<<(64-PayloadDigitBits-PayloadIntegerShift*2),&rh);
    if(ap&bp&(rh==nat(int64(r)>>63)))
        return integer(construct_payload{},(r>>(64-PayloadDigitBits-PayloadIntegerShift))+1);
    return Multiply(a,b);
}
integer  Verse::integer::operator++(int)              {integer a=*this; new(this)integer(a+1); return a;}
integer  Verse::integer::operator--(int)              {integer a=*this; new(this)integer(a-1); return a;}
integer& Verse::integer::operator++()                 {return *new(this)integer(*this+1);}
integer& Verse::integer::operator--()                 {return *new(this)integer(*this-1);}
integer& Verse::integer::operator+=(const integer& b) {return *new(this)integer(*this+b);}
integer& Verse::integer::operator-=(const integer& b) {return *new(this)integer(*this-b);}
integer& Verse::integer::operator*=(const integer& b) {return *new(this)integer(*this*b);}
integer& Verse::integer::operator/=(const integer& b) {return *new(this)integer(*this/b);}
integer& Verse::integer::operator%=(const integer& b) {return *new(this)integer(*this%b);}
integer& Verse::integer::operator&=(const integer& b) {return *new(this)integer(*this&b);}
integer& Verse::integer::operator^=(const integer& b) {return *new(this)integer(*this^b);}
integer& Verse::integer::operator|=(const integer& b) {return *new(this)integer(*this|b);}

// Half-width unsigned operations.
static nat  HalfCount  (const managed_integer& s)         {nat c=s.Count; if(c>1 && s.Digits[c-1]==0) c--; return c*2 - ((s.Digits[c-1]>>sizeof(base)*4)==0);}
static base HalfDigits (const managed_integer& s,nat i)   {return (s.Digits[i/2]>>(i&1)*sizeof(base)*4)&((1ULL<<sizeof(base)*4)-1);}

// Internal integer operations.
bool kernel::DecodeInt(int64* Result,const pin<any>& A) {
	if(auto P=A.Payload; PayloadIsImmediateInteger(P))
		return *Result=int64(P)>>PayloadIntegerShift, true;
	if(auto IO=Cast<managed_integer>(future<>(A))) {
		box<managed_integer> I(IO.Coerce());
		if constexpr(sizeof(base)==sizeof(nat64)) {
			if(I->Count==1)
				return *Result=I->Digit, true;
		}
		else if(auto c=sizeof(nat64)/sizeof(base); I->Count<=c) {
			int64 r=0;
			for(auto i=c-1; i+1; i--) // Ensure we get sign extension for type of specified size.
				r=(r<<sizeof(base)*8)+int64(I->operator[](i));
			return *Result=r, true;
		}
	}
	return false;
}
bool kernel::DecodeNat(nat64* Result,const pin<any>& a) {
	if(auto p=a.Payload; PayloadIsImmediateInteger(p) && int64(p)>=0)
		return *Result=nat64(p)>>PayloadIntegerShift, true;
 	if(auto io=Cast<managed_integer>(future<>(a))) {
		box<managed_integer> b(io.Coerce());
		if constexpr(sizeof(base)==sizeof(nat64)) {
			if(b->Count==1 || b->Count==2 && b->Digits[1]==0)
				return *Result=b->Digit, true;
		}
		else if(auto c=sizeof(nat64)/sizeof(base); b->Count<=c || b->Count==c+1&&b->Digits[c]==0) {
			nat64 r=0;
			for(nat i=b->Count-1; i+1; i--) // Ensure we get sign extension for type of specified size.
				r=(r<<sizeof(base)*8)+b->Digits[i];
			return *Result=r, true;
		}
	}
	return false;
}
nat Verse::CastIndex(const future<>& a) {
	// Casts to Index that can be used for addressing in-memory array with nonzero sized elements.
	if constexpr(sizeof(base)==sizeof(nat64))
		if(auto p=a.Payload; kernel::PayloadIsImmediateInteger(p))
			return nat(int64(p)>>PayloadIntegerShift); // Otherwise, must take slow path, in case it's a resolved future.
	return Cast<nat>(a).Else(Max<nat>());
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Euclidean Division: Computes q&r where D=d*q+r, 0<=r<d.
// - Hence q=Floor(D/d), thus power-of-two mulitply & divide match arithmetic shifts.
// - This is unlike C99, C++11, modern CPUs which choose q=Trunc(D/d).
// See: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

VERSE_NO_INLINE option<tuple<natural,natural>> Verse::EuclideanDivision(natural Di,natural di) {
	if(di!=0) {
		if(auto dbo=Cast<base>(di)) {
			base db=dbo.Coerce();
			if(auto Dbo=Cast<base>(Di)) {
				// Use native division for small divisor, small dividend.
				base Db=Dbo.Coerce(), qb=Db/db;
				return Truth(tuple<natural,natural>{qb,Db-qb*db});
			}
			else {
				// Use Knuth division for small divisor, large dividend.
				Internal::integer_local D(Di);
				base rb=0;
				auto q=box<managed_integer>(D->Count,[&](managed_integer& q) {
					for(nat i=D->Count-1; i+1; i--)
						q.Digits[i]=TruncatingDivisionCarry(rb,D->Digits[i],db,&rb);
				});
				return Truth(tuple<natural,natural>{reinterpret_cast<const natural&>(q),rb});
			}
		}
		else {
			// Use long division for large divisor, large dividend.
			Internal::integer_local D(Di),d(di);
			integer ri;
			auto q=box<managed_integer>(D->Count,[&](managed_integer& q) { 
				ri=Canonize(box<managed_integer>(d->Count+1,[&](managed_integer& r) {
					for(nat i=0; i<q.Count; i++) q.Digits[i]=0;                                 // Quotient q=0
					r.Count=1; r.Digits[0]=0;                                                   // Remainder r=0
                                                                                                // Iterate over half-digits of q to perform native word-size divides.
                    nat  dn  = HalfCount(d.Big);                                                // Divisor Count of half-digits.
					base dhh = HalfDigits(d.Big,dn-1)<<sizeof(base)*4 | HalfDigits(d.Big,dn-2); // Divisor upper two half-digits.
					for(nat i=D->Count*2-1; i+1; i--) {
						r.Digits[r.Count++]=0; // Update r = r*sqrt(base) + D.HalfDigits[i]:
						for(nat j=r.Count-1; j; j--)
							r.Digits[j]=r.Digits[j]<<sizeof(base)*4 | r.Digits[j-1]>>sizeof(base)*4;
						r.Digits[0]=r.Digits[0]<<sizeof(base)*4 | HalfDigits(D.Big,i);
                        r.Finish();
						base qdig=0,rh;
						if(ExposeCompare(r,d.Big)>=nullptr) {
							nat   rn   = HalfCount(r);
                            VERSE_ASSERT(rn==dn || rn==dn+1);
							base rh0  = HalfDigits(r,rn-1);
							base rh1  = HalfDigits(r,rn-2);
							base rh2  = HalfDigits(r,rn-3);
							qdig      = rn==dn? TruncatingDivisionCarry(0  ,rh0<<sizeof(base)*4 | rh1,dhh,&rh):
							                    TruncatingDivisionCarry(rh0,rh1<<sizeof(base)*4 | rh2,dhh,&rh);
							SubtractMultiplyIntegers(r,qdig,d.Big);
							if(signed_base(r.Digits[r.Count-1])<0)
								qdig--,AddIntegers(r,r,d.Big);
							VERSE_ASSERT(signed_base(r.Digits[r.Count-1])>=0);
						}
						q.Digits[i/2]|=qdig<<(i&1)*sizeof(base)*4;
					}
                }));
			});
			Canonize(q);
			return Truth(tuple<natural,natural>{reinterpret_cast<const natural&>(q),reinterpret_cast<const natural&>(ri)});
		}
	}
	return False;
}
VERSE_NO_INLINE option<tuple<integer,integer>> Verse::EuclideanDivision(integer Di,integer di) {
	auto Dn=Cast<natural>(Di),dn=Cast<natural>(di);
	if(Dn&&dn)
		return EuclideanDivision(Dn.Coerce(),dn.Coerce());
	auto[uq,ur] = EuclideanDivision(Dn? Di: -Di,dn? di: -di).Coerce();
	auto QR    = 
		bool(Dn)==bool(dn)? tuple<integer,integer>{ uq  ,   ur}:
		ur>0?               tuple<integer,integer>{-uq-1,di-ur}:
			                tuple<integer,integer>{-uq  ,   ur};
	VERSE_ASSERT(Di==QR.get<0>()*di+QR.get<1>() && QR.get<1>()==0 && QR.get<0>()<di);
	return Truth(QR);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Rational numbers.

struct ratio {
	integer d,D;
    ratio(const integer& d0,const integer& D0): d(d0), D(D0) {}
	friend total_ordering ExposeCompare(const ratio& A,const ratio& B) {
		return CompareTotal(A.d*B.D,A.D*B.d);
	}
	friend nat ExposeHash(const ratio& Ratio) {
		return Hash(Ratio.d)^Hash(Ratio.D);//crappy
	}
};
template<> struct expose<ratio>: default_expose<ratio> {
	static path ExposeStaticSignature() {
		return "/Verse.org/real"_VP;
	}
};
ratio ToRatio(const rational& r,const option<integer>& io) {
	if(io)
		return ratio(io.Coerce(),1);
	else
		return Coerce<ratio>(r);
}
ratio ToRatio(const rational& r) {
    return ToRatio(r,Cast<integer>(r));
}
rational MakeRational(const integer& d,const integer& D) {
    integer g=GCD(Abs(d),D),d1=Quotient(d,g);
    if(D!=g) {
		const auto& R=box<ratio>(d1,Quotient(D,g));
        return reinterpret_cast<const rational&>(R);
	}
    else return d1;
}
bool kernel::IsRational(const future<>& a) {
	return IsInteger(a) || IsA<ratio>(a);
}
rational Verse::operator+(const rational& a,const rational& b) {
    auto aio=Cast<integer>(a),bio=Cast<integer>(b);
    if(aio&&bio)
        return aio.Coerce()+bio.Coerce();
    ratio ar=ToRatio(a,aio),br=ToRatio(b,bio);
    return MakeRational(ar.d*br.D+br.d*ar.D,ar.D*br.D);
}
rational Verse::operator-(const rational& a,const rational& b) {
    auto aio=Cast<integer>(a),bio=Cast<integer>(b);
    if(aio&&bio)
        return aio.Coerce()-bio.Coerce();
    ratio ar=ToRatio(a,aio),br=ToRatio(b,bio);
    return MakeRational(ar.d*br.D-br.d*ar.D,ar.D*br.D);
}
rational Verse::operator+(const rational& a) {
    return a;
}
rational Verse::operator-(const rational& a) {
    if(auto aio=Cast<integer>(a))
        return -aio.Coerce();
    ratio ar=ToRatio(a);
    return MakeRational(-ar.d,ar.D);
}
rational Verse::operator*(const rational& a,const rational& b) {
    auto aio=Cast<integer>(a),bio=Cast<integer>(b);
    if(aio&&bio)
        return aio.Coerce()*bio.Coerce();
    ratio ar=ToRatio(a,aio),br=ToRatio(b,bio);
    return MakeRational(ar.d*br.d,ar.D*br.D);
}
rational Verse::operator/(const rational& a,const rational& b) {
    ratio ar=ToRatio(a),br=ToRatio(b);
    if(br.d!=0)
        return MakeRational(ar.d*br.D*Sgn(br.d),ar.D*Abs(br.d));
	VERSE_ERR("rational division by zero");
}
rational Ratio(const rational& a,const rational& b) {
	return a/b;
}
rational& rational::operator+=(const rational& b) {return *new(this)rational(*this+b);}
rational& rational::operator-=(const rational& b) {return *new(this)rational(*this-b);}
rational& rational::operator*=(const rational& b) {return *new(this)rational(*this*b);}
rational& rational::operator/=(const rational& b)          {return *new(this)rational(*this/b);}
total_ordering Verse::CompareDynamic(const rational& a,const rational& b) {
    auto aio=Cast<integer>(a),bio=Cast<integer>(b);
    if(aio&&bio)
        return CompareTotal(aio.Coerce(),bio.Coerce());
    ratio ar=ToRatio(a,aio),br=ToRatio(b,bio);
    return CompareTotal(ar.d*br.D,ar.D*br.d);
}
integer Verse::Floor      (const rational& a) {auto ar=ToRatio(a); return Quotient(ar.d,ar.D);}
integer Verse::Ceil       (const rational& a) {return -Floor(-a);}
integer Verse::Trunc      (const rational& a) {return a>=0? Floor(a): Ceil(a);}
integer Verse::Round      (const rational& a) {auto ar=ToRatio(a); auto[q,r]=EuclideanDivision(ar.d*2+ar.D,ar.D*2).Coerce(); return r!=0 || (q&1)==0? q: q-1;}
integer Verse::Numerator  (const rational& a) {return ToRatio(a).d;}
integer Verse::Denominator(const rational& a) {return ToRatio(a).D;}
