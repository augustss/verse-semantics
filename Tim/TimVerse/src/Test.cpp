//==============================================================================================================================================================
// Tests of the library.

// The Theory Group changes to TimVerse are fenced by The TG Pragma
#include "Main.h"

#include "Terse.h"
using namespace Verse;
#pragma warning (disable:4100 4189 4702)

// Static tests.
static_assert(False==False);
static_assert(false<true);
static_assert(option<bool>(False)<Truth(false));
static_assert(option<bool>(False)<Truth(true));
static_assert(Truth(false)<Truth(true));

// Test integer.
nat Random() {static nat Seed=0; Seed=6364136223846793005*Seed+1442695040888963407; return Seed^(Seed>>31);} // Knuth linear congruence generator parameters.
nat RandomPow2() {return Random()>>(2+Random()%62);}
int RandomSgn() {return Random()&1? 1: -1;}
void Problem(nat a,nat b,const char* str) {Print(a,str,b); VERSE_ERR(str);}
void Problem(int64 a,int64 b,const char* str) {Print(a,str,b); VERSE_ERR(str);}

// User types.
nat TestCtorCount=0;
struct test_struct {
	test_struct(const test_struct&) {TestCtorCount++;}
	test_struct() {TestCtorCount++;}
	~test_struct() {TestCtorCount--;}
	test_struct& operator=(const test_struct&)=delete;
};
struct managed_struct: managed {}; // Ensure everything works fine with default constructor.

struct immediate_struct {nat8 x,y;};

struct unique_struct {};
expose_const ExposeUnique(const unique_struct&);

total_ordering ExposeCompare(float64 a,float64 b) {if(a==a||b==b) return{a<=b,a>=b}; else return{0,0};}

// Test polymorphic casting.
struct C0 {virtual void f() {}}; struct C1: C0 {}; struct C2: C1 {}; struct C3: C1 {};
path ExposeSignature(const C1&) {return "/wtf"_VP;}

void TestCasting() {
	static_assert( IsDerived<integer,any>);
	static_assert( IsDerived<any,any>);
	static_assert(!IsDerived<any,integer>);
	static_assert( IsDerived<C0,C0>); 
	static_assert(!IsDerived<C0,C1>); 
	static_assert(!IsDerived<C0,C3>);
	static_assert( IsDerived<C1,C0>); 
	static_assert( IsDerived<C1,C1>); 
	static_assert(!IsDerived<C1,C3>);
	static_assert( IsDerived<C3,C0>); 
	static_assert( IsDerived<C3,C1>);
	static_assert( IsDerived<C3,C3>);
	box<C0> a0=box<C1>();
		VERSE_ENSURE(Cast<box<C0>>(a0));
		VERSE_ENSURE(Cast<box<C1>>(a0));
		VERSE_ENSURE(!Cast<box<C2>>(a0));
		VERSE_ENSURE(!Cast<box<C3>>(a0));
	box<C0> a1=box<C2>();                  // If we remove virtual function, then...
		VERSE_ENSURE(Cast<box<C0>>(a1));   // Succeeds due to static type info.
		VERSE_ENSURE(Cast<box<C2>>(a1));   // Succeeds dynamically due to exact type match.
		VERSE_ENSURE(Cast<box<C1>>(a1));   // Fails dynamically even with new RTTI.
		VERSE_ENSURE(!Cast<box<C3>>(a1));
	any a2=box<C2>();                      // If we remove virtual function, then...
		VERSE_ENSURE(Cast<box<C0>>(a2));   // Fails dynamically even with new RTTI.
		VERSE_ENSURE(Cast<box<C2>>(a2));   // Succeeds dynamically due to exact type match.
		VERSE_ENSURE(Cast<box<C1>>(a2));   // Fails dynamically even with new RTTI.
		VERSE_ENSURE(!Cast<box<C3>>(a2));
	Print(Signature(a0));
	VERSE_ENSURE(Signature(a0)=="/wtf"_VP);
	VERSE_ENSURE(Signature(a1)=="/wtf"_VP);
	//box<C1> a2=box<C0>; // Should be compile-time error.
}

// Test boxing.
struct D0 {integer a; D0(const integer& a0): a(a0) {} box<D0> get_this() const {return *this;}};
struct E0 {nat x; E0(nat x0): x(x0) {} nat operator()(nat y) const {return x+y;}};
void TestBox() {
    auto d0=box<D0>(123);
    VERSE_ENSURE(d0->a==123);
    VERSE_ENSURE(d0->get_this()->a==123);
	auto e0=box<E0>(123);
	VERSE_ENSURE(e0(1)==124);
	any e1=e0;
	auto e2=Coerce<box<E0>>(e1);
	VERSE_ENSURE(e2(2)==125);
	function<nat(nat)> e3=e0;
	VERSE_ENSURE(e3(3)==126);
	//e3(3,6); // Ensure untyped interop version is hidden.
	function<integer(nat8)> e4=e0;
	VERSE_ENSURE(e4(4)==127);
}

// Test polymorphism.
struct Actor       {virtual void f()          {} nat a; Actor     () {a=1;}};
struct Pawn: Actor {        void f() override {} nat b; Pawn      () {a=2; b=3;}};
struct Printy      {virtual void p()          {} nat c; Printy    () {c=4;}};
struct Mill: Pawn, Printy {nat d;};
void TestPolymorphism() {
	Print("--- polymorphism ---");
	any tmp=0;
	if(1) {
		auto A=box<Actor>();     VERSE_ENSURE(A->a==1);
		auto B=box<Pawn>();      VERSE_ENSURE(B->a==2 && B->b==3);
		auto C=box<Printy>();    VERSE_ENSURE(C->c==4);
		auto D=box<Mill>();      VERSE_ENSURE(D->a==2 && D->b==3 && D->c==4);
		box<Actor>     B1=B;     VERSE_ENSURE(B1->a==2);
		//box<Actor>   C1=C; // Should be compile error.
		box<Actor>     D1=D;     VERSE_ENSURE(D1->a==2);
		box<Printy>    D2=D;     VERSE_ENSURE(D2->c==4);
		VERSE_ENSURE(Coerce<box<Mill>>(D1)->a==2);
		VERSE_ENSURE(Coerce<box<Mill>>(D1)->c==4);
		VERSE_ENSURE(Coerce<box<Mill>>(any(D1))->a==2);
		VERSE_ENSURE(Coerce<box<Mill>>(any(D1))->c==4);
		VERSE_ENSURE(Coerce<box<Actor>>(D1)->a==2);
		VERSE_ENSURE(Coerce<box<Actor>>(any(D1))->a==2);
		VERSE_ENSURE(Coerce<box<Printy>>(D1)->c==4);
		VERSE_ENSURE(Coerce<box<Printy>>(any(D1))->c==4);
		//Cast<Actor>(D); // Should be compile error.
		tmp=D2;
	}
	Collect();
	auto DC=Cast<box<Printy>>(tmp); VERSE_ENSURE(DC && DC->c==4);
	auto DA=Cast<box<Actor>>(tmp);     VERSE_ENSURE(DA && DA->a==2);
}

// Variables.
void TestVars() {
	auto iv0=var<integer>(123);
	Collect();
	VERSE_ENSURE(*iv0==123);
	iv0=456;
	VERSE_ENSURE(*iv0==456);
	auto i0=Pow(integer(10),100);
	iv0=i0;
	VERSE_ENSURE(*iv0-1==i0-1);
	Collect();
	VERSE_ENSURE(*iv0-1==i0-1);
	for(nat i=0; i<100; i++)
		iv0=*iv0*2;
	VERSE_ENSURE(Signature(iv0)=="/Verse.org/pointer"_VP);
	static_assert(IsSubtype<int,integer>);
	future<integer> fi=-1;    VERSE_ENSURE(Coerce<int>(fi)==-1);
	auto vi=var<integer>(-1); VERSE_ENSURE(*vi==-1);
	array<int> hm=array{construct_elements{},2,4};
	array<int> hmm=array<int>{2,4,6,8};
	array<int> hmmm=array{2};
	array<int> hmmmm=array{2,4,6,8};
	any vis=var<array<int>>(array{2,4}); VERSE_ENSURE(!Cast<array<>>(vis));
	//auto AIV=var<int>(3),BIV=var<int>(5);
	//VERSE_ENSURE(AIV!=BIV);
	//VERSE_ENSURE(*AIV==3);
	//VERSE_ENSURE(*BIV==5);
	//AIV.Redirect(BIV);
	//VERSE_ENSURE(*AIV==5);
	//VERSE_ENSURE(*BIV==5);
	//VERSE_ENSURE(AIV==BIV);
}

extern "C" void _xabort(unsigned int);
extern "C" unsigned _xbegin(void);
extern "C" void _xend(void);
extern "C" void _mm_pause(void);
extern "C" void _mm_mfence();

template<class f> void ThreadGrind(const char* Test,nat Count,const f& F) {
	Print(Test);
	for(nat i=0; i<Count; i++) RunThread<int>([F,i]() {
		Print<0>("+",i," ");
		F(i);
		Print<0>("-",i," ");
		return int(i);
	});
	while(IsMultithreading())
		Collect();
	Print();
}

VERSE_NO_INLINE nat trivial(nat a,nat b) {
	return a+b;
}
volatile nat pd;
void TestPerformance() {
	nat best=~0U>>1;
	/*for(nat z=0; z<0*10; z++) {
		// AddAtomic cost: add=0.5c,AddAtomic=16c.
		nat n=100,k=10000000,*data=new nat[n];
		nat c0=Clock(); for(nat i=0; i<k; i++) for(nat j=0; j<n; j++) data[j]++;
		nat c1=Clock(); for(nat i=0; i<k; i++) for(nat j=0; j<n; j++) AddAtomic(data+j,1);
		nat c2=Clock(); //Print(n,"B cyc/kops: ",1000*(c1-c0)/n/k," vs ",1000*(c2-c1)/n/k);
		best=Min(best,1000*(c2-c1)/n/k);
	}*/
	//for(nat z=1; z<1*12; z++) {integer a1=2,b1=3; for(nat i=0; i<10000000; i++) a1+=b1;}

	/*nat stuff[65536]{},c0=Clock(); // Benchmark contended operations, cost of contention.
	for(nat i=0; i<16; i++) //managed::RunThread<int>([i,&stuff]() {
		for(nat j=0; j<4096*65536; j++) {             // timing, collision probability -- given 16 threads
			//AddAtomic(&stuff[(j&255)+256*(i&255)],1); //23c  0, vs 17c when single-threaded
			//AddAtomic(&stuff[j&65535],1);             //22c  1/512
			//AddAtomic(&stuff[j&1023],1);              //69c  1/8
			//AddAtomic(&stuff[j&255],1);               //124c 1/2
			//AddAtomic(&stuff[j&15],1);                //653c 1
		}
		return int(i);
	});
	Print("cyc=",(Clock()-c0)*100/4096/65536);*/
	for(nat z=1; z<1*20; z++) {
		// nat     = 2.5c
		// integer = 3c -> 3.5c -> 2.5c (no exceptions) -> 2.5c (no-relocation pinning)
		nat n=10000000;
		nat c0=Clock(); volatile int a0=2,b0=3; for(nat i=0; i<n; i++) {a0=a0+b0; b0=b0+1;}
		nat c1=Clock(); integer      a1=2,b1=3; for(nat i=0; i<n; i++) {a1+=b1; b1+=1;}
        nat c2=Clock(); Print("integer cyc/kops: ",1000*(c1-c0)/n/2," vs ",1000*(c2-c1)/n/2);
		//VERSE_ENSURE(a0==a1);
		best=Min(best,1000*(c2-c1)/n/2);
	}
	Print("best=",best),best=~0U>>1;
	for(nat z=1; z<1*20; z++) {
		// nat     = 1.8c
		// integer = 4c -> 7c -> 6.25c -> 5.6c -> 7c (investigate new regression) -> 6.7c (eliminate memtags512) ->
		//           5.5c (no relocation, no pinning, no hazard for mutable locals) -> 4.5c (latest, no analysis)
		nat n=10000000;
		volatile nat a0=1,b0=1; for(nat i=1; i<100; i++) {a0=a0*i; b0=a0-1;}
		integer      a1=1,b1=1; for(nat i=1; i<100; i++) {a1=a1*i; b1=a1-1;}
		nat c0=Clock(); for(nat i=0; i<n; i++) Swap(a0,b0);
		nat c1=Clock(); for(nat i=0; i<n; i++) Swap(a1,b1);
		nat c2=Clock(); Print("swap cyc/kops: ",1000*(c1-c0)/n/3," vs ",1000*(c2-c1)/n/3);
		VERSE_ENSURE(a0!=a1);
		best=Min(best,1000*(c2-c1)/n/3);
	}
	//!!try also for box<definitely not managed>, box<definitely managed>
	Print("best=",best),best=~0U>>1;
	for(nat z=1; z<1*20; z++) {
		// volatile nat  = 1c
		// integer       = 8c->7.8c->8.3c->2.3c
		// var<nat>      = 240->160->138->112->110c->106c->101c->95c->91c->66c(Cast)->55c(kast-no-copy)->68c(was-unsound)->
		//                 58c(Cast)-> 57c (NRVO-kast)-> 46c (fully fixed & NRVO'd kast)->
		//                 44c (CompareExchangeInterior don't pin)-> 41c (inline tweaks) -> 54c (random regressions) ->
		//                 43c (fix Coerce taking pinned) -> 41c (drop special array&option) ->
		//                 40c (PayloadIsCopyable via bit) -> 39c (pinned ctor don't Advance) ->
		//                 32c (no exceptions) -> 34c (recent regression) -> 33c (eliminate memtags512) ->
		//                 26c (no relocation, no pinning, no hazard for mutable locals)
		// -10c if we write var without CompareExchange
		nat n=10000000;
		nat c0=Clock(); volatile nat a0=2,b0=3; for(nat i=0; i<n; i++) {a0=a0+b0; b0=b0+1;}
		nat c1=Clock(); integer      a1=2,b1=3; for(nat i=0; i<n; i++) {a1=a1+b1; b1=b1+1;}
		nat c2=Clock(); auto a2=var<nat>(2ULL),b2=var<nat>(3ULL); for(nat i=0; i<n; i++) a2=*a2+*b2,b2=*b2+1;
		nat c3=Clock(); Print("var cyc/kops: ",1000*(c1-c0)/n/5," vs ",1000*(c2-c1)/n/5," vs ",1000*(c3-c2)/n/5);
		best=Min(best,1000*(c2-c1)/n/5);
	}
	Print("best=",best),best=~0U>>1;

	/*
	for(nat j=0; j<0*10; j++) {
		nat testc=1000000,c=Clock();
		for(nat i=0; i<testc; i++) {
			nat a=(int32)Random(),b=(int32)Random(); // Full range we can natively multiply.
			if((a*b)!=(integer(a)*integer(b))) Problem(a,b,"*");
		}
		managed::Collect();
		Print("multiplication ok ",(Clock()-c)/1000000); // 1500(normal)
		best=Min(best,(Clock()-c)/1000000);
	}
	for(nat z=0; z<0*100; z++) {
		static volatile nat x0=0,buf[1024];
		nat n=10000000,c0=Clock(),z0=0,z1=1; 
		for(nat i=0; i<n; i++) {
			// We see 1:60000 failure rate maybe due to preemption; best=53c (_xbegin/_xend), 14c (AddAtomic), 4.5c (regular volatile).
			if(1) {
				unsigned r=_xbegin();
				if(r==0xFFFFFFFF) buf[0]++,_xend();
				//else z1++;
			}
			else if(0) AddAtomic(&x0,1);
			else x0=x0+1;
		}
		nat c1=Clock();  
		best=Min(best,1000*(c1-c0)/n);
		Print(1000*(c1-c0)/n," ",z1);
	}
	for(nat z=0; z<0*100; z++) {
		static volatile nat x0=0,x1=1; nat tmp=0,wtf=0;
		nat n=10000000,c0=Clock(); 
		for(nat i=0; i<n; i++) {
			//x0=tmp; tmp=x1;                             // 4cyc
			//x0=tmp; AddAtomic(&wtf,0); tmp=x1;          // 18cyc
			//update_atomic(&x0,[&](nat x){return x+1;}); // 27cyc.
			//x0=tmp; _mm_mfence(); tmp=x1;               // 38cyc
			//x0=x0+x1; //4.1
			x0=trivial(x0,x1); //4.2
		}
		nat c1=Clock(); 
		best=Min(best,1000*(c1-c0)/n);
	}
	*/
}

// Threading.
void TestThreading() {
	if(0) {
		// Currently broken.
		if(1) {
			future<integer> ifu;
			ThreadGrind("--- multithreaded wait race ---",32,[&](nat i) {
				for(nat j=0; j<i*4096; j++)
					WhenResolve(VERSE_HERE("test"),effects,ifu,[](const integer& k) {});
			});
			ifu.Resolve(123);
		}
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded big allocs ---",32,[](nat i) {
			nat n=i*1024+2;
			auto Fibs=For(n,[](nat){return array<future<integer>>();});
			Fibs[0].Resolve(1);
			Fibs[1].Resolve(1);
			for(nat j=2; j<n; j++)
				Fibs[j].Resolve(Coerce<integer>(Fibs[j-2])+Coerce<integer>(Fibs[j-1]));
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded start and stop ---",32,[](nat i) {
			integer a=Pow(integer(2),72);
			integer b=Pow(integer(10),10);
		});
		Collect(1,1);
	}
	if(1) {
		if(1) {
			auto Base=Pow(integer(2),72);;
			auto ivs=For(400,[&](nat i) {return var<integer>(Base);});
			ThreadGrind("--- multithreaded var-pummel ---",32,[=](nat i) {
				for(nat j=0; j<65536; j++)
					ivs[j%400]++;
			});
			integer sum=0;
			for(nat i=0; i<400; i++)
				sum+=*ivs[i]-Base;
			VERSE_ENSURE(sum==65536*32);
		}
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded future-flood ---",32,[](nat i) {
			for(nat j=0; j<i*4096; j++)
				future<nat>();
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded var-flood ---",32,[](nat i) {
			auto a=Pow(integer(2),72);
			for(nat j=0; j<i*4096; j++)
				var<integer>(a+j);
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded local-var ---",32,[](nat i) {
			auto a=var<integer>(Pow(integer(2),72));
			auto b=var<integer>(Pow(integer(10),10));
			for(nat j=0; j<i*4096; j++) {
				auto tmp=*a;
				a=*b+1;
				b=tmp+1;
			}
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded interior-var ---",32,[](nat i) {
			struct gameobj {var<integer> x,y; gameobj(): x(Pow(integer(2),72)), y(Pow(integer(10),10)) {}};
			auto go=box<gameobj>();
			for(nat j=0; j<i*4096; j++) {
				auto z=*go->x;
				go->x=*go->y+1;
				go->y=z+1;
			}
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded future<t> ---",32,[](nat i) {
			for(nat j=0; j<i*128; j++) {
				integer a=1;
				auto afs=For(64,[](nat){return array<future<integer>>();}),bfs=For(64,[](nat){return array<future<integer>>();});
				for(nat k=1; k<64; k++) {
					afs[k].Resolve(afs[k-1]);
					bfs[k].Resolve(a*4);
					a=a*3+k;
				}
				afs[0].Resolve(a*3);
				SettleCollector();
			}
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded array<integer> ---",32,[](nat i) {
			for(nat j=0; j<i*64; j++) {
				array<integer> as=False;
				integer a=1999;
				for(nat k=0; k<32; k++) {
					as+=array<integer>{a};
					a=a*456;
				}
				SettleCollector();
			}
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded integer shuffle ---",32,[](nat i) {
			integer Base=1,Lots[2048]={};
			for(nat j=0; j<CountOf(Lots); j++)
				Base*=(j+1),Lots[j]=j&1? Base: 123;
			for(nat j=0; j<i*0x20000/CountOf(Lots); j++)
				for(nat k=0; k<CountOf(Lots); k++)
					Swap(Lots[k],Lots[Random()&(CountOf(Lots)-1)]);
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded integer ---",32,[](nat i) {
			for(nat j=0; j<i*256; j++) {
				integer a=1,b=1;
				for(nat k=0; k<64; k++) {
					integer d=a+b;
					a=b,b=d;
				}
				SettleCollector();
			}
		});
		Collect(1,1);
	}
	if(1) {
		ThreadGrind("--- multithreaded string ---",32,[](nat i) {
			for(nat j=0; j<i*256; j++) {
				string as="A"_VS,bs="a"_VS;
				for(nat k=0; k<8; k++) {
					string ds=as+bs;
					as=bs,bs=ds;
				}
				SettleCollector();
			}
		});
		Collect(1,1);
	}
	if(0) {
		ThreadGrind("--- multithreaded scripts ---",1,[](nat i) {
			// Can't thread multiple scripts because context isn't thread-local.
			for(nat j=0; j<16; j++) {
				void TestScript(); TestScript();
				SettleCollector();
			}
		});
		Collect(1,1);
	}
	if(0) {
		// Currently disabling due to user page tracking explosion. Bring back when we can untrack empty user pages.
		ThreadGrind("--- multithreaded large allocations ---",16,[](nat i) {
			struct biggie {nat8 pad0[8000]; string s; nat8 pad1[8000]; biggie(nat i): s(ToString("ABCD",i)) {}};
			biggie** biggies=new biggie*[i*1024];
			for(nat j=0; j<i*1024; j++)
				biggies[j]=new biggie(j);
			for(nat j=0; j<i*1024; j++)
				delete biggies[j];
			delete[] biggies;
			SettleCollector();
		});
		Collect(1,1);
	}
}
struct whatevs {nat a; char c;};
void TestArithmetic() {
	nat n=0; n--; Coerce<nat>(integer(n));
	if(1) {
		Print("--- integer ---");
		nat testc=100000, c=0; (void)c;
		any a0=integer(123);
		integer az=1; az+2; 2+az;
		for(nat i=0; i<testc; i++) {
			int64 a=RandomSgn()*RandomPow2(),b=RandomSgn()*RandomPow2();
			if((a+b)!=(integer(a)+integer(b))) Problem(a,b,"+");
			if((a-b)!=(integer(a)-integer(b))) Problem(a,b,"-");
			if((a&b)!=(integer(a)&integer(b))) Problem(a,b,"&");
			if((a^b)!=(integer(a)^integer(b))) Problem(a,b,"|");
			if((a|b)!=(integer(a)|integer(b))) Problem(a,b,"^");
		}
		Print("--- addition ---");
		c-=Clock();
		for(nat i=0; i<testc*4; i++) {
			int64 a=(int32)Random(),b=(int32)Random(); // Full range we can natively multiply.
			if(a*b!=integer(a)*integer(b)) Problem(a,b,"*");
		}
		c+=Clock();
		Print("--- multiplication ---");
		for(nat sizecase=0; sizecase<2; sizecase++) {
			for(nat i=0; i<testc; i++) {
				nat D=Random()>>1,d=Random()&((1ULL<<(sizecase? 47: 7))-1),qp,rp; 
				d+=d==0; qp=D/d; rp=D-qp*d;
				auto[q,r]=EuclideanDivision(integer(D),integer(d)).Coerce();
				if(qp!=q || rp!=r) Problem(D,d," divide_short ");
			}
		}
		Print("--- division ---");
		for(nat i=0; i<testc; i++) {
			nat D=RandomPow2(),d=RandomPow2(),q0=0,r0=0;
			d+=d==0; q0=D/d; r0=D-q0*d;
			auto[q1,r1]=EuclideanDivision(integer(D),integer(d)).Coerce();
			if(q0!=q1 || r0!=r1) Problem(D,d," divide_long ");
			if(D!=q1*d+r1 || r1<0 || r1>=d) Problem(D,d," divide_long ");
		}
		Print("long division ok");
	}
	Collect(1,1);
}
int TestCommon() {
	VERSE_ENSURE(sizeof(integer)==sizeof(any));
	VERSE_ENSURE(sizeof(array<any>)==sizeof(any));
	if(1) {
		Print("--- concepts ---"); // Really fill this out in details so we don't waste monumental amounts of debugging time later.
		static_assert(IsEqual<int8,int8>);
		static_assert(!IsEqual<int8,int16>);
		static_assert(IsSubtype<int8,int8>);
		static_assert(IsSubtype<int8,int16>);
		static_assert(!IsSubtype<int16,int8>);
		static_assert(IsSubtype<int64,integer>);
		static_assert(!IsSubtype<integer,int64>);
	}

	if(1) {
		static_assert(IsComparable<integer>());
		static_assert(IsComparable<char8>());
		static_assert(IsComparable<string>());
		static_assert(!IsComparable<any>());
		static_assert(!IsComparable<array<>>());
		static_assert(IsComparable<rational>());
		static_assert(IsComparable<array<int>>());
		static_assert(IsComparable<path>());
		static_assert(IsEqual<common_type<rational,array<int>>,comparable>);
		static_assert(IsEqual<common_type<string,array<int>>,array<comparable>>);
		static_assert(IsEqual<common_type<rational,box<test_struct>>,any>);
		static_assert(IsEqual<common_type<rational,box<unique_struct>>,comparable>);
		VERSE_ENSURE(Cast<comparable>(any(var<int>(123))));
		VERSE_ENSURE(Cast<comparable>(any(var<bag<int>>())));
		VERSE_ENSURE(Cast<comparable>(any(Pow(integer(256),16))));
		VERSE_ENSURE(Cast<comparable>(any(123)));
		VERSE_ENSURE(!Cast<comparable>(any(BadStep)));
		VERSE_ENSURE(Cast<comparable>(any(array{123,456})));
		VERSE_ENSURE(!Cast<comparable>(any(array{123,BadStep})));
	}

	if(1) {
		// For tweaking natvis.
		any b0=tuple<nat8,char8>(3,'x');
		//box<tuple<nat8,char8>> b1(3,'x');
		nat8 i2[2]={3,6};
		any a(27);
		any b(integer(65536)*65536*65536*65536);
		array<int16> cs{int16(123),int16(456)};//want to vis basic types in decimal, not hex
		any c("Hello"_VS);//want to vis as string, not array of chars
		any d('x');
		any e("Tim"_VS);
		span s0(u8"Hello",5);
		string s1=s0;
		span s2(i2,2);
		any f(array{3,12});
		Print("--- natvis ---"); // Really fill this out in details so we don't waste monumental amounts of debugging time later.
		Coerce<char>(any('x'));
		Coerce<char8>(any('x'));
		future<int> g;
		future<int> h=5;
		integer i=7;
		any j=tuple{};
		any k="Hell"_VS;
		array<int> l{3,5,7,9};
		integer m=integer(123456789)*integer(123456789)*integer(123456789);
		Print("natvis ok");
	}
	Collect(1,1);

	if(1) {
		Print("--- options as tables and sets ---");
		static_assert(IsSubtype<option<int>,function<int(int)>>);
		static_assert(IsSubtype<option<int>,map<int,int>>);
		static_assert(IsEqual<exposed_function_type<option<int>>,map<int,int>>);
		function<int(int)> W0 = Truth(123);
		map<int,int>       W1 = Truth(123);
		auto C1=Length(W1);
		VERSE_ENSURE(C1==1);
		Coerce<option<int>>(W1);
		//VERSE_ENSURE(Keys(W1)==array{123});
		auto W2=Coerce<option<int>>(W1);
		auto W3=W2.Coerce();
		VERSE_ENSURE(W3==123);
		future<option<int>> W4=Truth(123);
		auto W5=Coerce<option<int>>(W4);
		auto W6=W5.Coerce();
		VERSE_ENSURE(W6==123);
	}

	if(1) {
		Print("--- booleans ---");
		any A0=false,A1=true,A2=123;
		auto A3=ToAny(false),A4=ToAny(true);
		VERSE_ENSURE(!A3);
		VERSE_ENSURE(A4);
		VERSE_ENSURE(!Coerce<bool>(A0));
		VERSE_ENSURE(Coerce<bool>(A1));
		VERSE_ENSURE(!Cast<bool>(A2));
		logic LF=false;
		logic LT=true;
		VERSE_ENSURE(!LF);
		VERSE_ENSURE(!bool(LF));
		VERSE_ENSURE(LT);
		VERSE_ENSURE(bool(LT));
		VERSE_ENSURE(!!LT);
		VERSE_ENSURE(!bool(!LT));
	}

	if(1) {
		Print("--- future<t> ---");
		int ic0=0,ic1=0,/*ic2=0,*/ic3=0,ic4=0,ic5=0,ic6=0,ic7=0,ic8=0,ic9=0,ic10=0,/*ic11=0,*/ic12=0,/*ic13=0,*/ic15=0;

		// Future initialization to value.
		WhenResolve(VERSE_HERE("test"),effects,23,[&ic5](int x) {ic5+=x;});
		VERSE_ENSURE(ic5==23);

		// Async future use.
		future<int> if0,if1(123),if2;
		VERSE_ENSURE(!Cast<int>(if0));
		VERSE_ENSURE(Cast<int>(if1));
		VERSE_ENSURE(Cast<int>(if1).Coerce()==123);
		VERSE_ENSURE(!Cast<int>(if0));
		if0.Resolve(456);
		VERSE_ENSURE(Cast<int>(if0));
		VERSE_ENSURE(Cast<int>(if0).Coerce()==456);
		WhenResolve(VERSE_HERE("test"),effects,if0,[&](int v) {ic0+=v;});
		VERSE_ENSURE(ic0==456);
		WhenResolve(VERSE_HERE("test"),effects,if2,[&](int v) {ic1+=v;});
		VERSE_ENSURE(ic1==0);
		if2.Resolve(789);
		VERSE_ENSURE(ic1==789);
		
		// Coercion of futures.
		auto if10=WhenResolve(VERSE_HERE("test"),effects,if0,[&](int v) {return v+1;});
		VERSE_ENSURE(Coerce<int>(if10)==457);

		// Using resolved futures through any.
		future<int> if21(999);
		future<> a21=if21;
		WhenResolve(VERSE_HERE("test"),effects,a21,[&ic3](const any& x) {ic3+=Coerce<int>(x);});
		VERSE_ENSURE(ic3==999);

		// Using unresolved futures through any.
		future<int> if22;
		future<> a22=if22;
		WhenResolve(VERSE_HERE("test"),effects,a22,[&ic4](const any& x) {ic4+=Coerce<int>(x);});
		VERSE_ENSURE(ic4==0);
		if22.Resolve(7);
		VERSE_ENSURE(ic4==7);

		// Covariant resolved future use.
		future<integer> if23=if22;
		WhenResolve(VERSE_HERE("test"),effects,if23,[&ic6](const integer& x) {ic6++;});
		VERSE_ENSURE(ic6==1);

		// Covariant unresolved future use.
		future<int> if24; future<integer> if25=if24;
		WhenResolve(VERSE_HERE("test"),effects,if25,[&ic7](const integer& x) {ic7++;});
		WhenResolve(VERSE_HERE("test"),effects,if25,[&ic7](const integer& x) {ic7++; return 123;});
		VERSE_ENSURE(ic7==0);
		if24.Resolve(98);
		VERSE_ENSURE(ic7==2);

		// Future subtype assignment compatibility.
		future<array<int>> af0(array<int>{1024,2048,4096});
		future<array<integer>> af1=af0;
		future<array<any>> af2=af0;
		future<> af3=af0;

		// Futures on things requiring allocation.
		future<array<integer>> af5;
		future<int> af6=WhenResolve(VERSE_HERE("test"),effects,af5,[&ic8](any x) {ic8++; return 7;});
		auto af7=WhenResolve(VERSE_HERE("test"),effects,af6,[&](any x) {ic9++; return array<integer>{123,456};});
		array<integer>{123,456};
		VERSE_ENSURE(ic8==0);
		VERSE_ENSURE(!Cast<array<integer>>(af5));
		VERSE_ENSURE(!Cast<int>(af6));
		VERSE_ENSURE(!Cast<array<integer>>(af7));
		af5.Resolve(array<int>{2,3,4});
		VERSE_ENSURE(ic8==1);
		VERSE_ENSURE(ic9==1);
		VERSE_ENSURE(Cast<array<integer>>(af5));
		VERSE_ENSURE(Cast<int>(af6));
		VERSE_ENSURE(Cast<array<integer>>(af7));
		WhenResolve(VERSE_HERE("test"),effects,af7,[](array<integer> as) {VERSE_ENSURE(Length(as)==2);});

		// Ensure garbage collection hasn't messed anything up.
		future<int> if30;
		Collect();
		if30.Resolve(123);
		Collect();

		// Futures resolving to resolved futures.
		future<int> if31,if32;
		if31.Resolve(123);
		VERSE_ENSURE(Cast<int>(if31));
		if32.Resolve(if31);
		VERSE_ENSURE(Cast<int>(if32));

		// Futures resolving to unresolved futures.
		future<int> if40,if41;
		future<int> if42=WhenResolve(VERSE_HERE("test"),effects,if40,[&](int x) {ic10+=x; return x+1;});
		future<> a10=if41;
		VERSE_ENSURE(!Cast<int>(if41));
		if41.Resolve(if42);
		VERSE_ENSURE(ic10==0);
		VERSE_ENSURE(!Cast<int>(if41));
		VERSE_ENSURE(!Cast<int>(a10));
		if40.Resolve(123);
		VERSE_ENSURE(ic10==123);
		VERSE_ENSURE(Cast<int>(if41));
		VERSE_ENSURE(Cast<int>(a10));
		WhenResolve(VERSE_HERE("test"),effects,if42,[&](int x) {VERSE_ENSURE(x==124);});
		Collect();

		// Futures resolving to unresolved futures requiring allocation.
		future<array<int>> af40,af41;
		future<array<int>> af42=WhenResolve(VERSE_HERE("test"),effects,af40,[&](const array<int>& xs) {ic12++; return xs;});
		af41.Resolve(af42);
		VERSE_ENSURE(ic12==0);
		af40.Resolve(array<int>{3,5,7,9});
		VERSE_ENSURE(ic12==1);
		WhenResolve(VERSE_HERE("test"),effects,af42,[&](array<any> as) {VERSE_ENSURE(Length(as)==4);});
		Collect();

		// Futures resolving to unresolved any.
		future<> af50,af52;
		future<> af51=af50;
		WhenResolve(VERSE_HERE("test"),effects,af52,[&](const any&) {ic15++;});
		static_assert(IsSubtype<any,future<>>);
		af52.Resolve(af51);
		VERSE_ENSURE(ic15==0);
		af50.Resolve(123);
		VERSE_ENSURE(ic15==1);
		Collect();

		// Subtyping.
		array<int> as50{1,2,3};
		array<future<int>> afs50=as50;
		array<integer> as51=as50;
		Collect();

		// Resolution to future that's not managed.
		future<int> if60=9,if61;
		if61.Resolve(if60);
		//VERSE_ENSURE(CompareDynamic(if61,9)==0);
		Collect();

		// Future pileup.
		auto ifs0=For(256,[](nat){return future<int>();});
		for(nat i=1; i<Length(ifs0); i++)
			ifs0[i].Resolve(ifs0[i-1]);
		ifs0[0].Resolve(10);
		VERSE_ENSURE(Coerce<int>(ifs0[Length(ifs0)-1])==10);
		Collect();

		// Future pileup.
		auto ifs1=For(256,[](nat){return future<int>();});
		for(nat i=1; i<Length(ifs1); i++)
			ifs1[i].Resolve(WhenResolve(VERSE_HERE("test"),effects,ifs1[i-1],[](int x) {return x+1;}));
		ifs1[0].Resolve(10);
		
		Print(ifs1);
		Collect();

		// Going managed with arrays of forwarded elements.
		auto ifs2=For(2,[](nat){return future<string>();});
		ifs2[0].Resolve("abc"_VS);
		VERSE_ENSURE(!Cast<array<string>>(ifs2));
		ifs2[1].Resolve("def"_VS);
		Collect();

		// Synchronous casting.
		VERSE_ENSURE(!Cast<any>(future<integer>()));

		// Must produce error!
		//future<integer>(123).Resolve(456);
		//future{array<int>{2,3}}.Resolve(array<int>{1,2});
	}
	Collect(1,1);

	if(1) {
		Print("--- test ---");

		// Trivial, no possibility of failure.
		int i0=1; bool got0=false;
		auto r0=WhenCast<int>(VERSE_HERE("test0"),effects,i0,
			[&got0](int i)->int {VERSE_ENSURE(i==1); got0=true; return i;},
			[     ](     )->int {VERSE_UNEXPECTED;}
		);
		VERSE_ENSURE(got0 && r0==1);

		// Synchronous success.
		int i1=2; bool got1=false;
		auto r1=WhenCast<int8>(VERSE_HERE("test0"),effects,i1,
			[&got1](int8 i)->int8 {VERSE_ENSURE(i==2); got1=true; return i;},
			[     ](      )->int8 {VERSE_UNEXPECTED;}
		);
		//int8 r1_check=r1;
		//VERSE_ENSURE(got1 && r1==2);
		// Above test notes we could make future-free in cases where we're guaranteed synchronous success or 
		// failure, but that requires specifying for every source type whether casting may fail.

		// Synchronous failure.
		int i2=129; bool got2=false;
		WhenCast<int8>(VERSE_HERE("test0"),effects,i2,
			[     ](const auto& produce) {VERSE_UNEXPECTED;},
			[&got2](                   ) {got2=true;}
		);
		VERSE_ENSURE(got2);

		// Suspending success.
		future<int> ifu3; bool got3=false;
		auto r3=WhenCast<int8>(VERSE_HERE("test0"),effects,ifu3,
			[&got3](int8 i)->int8 {VERSE_ENSURE(i==4); got3=true; return i;},
			[     ](      )->int8 {VERSE_UNEXPECTED;}
		);
		VERSE_ENSURE(!got3);
		ifu3.Resolve(4);
		VERSE_ENSURE(got3);
		VERSE_ENSURE(Coerce<int8>(r3)==4);

		// Trivial tuple.
		tuple<int,int> tufu4{55,66}; bool got4=false;
		/*auto r4=*/WhenCast<tuple<int,int>>(VERSE_HERE("tufu"),effects,tufu4,
			[&got4](const auto& tu1)->tuple<int,int> {VERSE_ENSURE(tu1.template get<0>()==55&&tu1.template get<1>()==66); got4=true; return tu1;},
			[     ](               )->tuple<int,int> {VERSE_UNEXPECTED;}
		);
		VERSE_ENSURE(got4);
		//tuple<int,int> r4_check=r4;

		// Synchronous tuple.
		tuple<int,int> tufu5{50,60}; bool got5=false;
		auto r5=WhenCast<tuple<int8,int8>>(VERSE_HERE("tufu"),effects,tufu5,
			[&got5](const auto& tu1)->tuple<int8,int8> {VERSE_ENSURE(tu1.template get<0>()==50&&tu1.template get<1>()==60); got5=true; return tu1;},
			[     ](               )->tuple<int8,int8> {VERSE_UNEXPECTED;}
		);
		VERSE_ENSURE(got5);
		future<tuple<int8,int8>> r5_check=r5;
		
		any axx=tuple<integer,future<integer>>{77,88}; Coerce<array<>>(axx);

		// Nested futurey tuple success.
		future<integer> ifu6; bool got6=false;
		tuple<integer,future<integer>> tufu6{77,ifu6};
		VERSE_ENSURE(tufu6.get<0>()==77);
		auto r6=WhenCast<tuple<int8,int8>>(VERSE_HERE("tufu"),effects,tufu6,
			[&got6](const auto& tu1)->tuple<int8,int8> {Print(tu1); VERSE_ENSURE(tu1.template get<0>()==77&&tu1.template get<1>()==88); got6=true; return tu1;},
			[     ](               )->tuple<int8,int8> {VERSE_UNEXPECTED;}
		);
		future<tuple<int8,int8>> r6_check=r6;
		VERSE_ENSURE(!got6);
		ifu6.Resolve(88);
		VERSE_ENSURE(got6);
		VERSE_ENSURE((Coerce<tuple<int,int>>(r6).get<1>()==88));

		// Nested futurey tuple failure.
		future<integer> ifu7; bool got7=false;
		tuple<integer,future<integer>> tufu7{77,ifu7};
		WhenCast<tuple<int8,int8>>(VERSE_HERE("tufu"),effects,tufu7,
			[     ](const auto& tu1)->void {VERSE_UNEXPECTED;},
			[&got7](               )->void {got7=true;}
		);
		VERSE_ENSURE(!got7);
		ifu7.Resolve(257);
		VERSE_ENSURE(got7);

		// Arrays.
	}
	Collect(1,1);

	if(1) {
		Print("--- Families ---");
		Print("family-of ",Signature(integer(123)));
		Print(Signature(integer(456)));
		auto v0=box<whatevs>(); any a0=v0;
		auto v1{3.0}; any a1=v1;
		Print(Signature(int(123)));
		Print(Signature(nat8(123)));
		Print(Signature(v0));
		//VERSE_ENSURE(Signature(v0)=="struct whatevs");
		//VERSE_ENSURE(Signature(a0)=="struct whatevs");
		VERSE_ENSURE(Signature(v1)=="/Verse.org/float64"_VP);
		VERSE_ENSURE(Signature(a1)=="/Verse.org/float64"_VP);
		Collect();
	}
	Collect(1,1);

	if(1) {
		Print("--- unboxing ---");
		Print(ToString((nat8)3,(int8)3,(nat16)3,(int16)3,(nat32)3,(int32)3,(nat)3,(int64)3,'X'));
		VERSE_ENSURE(ExposeCompare('X','X')==nullptr);            VERSE_ENSURE(ExposeCompare('X','Y')<nullptr); 
		VERSE_ENSURE(CompareDynamic('X','X')==nullptr);           VERSE_ENSURE(CompareDynamic('X','Y')!=nullptr); 
		VERSE_ENSURE(CompareDynamic(any('X'),any('X'))==nullptr); VERSE_ENSURE(CompareDynamic(any('X'),any('Y'))!=nullptr);
		VERSE_ENSURE(TestCtorCount==0);
		auto t0=test_struct();
		VERSE_ENSURE(TestCtorCount==1);
		any t1=test_struct();
		//VERSE_ENSURE(CompareDynamic(ToCode(t1),string("native'test_struct'"))==0);
		//VERSE_ENSURE(ExposeCompare(ToCode(t1),string("native'test_struct'"))==0);
		//VERSE_ENSURE(ToCode(t1)==string("native'test_struct'"));

		VERSE_ENSURE(TestCtorCount==2);
		//!!no copy ctor, so how could this have ever worked? VERSE_ENSURE(Cast<test_struct>(t1)); // Breaks in Clang if managed allocation doesn't specialize Size==Offset.

		if(1) {
			any t2=test_struct();
			VERSE_ENSURE(TestCtorCount==3);
		}
		Collect();
		VERSE_ENSURE(TestCtorCount==2);

		// Was broken at one point because aggregate initialization of no-constructor derived types zero-inited managed::ManagedForward.
		any t2=box<managed_struct>();
		t2.Context(); 

		// Compiler must reject:
		//box<nat> tmp(0);
		//box<integer> tmp(0)
		//any(true);
	}
	Collect(1,1);

	if(1) {
		Print("--- ExposeCompare ---");
		VERSE_ENSURE(ExposeCompare(3,5)< nullptr);
		VERSE_ENSURE(ExposeCompare(5,3)> nullptr);
		VERSE_ENSURE(ExposeCompare(5,5)==nullptr);
		VERSE_ENSURE(ExposeCompare(3,-5)>nullptr);
		VERSE_ENSURE(ExposeCompare(-5,3)<nullptr);
	}
	Collect(1,1);

	static_assert(IsImmediate<char8>);
	static_assert(IsImmediate<char16>);
	static_assert(IsImmediate<char32>);
	static_assert(IsImmediate<float32>);
	static_assert(!IsImmediate<float64>);

	if(1) {
		Print("--- CompareDynamic ---");
		VERSE_ENSURE(CompareDynamic(any(123),any(123))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(123),any(124))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any('X'  ),any('X'  ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('X'  ),any('Y'  ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any('X'  ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any('Y'  ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any('X'  ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any('Y'  ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any('X'  ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any('Y'  ))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any('X'  ),any(u8'X'))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('X'  ),any(u8'Y'))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(u8'X'))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(u8'Y'))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(u8'X'))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(u8'Y'))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(u8'X'))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(u8'Y'))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any('X'  ),any(u'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('X'  ),any(u'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(u'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(u'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(u'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(u'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(u'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(u'Y' ))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any('X'  ),any(U'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('X'  ),any(U'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(U'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u8'X'),any(U'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(U'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(u'X' ),any(U'Y' ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(U'X' ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(U'X' ),any(U'Y' ))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any(array<int>{3,5}),any(array<comparable>{3,5}))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(array<int>{3,5}),any(array<comparable>{3,7}))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(array<int>{1,5}),any(array<comparable>{3,5}))!=nullptr);

		VERSE_ENSURE(CompareDynamic(any(rational(3)/4),any(rational(6)/8))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(rational(3)/4),any(rational(3)/8))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(rational(4)/2),any(rational(4)/2))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(rational(4)/2),any(2))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(rational(4)/2),any(3))!=nullptr);
		comparable(rational(4)/3);

		VERSE_ENSURE(Cast<integer>(rational(4)/2));
		VERSE_ENSURE(!Cast<integer>(rational(4)/3));
		VERSE_ENSURE(Cast<rational>(any(rational(4)/2)));
		VERSE_ENSURE(Cast<rational>(any(rational(4)/3)));

		VERSE_ENSURE(Cast<char>(comparable('a')));
		VERSE_ENSURE(Cast<char8>(comparable('a')));
		VERSE_ENSURE(Cast<char16>(comparable('a')));
		VERSE_ENSURE(Cast<char32>(comparable('a')));
		VERSE_ENSURE(CompareDynamic(any('a'),any(char('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char8('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char16('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char32('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char8('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char16('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char32('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char8('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char16('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char32('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char8('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char16('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char32('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char8('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char16('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char32('a')))==nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char8('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char16('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any('a'),any(char32('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char8('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char16('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char('a')),any(char32('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char8('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char16('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char8('a')),any(char32('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char8('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char16('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char16('a')),any(char32('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char8('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char16('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(char32('a')),any(char32('b')))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(Pow(integer(2),256)),any(Pow(integer(4),128)))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(Pow(integer(2),256)),any(Pow(integer(3),128)))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(Pow(integer(2),256)),any(123))!=nullptr);
		VERSE_ENSURE(Cast<comparable>(any(rational(2)/3)));
		VERSE_ENSURE(CompareDynamic(any(3.0f),any(4   ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0f),any(3.0f))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0f),any(4.0f))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0f),any(4.0 ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0),any(4   ))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0),any(3.0f))!=nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0),any(3.0 ))==nullptr);
		VERSE_ENSURE(CompareDynamic(any(3.0),any(4.0 ))!=nullptr);
		auto V0=var<int>(123),V1=var<int>(456);
		static_assert(IsComparable<var<int>>());
		VERSE_ENSURE(V0==V0);
		VERSE_ENSURE(V0!=V1);
		VERSE_ENSURE(comparable(V0)==comparable(V0));
		VERSE_ENSURE(comparable(V0)!=comparable(V1));
		VERSE_ENSURE(IsComparableDynamic(V0));
		VERSE_ENSURE(IsComparableDynamic(any(V0)));
		//equality_ordering(CompareDynamic(comparable(V0),V1)); // Overload ambiguity.
		//VERSE_ENSURE(comparable(V0)==V1); // Overload ambiguity.
		//VERSE_ENSURE(comparable(V0)==V1); // Overload ambiguity.
		//VERSE_ENSURE(CompareDynamic(integer(3),any([=]{})).IsIncomparable());
	}
	Collect(1,1);

	if(1) {
		Print("--- Hash ---");
		VERSE_ENSURE(Hash(array{})==Hash(False));
		VERSE_ENSURE(Hash(array{3})==Hash(tuple{3}));
		VERSE_ENSURE(Hash(array{3,5,7})==Hash(tuple{3,5,7}));
		VERSE_ENSURE(Hash(int32(123456))==Hash(int64(123456)));
		VERSE_ENSURE(Hash(int32(123456))==Hash(int64(123456)));
		VERSE_ENSURE(Hash(int32(123456))==Hash(nat64(123456)));
		VERSE_ENSURE(Hash(int32(123456))==Hash(nat64(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(int64(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(int64(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(nat64(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(nat64(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(integer(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(natural(nat(123456))));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(rational(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==Hash(comparable(123456)));
		VERSE_ENSURE(Hash(nat32(123456))==HashDynamic(any(123456)));
		VERSE_ENSURE(Hash(int32(-123456))==Hash(int64(-123456)));
		VERSE_ENSURE(Hash(int32(-123456))==Hash(int64(-123456)));
		VERSE_ENSURE(Hash(int32(-123456))==Hash(integer(-123456)));
		VERSE_ENSURE(Hash(int32(-123456))==Hash(rational(-123456)));
		VERSE_ENSURE(Hash(int32(-123456))==Hash(comparable(-123456)));
		VERSE_ENSURE(Hash(int32(-123456))==HashDynamic(any(-123456)));
		VERSE_ENSURE(Hash('A')==Hash('A'));
		VERSE_ENSURE(Hash('A')==Hash(u8'A'));
		VERSE_ENSURE(Hash('A')==Hash(u'A'));
		VERSE_ENSURE(Hash('A')==Hash(U'A'));
		VERSE_ENSURE(Hash('A')==Hash(comparable('A')));
		VERSE_ENSURE(Hash('A')==Hash(comparable(u8'A')));
		VERSE_ENSURE(Hash('A')==Hash(comparable(u'A')));
		VERSE_ENSURE(Hash('A')==Hash(comparable(U'A')));
		static_assert(IsImmediate<immediate_struct>);
		//HashDynamic(immediate_struct{3,5}); // Runtime error.
		//Hash(immediate_struct{3,5}); // Compile-time error.
		Hash(12.0f);
		Hash(12.0);
		VERSE_ENSURE(Hash(comparable(array{3,5}))==Hash(array{3,5}));
		VERSE_ENSURE(Hash(comparable(array{}))==Hash(array{}));
		VERSE_ENSURE(HashDynamic(any(array<>{}))==Hash(array{}));
		//VERSE_ENSURE(Hash(comparable(Truth(7)))==Hash(Truth(7)));
		//VERSE_ENSURE(Hash(comparable(option<int>()))==Hash(False));
		VERSE_ENSURE(Hash(comparable(Pow(integer(2),256))));
		VERSE_ENSURE(HashDynamic(any(Pow(integer(2),256))));
		auto V0=var<int>(123);
		VERSE_ENSURE(Hash(V0)==Hash(V0));
		VERSE_ENSURE(Hash(comparable(V0))==Hash(V0));
		VERSE_ENSURE(Hash(comparable(V0))==Hash(comparable(V0)));
		
		//future{construct_no_init{}};
		//any{construct_no_init{}};
		//auto WTF=any(construct_no_init{});
	}
	Collect(1,1);

	if(1) {
		Print("--- conversions ---");
		char c0='x'; VERSE_ENSURE(Cast<char8>(c0).Coerce()=='x');
		any f0(123.0); 
		VERSE_ENSURE(Cast<float64>(f0).Coerce()==123.0);
		VERSE_ENSURE(Cast<integer>(123));
		VERSE_ENSURE(!Cast<integer>('x'));
	}
	Collect(1,1);

	if(1) {
		Print("--- tuples ---");
        auto xs=tuple<int,int>{2,'x'};
        any  ys=xs;
        auto zs=tuple{3,'y'};
        ToCode(zs);
        auto ws=array{3,5};
        tuple{1,2}+tuple{u8'x',7};
		//common_type<tuple<char8,char8>,array<char8>> wtf=1234; // Expect error.
		static_assert(IsSubtype<tuple<char8,char8>,array<char8>>);
        string s=tuple{u8'x',u8'y'};
        Print("hello ",s);
        VERSE_ENSURE((tuple{3,5}==tuple{3,integer(5)} && tuple{3,5}!=tuple{3,6}));
		static_assert(std::tuple_size<tuple<int,char>>::value==2);
		static_assert(IsEqual<typename std::tuple_element<0,tuple<int,char>>::type,int>);
		static_assert(IsEqual<typename std::tuple_element<1,tuple<int,char>>::type,char>);
		const auto& xtu=tuple<int,char>{2,'z'};
		VERSE_ENSURE(xtu.get<0>()==2);
		VERSE_ENSURE(xtu.get<1>()=='z');
		auto[xi,xj]=xtu;
		VERSE_ENSURE(xi==2);
		VERSE_ENSURE(xj=='z');
    }
	Collect(1,1);

	if(1) {
		Print("--- function ---");
		//int(*xf1p)(int)=nullptr;
		//callable<decltype(xf1p)> wtf; -wtf;

        auto f1=[](int x) {return x+1;}; int(*f1p)(int)=f1; function g1(f1p); any g1a=(int(*)(int))f1; VERSE_ENSURE(g1(2)==3);
        //!!NEW ERROR: truncation from int(*)(int) to bool: auto f2=[](int x) {return x+1;}; function g2(f2); any g2a=f2; VERSE_ENSURE(g2(2)==3); 
        //!!SAME auto f3=[](int x) {return x+2;}; function g3(f3); any g3a=f3; VERSE_ENSURE(g3(2)==4); 
        int counter=3; auto f4=[&](int x){return x+(counter++);}; function g4(f4); any g4a=f4; VERSE_ENSURE(g4(2)==5);
        auto f5=[&](int x,int y) {return x+y+counter++;}; function g5=f5; VERSE_ENSURE(g5(3,4)==11);
		//auto f6=function(operator*); // Shouldn't be able to deduce template arguments since overloaded.
		//auto f7=function<rational(const rational&,const rational&)>(operator*); // Can we make this Work?
    }
	Collect(1,1);

	if(1) {
		Print("--- var ---");
		TestVars();
		auto a=var<integer>(Pow(integer(2),72));
		for(nat j=0; j<4096; j++)
			a++;
	}
	Collect(1,1);

	if(1) {
		Print("--- var<bag> ---");
		var<bag<int>> S;
		S.Set(3), S.Set(5);
		array<int> NS;
		for(auto[N,_]:S) {
			Print("GOT ",N);
			if(N==3) S.Set(7);
			else if(N==7) S.Set(9), S.Set(11);
			NS+=array{N};
		}
		VERSE_ENSURE((NS==array{3,5,7,9,11}));
	}
	Collect(1,1);

	if(1) {
		Print("--- array ---");

		// Construction and lifetime.
		auto is2=array<int64  >{60,70}; Print(ToCode(is2));
		auto is3=array<integer>{70,80}; Print(ToCode(is3));
		auto is4=array<any    >{80,90}; Print(ToCode(is4));
		auto ss0a=array<any>{is2};

		auto ss0b=array<any>{is2,is3,array<any>{30,40}};
		Collect();

		// Destructors.
		if(1) {
			array<test_struct> zss{test_struct(),test_struct()};
			VERSE_ENSURE(TestCtorCount==2);
		}
		Collect();
		static_assert(HasDestructor<managed_array_flat<test_struct>>);
		VERSE_ENSURE(TestCtorCount==0);

		array<integer> is5=is3;
		auto ss1=array<array<integer>>(False);
		auto ss2=array<array<integer>>{is3};
		auto ss3=array<array<integer>>{is3,array<integer>{40,50}};
		auto ss4=array<array<any>>    {is3,array<integer>{60,70}};

		// Conversions.
		//array<integer> is4a=is4; // Expect compiler error.
		array<any> is3a=is3; VERSE_ENSURE(CompareDynamic(is3a[0],is3[0])==nullptr);
		array<integer> is2a=is2;
		array<any> is2b=is2a;
		VERSE_ENSURE(is2a==is2a);
		VERSE_ENSURE(is2a[1]==70);
		VERSE_ENSURE(CompareDynamic(is2b[1],70)==nullptr);

		// Strange arrays.
		auto shorts=array<nat16>{nat16(0),nat16(1),nat16(2)};
		Print("shorts ",ToCode(shorts)," ",ToCode(shorts[1]));
		char ch0='x'; any ch1=ch0;
		//array<int16> shorts1=shorts; // Expect compiler error.
		array<int32> shorts1=shorts;
		VERSE_ENSURE(shorts1[2]==2);

		auto a0=array<integer>{2,3}; array<any> a1=a0;
		auto a2=array<array<any>>{a0,a1,a0};
		auto a3=array<integer>{1};
		VERSE_ENSURE(Cast<array<integer>>(a3));
		VERSE_ENSURE(Cast<array<int>>(a3));
		int64 bsrc[]={333,444,555};
		array<int64>    b0=span<int64>((const int64*)bsrc,3);
		array<int64>    b1=span<int64>(bsrc,3);
		array<integer>  b3=span<int64>(bsrc,3);
		array<any>      b4=span<int64>(bsrc,3);
		VERSE_ENSURE(Cast<array<any>>(b0));
		VERSE_ENSURE(Cast<array<int64>>(b0));
		VERSE_ENSURE(Cast<array<integer>>(b0));
		VERSE_ENSURE(!Cast<array<nat8>>(b0));
		//auto b5=array<int16>::reference(bsrc,3,true); // Expect compiler error.
		integer cx0=0;
		for(auto axxx:a0) cx0+=axxx;
		VERSE_ENSURE(cx0==5);

		any c0="Tim"_VS;
		auto c1=string("ABC");
		string c2=string("ABC");
		VERSE_ENSURE(Length(c1)==3 && c1(0)==char(65) && c1(1)==char(66));

		// Constructing using variadic template.
		auto d0=array<nat>{3,5,7};
		Print(ToCode(d0.Sort()));

		// Concatenation.
		auto e=string("Tim Sweeney Is Awesome");
		auto f=string(False);
		VERSE_ENSURE(Length(e)==22);
		VERSE_ENSURE(Length(f)==0);
		VERSE_ENSURE(Length(e+f)==22);
		VERSE_ENSURE(Length(e+c1)==25);
		VERSE_ENSURE(e==e);
		VERSE_ENSURE(e!=f);
		array<any> wat=e+b1;

		// Slice.
		VERSE_ENSURE(e(1)==char('i'));
		VERSE_ENSURE(Length(e.Slice(1,21))==20);
		VERSE_ENSURE(e.Slice(1,21)(1)==char('m'));

		// Sorting.
		for(nat i=0; i<1024; i++) {
			array<integer> is0=False; nat n=0;
			while((Random()&31)!=0 && n<400)
				is0+=array<integer>{Random()},n++;
			auto is1=is0.Sort();
			VERSE_ENSURE(Length(is1)==Length(is0));
			for(nat j=0; j<n; j++) {
				VERSE_ENSURE(j==0 || CompareTotal(is1[j-1],is1[j])<=nullptr);
				bool found=0;
				for(nat k=0; k<n; k++) if(CompareTotal(is1[k],is0[j])==nullptr) found=1;
				VERSE_ENSURE(found);
			}
		}

		// Order.
		//auto g0=array<any    >{1,10,20,3,2,1,90,80,1,11,12,17,15,13,12,1}; Print(ToCode(g0.Sort()));
		//auto g1=array<any    >{123,'z',c0,c1,'a',d,d0,d,456};              Print(ToCode(g1.Sort()));
		auto g2=array<integer>{1,10,20,3,2,1,90,80,1,11,12,17,15,13,12,1}; Print(ToCode(g2.Sort()));
		auto g3=array<int    >{1,10,20,3,2,1,90,80,1,11,12,17,15,13,12,1}; Print(ToCode(g2.Sort()));

        // Static constant strings.
        VERSE_ENSURE("Hello"_VS==string("Hello"));

		// Functions.
		VERSE_ENSURE(!Cast<array<any>>(function{[](any x) {return x;}}));
		auto f100=function<nat(string)>([=](const string& s) {return Length(s);});
		VERSE_ENSURE(f100("Hello"_VS)==5);
		function<> f101=f100;
		VERSE_ENSURE(Cast<function<>>(any(f100)));
		VERSE_ENSURE(Cast<function<>>(f100));

		// Ranges.
		auto h0=array<integer>(construct_range{},6);
		integer h1=0;
		for(auto h:h0)
			h1+=h;
		VERSE_ENSURE(h1==15);
    }
	Collect(1,1);

	if(1) {
		Print("--- option<t> ---");
        option<int>a=Truth(3);
        option<integer> b=a;
        option<integer> c=Truth(4);
		integer d=123;
		VERSE_ENSURE(d==d);
		VERSE_ENSURE(b==b);
        VERSE_ENSURE(a && b && a==b && a.Coerce()==b.Coerce());
        VERSE_ENSURE(c&&a!=c&&a.Coerce()!=c.Coerce());
        //VERSE_ENSURE(ToCode(b)=="bag{3}");
    }
	Collect(1,1);

	TestArithmetic();
    if(1) {
        Print("--- rational ---");
        rational a=3;
        rational b=7;
        VERSE_ENSURE(Cast<rational>(a));
        Print(a+b);
        Print(b-a);
        Print(a/b," ",b/a);
        VERSE_ENSURE(Cast<rational>(a/b));
        VERSE_ENSURE(!Cast<integer>(a/b));
        Print(rational(1024)/16);
        VERSE_ENSURE(CompareTotal(a/b,b/a)<nullptr);
        rational sum=0;
        for(nat i=0; i<256; i++)
            sum+=rational(i+1)/(i+2);
        Print(sum);
    }
    Collect(1,1);

	if(1) {
		Print("--- Cast ---"); 
		TestCasting();
	}
	Collect(1,1);

	if(1) {
		Print("--- box<t> ---"); 
		TestBox();
	}
	Collect(1,1);

    if(1) {
		Print("--- Iterate ---"); 

		{future<int> rfu; Iterate(VERSE_HERE("test"),False,AllowIteratesFx(Thread->AllowFx),effects,
            [=]{return LeaveIterateStep;},
            [=]{return rfu.Resolve(1);},
            [=]{return rfu.Resolve(5);}
		);
		VERSE_ENSURE(Coerce<int>(rfu)==1);}

        {future<int> rfu; Iterate(VERSE_HERE("test"),False,AllowIteratesFx(Thread->AllowFx),effects,
            [=]{StuckFx(only_cardinalities&fails); return LeaveIterateStep;},
            [=]{return rfu.Resolve(1);},
            [=]{return rfu.Resolve(5);}
        );
		VERSE_ENSURE(Coerce<int>(rfu)==5);}

		{future<int> rfu,ifu; Iterate(VERSE_HERE("test"),False,AllowIteratesFx(Thread->AllowFx),effects,
            [=]{WhenResolve(VERSE_HERE("test"),effects,ifu,[](int i) {}); return LeaveIterateStep;},
            [=]{return rfu.Resolve(Coerce<int>(ifu)*2);},
            [=]{return rfu.Resolve(5);}
		);
        VERSE_ENSURE(!Cast<int>(rfu));
        ifu.Resolve(10);
        VERSE_ENSURE(Cast<int>(rfu));
		VERSE_ENSURE(Coerce<int>(rfu)==20);}

		/*
		// This used to work, but C++ exceptions were too slow, so now they require stepping.
		{future<int> rfu; Iterate(VERSE_HERE("test"),False,AllowIteratesFx(Thread->AllowFx),
            [=](     ) {return ThrowAny(123);},
            [=](     ) {return rfu.Resolve(x+1);},
            [=](     ) {return rfu.Resolve(5);},
            [=](any a) {return rfu.Resolve(6);}
        );
		VERSE_ENSURE(Coerce<int>(rfu)==6);}*/
	}
	Collect(1,1);

	TestPolymorphism();
	Collect(1,1);

	Print("exit");
	return 0;
}
void TestPrintFx() {
	for(auto i=0; i<3; i++) {
		auto DefaultFx=i==0? no_effects: i==1? effects: function_defaults;
		Print();
		Print("Default: ",DefaultFx);
		Print(ToCode(accepts,DefaultFx));
		Print(ToCode(rejects,DefaultFx));
		Print(ToCode(function_defaults,DefaultFx));
		Print(ToCode(function_allows,DefaultFx));
		Print(ToCode(effects,DefaultFx));
		Print(ToCode(no_effects,DefaultFx));
		Print(ToCode(abstracts,DefaultFx));
		Print(ToCode(function_defaults&reads,DefaultFx));
		Print(ToCode(function_defaults&transacts,DefaultFx));
		Print(ToCode(function_allows&succeeds&suspends,DefaultFx));
		Print(ToCode(function_defaults&converges,DefaultFx));
		Print(ToCode(function_defaults&computes,DefaultFx));
		Print(ToCode(function_defaults+only_rejects,DefaultFx));
	}
}

#ifdef TG
// A different stating point is in Main.cpp::main()
#else
int main() {
	runtests();
}
#endif
int runtests() {
	//TestPerformance();
	//TestPrintFx();
#ifdef NDEBUG
	TestCommon(); // Never disable these tests; must discover breakage when it occurs.
#endif
	void TestScript(); TestScript(); 
	Collect(1,1); 
	return 0;
	TestArithmetic();
	for(nat z=0; z<16384; z++) 
		Print(), Print("loop: ",z),TestThreading();
} 