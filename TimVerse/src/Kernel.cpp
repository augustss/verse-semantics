//==============================================================================================================================================================
// Library kernel.

#include "Terse.h"
#pragma warning(disable:4100 4189 4706)
#if __clang__
#pragma clang diagnostic ignored "-Wbitwise-op-parentheses"
#endif
using namespace Verse;

// Statics.
static nat PageTaken,PageAllocated,PageFreed,CollectorThreads,MemoryTagsPages,PageTagsPages,CollectionsCount;
static nat RootCount,ReachedCount,DeadCount,UserPagesActive,UserPagesCount;

// Stress test.
void Preempt() {
#ifndef NDEBUG
	//static nat PreemptCount;
	//if(++PreemptCount==6661) {PreemptCount=0; if(managed::IsMultithreading()) Sleep(1);}
#endif
}

// Errors.
[[noreturn]] void R99() {VERSE_ERR("out of memory");}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Payload characterization.

// Nonvalue ids nonvalue_.
static const nat NonValueValue            = 0;
static const nat NonValueContext          = 1;
static const nat NonValueSuspension       = 2;
static const nat NonValueGlobal           = 4;
bool kernel::IsResolvedFuture (nat NonValueType) {return NonValueType-1>2;}
bool kernel::IsWaitingFuture  (nat NonValueType) {return NonValueType==NonValueSuspension;}
bool kernel::IsPointer        (nat NonValueType) {return NonValueType>=NonValueGlobal;}

// Payload pf_ shifts, flags, and masks.
static const nat8  PayloadMethodsShift    = 35;
static const nat8  PayloadImmediateShift  = 3;
static const nat8  PayloadNonValueShift   = 53;
static const nat8  PayloadSignShift       = 13;
static const nat   PayloadNonValueFlag    = 0x0000000000000004;
static const nat   PayloadMethodsMask     = 0x0000000000007FFF;
static const nat   PayloadPointerMask     = 0x00007FFFFFFFFFF8;
static const nat   PayloadValueMask       = 0x0007FFFFFFFFFFFB;
//static const nat   PayloadSignMask      = 0x0004000000000000;
static const nat   PayloadTransactionMask = 0xFFF0000000000004;
static const nat   PayloadCategoryMask    = 0x0000000000000007;
static const nat   PayloadInterior        = 0x0002000000000002;
static const nat   PayloadZero            = 0x0000000000000001;
static const nat   PayloadContext         = PayloadNonValueFlag|NonValueContext<<PayloadNonValueShift;
static const nat   PayloadNewVar          = PayloadNonValueFlag|NonValueGlobal <<PayloadNonValueShift;
static_assert(CountOf(ImmediateMethodsOfIndex)<=PayloadMethodsMask+1);

// Payload pfc_ categories.
static const nat   PayloadCategoryManaged          = 0x00;
static const nat   PayloadCategoryImmediateInteger = 0x01;
static const nat   PayloadCategoryInterior         = 0x02;
static const nat   PayloadCategoryImmediateIndexed = 0x03;

// Tests.
bool kernel::PayloadIsImmediateInteger(nat P) {return (P&PayloadCategoryMask)==PayloadCategoryImmediateInteger;}
bool kernel::PayloadIsBox             (nat P) {return (P&PayloadCategoryMask)==PayloadCategoryManaged;} // Correct even with nonvalue, due to 0x4 mask.
nat  kernel::PayloadMethodsIndex      (nat P) {constexpr nat8 shifts[4]={0,63,48,35}; return P>>shifts[P&3];}
bool kernel::PayloadIsManaged         (nat P) {return (P&1)==0;}
static bool  PayloadIsCopyable        (nat P) {return (P&4)==0;}
static bool  PayloadIsInterior        (nat P) {return (P&PayloadCategoryMask)==PayloadCategoryInterior;}
static nat   PayloadSignExtend        (nat P) {return nat(int64(P&PayloadValueMask)<<PayloadSignShift>>PayloadSignShift);}

// Page tags, pt_.
const  nat  PageTagSmall             = 0x0000000000000001;
const  nat  PageTagStatic            = 0x8000000000000000;
static nat  IsPageSmall(nat Size)    {return PayloadPointerMask&~(Size-1)|PageTagSmall;}
static nat  IsPageLarge(nat8* Start) {return nat(Start);}
static bool IsPageManaged(nat PT)    {return PT!=0 && !(PT&PageTagStatic);}

// Forward declarations.
static managed* InsideManaged(const pin<future<>>&);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Immediates.

immediate_methods Verse::ImmediateMethodsOfIndex[2048]={
	ImmediateMethodsOfType<Internal::integer_immediate>(),
	ImmediateMethodsOfType<Internal::integer_immediate>(),
	ImmediateMethodsOfType<Internal::ptr_immediate>(),
};
bool kernel::IsIndexed(const pin<any>& A,nat Index) {
	nat P=A.Payload;
	return ((P&7)==PayloadCategoryImmediateIndexed) & (((P>>PayloadMethodsShift)&PayloadMethodsMask)==Index);
}
bool Verse::IsZero(const future<>& F) {
	return F.Payload==PayloadZero;
}
nat kernel::EncodeImmediate(nat Immediate,nat MethodsIndex) {
	return PayloadCategoryImmediateIndexed | MethodsIndex<<PayloadMethodsShift | nat(Immediate)<<PayloadImmediateShift;
}
nat kernel::DecodeImmediateRaw(nat Payload) {
	return Payload>>PayloadImmediateShift;
}
nat32 kernel::DecodeImmediate32(nat Payload) {
	return nat32(Payload>>PayloadImmediateShift);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Memory tags.

// Memory tags for managed objects mtm_, overlaid on managed vtbl.
static const nat8 MemoryTagManagedStart = 0x80;
static const nat8 MemoryTagUnreached    = 0x80; // Dijkstra white.
static const nat8 MemoryTagReached      = 0xC0; // Dijkstra grey.
static const nat8 MemoryTagScanned      = 0xE0; // Dijkstra white.
static const nat8 MemoryTagStartShift   = 7;
static const nat8 MemoryTagManagedShift = 0;

// Memory Tags for Payloads mtp_, overlaid on payloads.
// For ManagedForward, exclusive write-owned by user Allocate thread before it clears PayloadTagUninitialized.
// If constructor throws, ~managed invokes and clears ManagedForward PayloadTagUninitialized, allowing gc.
// For other payloads, future<> thread sets, anyone may add PayloadTagManaged, cleared by ~future<>.
static const nat8 PayloadTagManaged       = 0x01; // This location contains a managed Payload or a resolved future.
static const nat8 PayloadTagMutable       = 0x02; // This location may contain a managed Payload now or in the future.
static const nat8 PayloadTagUninitialized = 0x04; // On ManagedForward, indicates constructor hasn't completed.
static const nat8 PayloadTagHasDestructor = 0x08; // On ManagedForward, indicates object has destructor.

// Helpers.
nat8 ToPayloadTag(nat P) {return (P&1)^1;}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Platform memory management.
// TODO: Figure out how to generalize between Windows&Linux, Intel&ARM.

static const nat PageSize   = 4096;           // Size of operating system page.
static const nat PagesSize  = 65536;          // Size of minimum complete allocation the operating system allows.
static const nat AddressEnd = 0x800000000000; // One past last valid address.

struct CONTEXT            {nat32 ContextFlags;}; // More...
struct EXCEPTION_RECORD   {nat32 ExceptionCode,ExceptionFlags; EXCEPTION_RECORD* ExceptionRecord; void* ExceptionAddress; nat32 NumberParameters; nat ExceptionInformation[15];};
struct EXCEPTION_POINTERS {EXCEPTION_RECORD* ExceptionRecord; CONTEXT* ContextRecord;};
struct MEMORY_BASIC_INFORMATION {void* BaseAddress; void* AllocationBase; nat32 AllocationProtect; nat RegionSize; nat32 State,Protect,Type;} mbi;

static const nat32 MEM_COMMIT=0x00001000,MEM_RESERVE=0x00002000;
static const nat32 MEM_DECOMMIT=0x4000/*,MEM_RELEASE=0x8000*/;
static const nat32 MEM_TOP_DOWN=0x00100000;
static const nat32 EXCEPTION_ACCESS_VIOLATION=0xc0000005;
static const nat32 PAGE_NOACCESS=0x01,/*PAGE_READONLY=0x02,*/PAGE_READWRITE=0x04/*,PAGE_GUARD=0x100*/;
static const int32 /*EXCEPTION_EXECUTE_HANDLER=1,*/EXCEPTION_CONTINUE_SEARCH=0,EXCEPTION_CONTINUE_EXECUTION=-1;

extern "C" void* __stdcall VirtualAlloc(volatile void* lpAddress,nat dwSize,nat32 flAllocationType,nat32 flProtect);
extern "C" nat32 __stdcall VirtualFree(volatile void* lpAddress,nat dwSize,nat32 dwFreeType);
extern "C" void* __stdcall AddVectoredExceptionHandler(nat32 FirstHandler,int32(__stdcall*handler)(EXCEPTION_POINTERS*));
extern "C" nat32 __stdcall VirtualProtect(volatile void* lpAddress,nat dwSize,nat32 flNewProtect,nat32* lpflOldProtect);
extern "C" void  __stdcall GetCurrentThreadStackLimits(int64* lo,int64* hi);
extern "C" nat32 __stdcall VirtualQuery(const void* lpAddress,MEMORY_BASIC_INFORMATION*,nat);
extern "C" nat8  __stdcall _BitScanForward64(nat32* Index,nat mask);

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Threading.

// Types.
static const nat8 ThreadStateNormal=0,ThreadStateLocked=1,ThreadStateEnding=2;
static const nat  LargeBin=0,BinCount=13;
struct page_link {
	nat8*      Start;
	nat8*      Stop;
	page_link* Next;
};
struct thread_link {
	// Written exclusively by GC, never by contained thread.
	nat volatile*         PayloadReadHazard;
	thread_link*	      Next;
	page_link* volatile   ManagedPages[BinCount];
	thread_link*          NextKill;
	volatile nat8	      ThreadStatus;
};
struct tracked_region_link {
	nat8* Address;
	bool  Duplicate;
	tracked_region_link* Next;
};

// Thread locals.
static thread_local thread_link*	ThreadLocalLink;
static thread_local nat volatile	ThreadLocalHazard;
static thread_local nat8*			ThreadLocalTop[BinCount];
static thread_local bool            ThreadLocalStartedByRunThread;
void Internal::thread_startup::PreThreadStartup() {
	ThreadLocalStartedByRunThread=1;
}

// Globals.
static page_link* volatile          UnsizedSmallPages;
static thread_link                  *FirstThread,*FirstKill,*FirstGraveyard;
static volatile nat8				IsCollecting;
static volatile nat8*				MemoryTags;
static volatile nat*				PageTags;
static tracked_region_link*			FirstUnmanagedRegion;

// Accessors.
auto MemoryTagsOf  (nat                  P) {VERSE_ASSERT((P&7)==0); return &MemoryTags[    P >>3];}
auto MemoryTagsOf  (const volatile void* P) {                        return &MemoryTags[nat(P)>>3];}
bool ClearMemoryTags(page_link* Link) {
	for(auto i=nat(Link->Start)/8,Stop=nat(Link->Stop)/8; i<Stop; i++)
		if(MemoryTags[i]!=0)
			return false;
	return true;
}

// Page flags, pt_.
volatile nat* PageTagsOf(const void* P) {return &PageTags[nat(P)/PageSize];};
volatile nat* PageTagsOf(nat         P) {return &PageTags[P/PageSize];};

// Allocation.
nat8* AllocatePages(nat Size) {
	if(auto Pages=(nat8*)VirtualAlloc(0,Size,MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE))
		if(VirtualAlloc(MemoryTagsOf(Pages),Size/8,MEM_COMMIT,PAGE_READWRITE))
			if(VirtualAlloc(PageTagsOf(Pages),Size/PageSize*sizeof(PageTags[0]),MEM_COMMIT,PAGE_READWRITE))
				return Pages;
	R99();
}
VERSE_NO_INLINE static void AllocateSizedPage(nat Bin,nat Size) {
	for(;;) {
		if(page_link* Link=UnsizedSmallPages) {
			// We have a shared unsized page available for reuse, so try to obtain it.
			if(CompareExchangeAtomic(&UnsizedSmallPages,Link->Next,Link)==Link) {
				AddAtomic(&PageAllocated,1);
				VERSE_ENSURE(CompareExchangeAtomic(PageTagsOf(Link->Start),IsPageSmall(Size),0)==0); // Detect races.
				Link->Next				           = ThreadLocalLink->ManagedPages[Bin];
				ThreadLocalLink->ManagedPages[Bin] = Link;
				ThreadLocalTop[Bin]		           = Link->Start;
				VERSE_ASSERT(ClearMemoryTags(Link));
				return;
			}
			else continue;
		}
		else {
			// Allocate more shared unsized pages.
			auto Pages=AllocatePages(PagesSize);
			AddAtomic(&PageTaken,PagesSize/PageSize);
			auto Page  = Pages+PagesSize-PageSize;
			Link       = new page_link{Page,Page+PageSize,UnsizedSmallPages};
			auto nextp = &Link->Next;
			for(Page-=PageSize; Page>=Pages; Page-=PageSize)
				Link=new page_link{Page,Page+PageSize,Link};
			while(!LinkAtomic(&UnsizedSmallPages,Link,nextp));
		}
	}
}
VERSE_NO_INLINE nat8* AllocateMultiplePages(nat Size) {
	Size=(Size+PageSize-1)&~(PageSize-1);
	auto Pages     = AllocatePages(Size);
	auto PageCount = Size/PageSize;
	AddAtomic(&PageAllocated,int64(PageCount));
	for(auto i=nat(Pages)/PageSize,end=i+PageCount,PT=IsPageLarge(Pages); i<end; i++)
		PageTags[i]=PT;
	ThreadLocalLink->ManagedPages[LargeBin]=new page_link{Pages,Pages+Size,ThreadLocalLink->ManagedPages[LargeBin]};
	return Pages;
}
managed* kernel::AllocationBase(nat Address) {
	auto PageTag = *PageTagsOf(Address);
	nat  Base    = PageTag&(Address|((PageTag&1)-1));
	//!!broken in startup initialization of context_managed VERSE_ASSERT(*MemoryTagsOf(Base)&MemoryTagManagedStart);
	return (managed*)Base;
}
managed* kernel::AllocationBaseSpeculative(nat Address) {
	if(nat PageTag=*PageTagsOf(Address&PayloadPointerMask))
		if(nat Base=PageTag&(Address|((PageTag&1)-1)); *MemoryTagsOf(Base)&MemoryTagManagedStart)
			return (managed*)Base;
	return 0;
}
nat AllocationSize(void* Start,volatile nat* PTP) {
	if(auto PageTag=*PTP; PageTag&PageTagSmall)
		return PayloadPointerMask&(~(PageTag-PageTagSmall)+1);
	else
		for(auto StartPTP=PTP++;; PTP++)
			if(*PTP!=PageTag)
				return (PTP-StartPTP)*PageSize;
}
nat AllocationSize(void* Start) {
	return AllocationSize(Start,PageTagsOf(Start));
}
static int32 __stdcall HandlePageFault(EXCEPTION_POINTERS* ExceptionInfo) {
	if(ExceptionInfo->ExceptionRecord->ExceptionCode==EXCEPTION_ACCESS_VIOLATION) {
		void* FaultAddress=(void*)ExceptionInfo->ExceptionRecord->ExceptionInformation[1];
		if(FaultAddress>=MemoryTags && FaultAddress<MemoryTags+AddressEnd/8) {
			// 4KB MemoryTags representing 1B/8B fault granularity means 32KB tracked region granularity.
			nat8* TagsPage  = (nat8*)(nat(FaultAddress)&~(PageSize-1));
			nat8* DataPages = (nat8*)(8*(TagsPage-MemoryTags));
			if(!IsPageManaged(*PageTagsOf(DataPages))) { 
				// Check because Track->free->manage->free->manage can lead to GC tracked-traversal MemoryTags fault.
				// Better solution: set pt_tracked here; have mechanism for clearing pt_tracked and decommitting MemoryTags when empty,
				// with GC removing tracked pages that then lack pt_tracked.
				auto Address = (nat8*)(nat(DataPages)&~(PageSize*8-1));
				auto Track   = new tracked_region_link{Address,0,FirstUnmanagedRegion};
				while(!LinkAtomic(&FirstUnmanagedRegion,Track,&Track->Next));
			}
			AddAtomic(&MemoryTagsPages,1);
			VERSE_ENSURE(VirtualAlloc(TagsPage,PageSize,MEM_COMMIT,PAGE_READWRITE)); // Must do last, to prevent dangle.
			return EXCEPTION_CONTINUE_EXECUTION;
		}
		if(FaultAddress>=PageTags && FaultAddress<PageTags+AddressEnd*sizeof(int64)/PageSize) {
			auto Page = (void*)(nat(FaultAddress)&~(PageSize-1));
			AddAtomic(&PageTagsPages,1);
			VERSE_ENSURE(VirtualAlloc(Page,PageSize,MEM_COMMIT,PAGE_READWRITE));
			return EXCEPTION_CONTINUE_EXECUTION;
		}
	}
	return EXCEPTION_CONTINUE_SEARCH;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Collector static and thread_local init and exit.

static thread_link* InitThread() {
	AddAtomic(&CollectorThreads,1);
	auto Link=new thread_link{&ThreadLocalHazard,FirstThread};
	while(!LinkAtomic(&FirstThread,Link,&Link->Next));
	return Link;
}
kernel::kernel() {
	Print("kernel init");
	VERSE_ASSERT(!MemoryTags && !PageTags && !ThreadLocalLink && !FirstGraveyard);
	AddVectoredExceptionHandler(1,HandlePageFault);
	MemoryTags  = (nat8 *)VirtualAlloc(0,sizeof(nat8 )*AddressEnd/8       ,MEM_RESERVE|MEM_TOP_DOWN,PAGE_NOACCESS);
	PageTags    = (nat64*)VirtualAlloc(0,sizeof(nat64)*AddressEnd/PageSize,MEM_RESERVE|MEM_TOP_DOWN,PAGE_NOACCESS);
	VERSE_ASSERT(MemoryTags && PageTags);
	FirstGraveyard=InitThread();
	ThreadLocalLink=InitThread();
}
kernel::~kernel() {
}
kernel::kernel_thread::kernel_thread() {
	if(!ThreadLocalLink) // If not the primary thread, whose Link is already initialized above.
		ThreadLocalLink=InitThread();
}
kernel::kernel_thread::~kernel_thread() {
	VERSE_ASSERT(ThreadLocalLink);

	// Note ending as soon as it's not locked (GC has short-duration locks to ensure ThreadLocalHazard availability).
	VERSE_ASSERT(ThreadLocalHazard==0);
	while(CompareExchangeAtomic(&ThreadLocalLink->ThreadStatus,ThreadStateEnding,ThreadStateNormal)!=ThreadStateNormal);

	// Must be done by last destructor for safety.
	if(ThreadLocalStartedByRunThread)
		AddAtomic(&kernel::RunningThreadCount,-1);

	// Note thread is ended from the point of view of garbage collection.
	// Because this is the last destructor in the first thread_local variable ahead of all managed types,
	// we're guaranteed the thread holds no managed resources.
	AddAtomic(&CollectorThreads,-1);
}
void kernel::RegisterStatic(managed* Managed,nat Size) {
	// SEEING AN ACCESS VIOLATION HERE AT STARTUP?
	// It's intended. Allow it in Visual C++ Exception Settings.
	for(auto PT=PageTagsOf(Managed),PT1=PageTagsOf((nat8*)Managed+Size-1); PT<=PT1; PT++)
		*PT=PageTagStatic|IsPageSmall(32);
	auto MTP=MemoryTagsOf(Managed);
	MTP[0]=MemoryTagScanned; 
}
void kernel::InitStatic(managed* Managed) {
	Managed->ManagedForward=TopContext.Payload|PayloadContext;
}
void kernel::FixStatic(const box<managed>& M) {
	const_cast<nat&>(M->ManagedForward)=TopContext.Payload|PayloadContext;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Low level payload management.

VERSE_FORCE_INLINE nat kernel::LockPayload(const volatile nat* PayloadP) {
#if 0
	return *PayloadP;
#else
	for(;;) {
		nat Payload       = *PayloadP; Preempt();
		ThreadLocalHazard = Payload;   Preempt();
		if(Payload==*PayloadP)
			return Payload;
	}
#endif
}
VERSE_FORCE_INLINE nat kernel::LockPayloadCopy(const volatile nat* Payload) {
	auto P=LockPayload(Payload);
	return PayloadIsCopyable(P)? P: nat(Payload)+PayloadInterior;
}
VERSE_NO_INLINE void kernel::ExposePayload(nat P) {
	Preempt();
	volatile nat8* ReachedP=MemoryTagsOf(AllocationBase(P&PayloadPointerMask));
	VERSE_ASSERT(*ReachedP&MemoryTagManagedStart);
	if(*ReachedP==MemoryTagUnreached) {
       	Preempt();
		*ReachedP=MemoryTagReached;
	}
	Preempt();
}
VERSE_FORCE_INLINE void kernel::StorePayload(volatile nat* Payload,nat P) {
	*Payload=P; Preempt();
	if(PayloadIsManaged(P)) {
		*MemoryTagsOf(Payload) = PayloadTagManaged;
		if(IsCollecting)
			ExposePayload(P);
	}
}
VERSE_FORCE_INLINE void kernel::StorePayload(volatile nat* Payload,nat P,nat8 MT) {
	*Payload=P; Preempt();
	*MemoryTagsOf(Payload)=MT|ToPayloadTag(P);
	if(IsCollecting && PayloadIsManaged(P))
		ExposePayload(P);
}
void kernel::UnlockPayload() {
	ThreadLocalHazard=0;
}
nat kernel::AdvancePinned(const pin<future<>>& Source) {
	auto P=Source.Payload;
	while(PayloadIsInterior(P)) { 
		P=LockPayload(reinterpret_cast<volatile nat*>(P-PayloadInterior));
		if(!PayloadIsCopyable(P))
			return UnlockPayload(),P>>PayloadNonValueShift;
		StorePayload(&Source.Payload,P);
		UnlockPayload();
	}
	return 0;
}
void kernel::non_value_accessor::Advance() {
	NonValueType=0;
	auto P=Source.Payload;
	while(PayloadIsInterior(P)) {
		P=LockPayload(reinterpret_cast<volatile nat*>(P-PayloadInterior));
		if(!PayloadIsCopyable(P)) {
			NonValueType = P>>PayloadNonValueShift;
			StorePayload(&Target.Payload,PayloadSignExtend(P));
			break;
		}
		StorePayload(&Source.Payload,P);
	}
	UnlockPayload();
}
kernel::non_value_accessor::non_value_accessor(const future<>& Source0): Source(Source0), Target(0) {
	Advance();
}
bool kernel::non_value_accessor::CompareExchangeInterior(const future<>& NewValue,nat NewNonvalueType) {
	auto PP = reinterpret_cast<volatile nat*>(Source.Payload-PayloadInterior);
	//!!VERSE_ASSERT(*MemoryTagsOf(PP)&PayloadTagMutable); // found breaking Cast on 5/5/2020, v5157
	VERSE_ENSURE(NonValueType);
	nat BaseP = LockPayloadCopy(&NewValue.Payload);
	nat NewP  = !NewNonvalueType? BaseP: BaseP         &PayloadValueMask | NewNonvalueType<<PayloadNonValueShift | PayloadNonValueFlag;
	nat OldP  =                          Target.Payload&PayloadValueMask | NonValueType   <<PayloadNonValueShift | PayloadNonValueFlag;
	if(CompareExchangeAtomic(PP,NewP,OldP)==OldP) {
		Preempt();
		auto MTP   = MemoryTagsOf(PP);
		nat8 NewMT = *MTP | ToPayloadTag(NewP);
		*MTP       = NewMT;
		Preempt();
		if(IsCollecting && PayloadIsManaged(NewP))
			ExposePayload(NewP);
		return UnlockPayload(),true;
	}
	return Advance(),false;
}
bool kernel::non_value_accessor::CompareExchangeVar(const future<>& NewValue) {
	VERSE_ASSERT(NonValueType==NonValueGlobal);
	return CompareExchangeInterior(NewValue,NonValueGlobal);
}
void kernel::non_value_accessor::ReadVar() const {
	VERSE_ASSERT(NonValueType==NonValueGlobal);
}
future<> kernel::non_value_accessor::Waiter() const {
	return IsWaitingFuture(NonValueType)? Target: 0;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Futures.

struct managed_future: managed {
	void        OnCopy      (managed* Target) const override {new(Target)managed_future(*this);}
	const char* OnNativeName(               ) const override {return "managed_future";}
};
future<>::~future() {
	*MemoryTagsOf(&Payload)=0;
#if 1
	while(IsCollecting==1); // Prevent caller freeing memory while gc is scanning user heap.
#endif
}
VERSE_FORCE_INLINE future<>::future(const pin<future<>>& A) {
	kernel::StorePayload(&Payload,A.Payload); // Guaranteed copyable.
}
VERSE_FORCE_INLINE future<>::future(const future& A) {
	nat P=kernel::LockPayloadCopy(&A.Payload);
	kernel::StorePayload(&Payload,P);
	kernel::UnlockPayload();
}
VERSE_FORCE_INLINE future<>::future(construct_value_copy,const future<>& A) {
	auto P=kernel::LockPayload(&A.Payload); // Guaranteed copyable.
	kernel::StorePayload(&Payload,P);
	kernel::UnlockPayload();
}
future<>::future() {
	if(!IsPageManaged(*PageTagsOf(this))) {
		auto     Offset  = nat(&((managed*)nullptr)->ManagedForward)+PayloadInterior;
		managed* Managed = kernel::Allocate(&Payload,sizeof(managed),Offset,Thread.Payload|PayloadContext,PayloadTagManaged|PayloadTagMutable|PayloadTagUninitialized);
		new(Managed)managed_future;
		*MemoryTagsOf(&Managed->ManagedForward)=PayloadTagManaged|PayloadTagMutable;
	}
	else {
		// Inline futures don't store context but utilize base allocation's context.
		// This depends vitally on context::Run preventing copy elision across context boundaries!
		kernel::StorePayload(&Payload,PayloadContext,PayloadTagMutable);
	}
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Transactional variables.

struct Verse::managed_ptr: managed {
	nat Contents;
	managed_ptr(const managed_ptr& Source): managed(Source) {
		auto P=kernel::LockPayloadCopy(&Source.Contents);
		kernel::StorePayload(&Contents,P);
		kernel::UnlockPayload();
	}
	managed_ptr(nat P,nat8 MT) {
		kernel::StorePayload(&Contents,P,MT);
	}
	managed_ptr(const managed_ptr& Source,nat P,nat8 MT): managed(Source) {
		kernel::StorePayload(&Contents,P,MT);
	}
	void OnCopy(managed* Target) const override {
		auto P=kernel::LockPayload(&Contents);
		new(Target)managed_ptr(*this,P,PayloadTagMutable|ToPayloadTag(P));
		kernel::UnlockPayload();
	}
	void OnDestructor() override {
		// This is called because managed_ptr is allocated without a wrapper.
		reinterpret_cast<const future<>&>(Contents).~future<>();
	}
	const char* OnNativeName() const override {
		return "managed_ptr";
	}
};
template<> struct expose<managed_ptr>: default_expose<managed_ptr> {
	path ExposeStaticSignature() {
		return "/Verse.org/pointer"_VP;
	}
};
any::any(construct_new_var,const pin<future<>>& A): future<>(construct_no_init{}) {
	nat  P  = PayloadNewVar|A.Payload&PayloadValueMask;
	nat8 MT = PayloadTagMutable|ToPayloadTag(P);
	if(!IsPageManaged(*PageTagsOf(this))) {
		auto  Offset  = nat(&((managed_ptr*)nullptr)->Contents)+PayloadInterior;
		auto* Managed = (managed_ptr*)kernel::Allocate(&Payload,sizeof(managed_ptr),Offset,Thread.Payload|PayloadContext,PayloadTagManaged|PayloadTagMutable|PayloadTagUninitialized);
		new(Managed)managed_ptr(P,MT);
		*MemoryTagsOf(&Managed->ManagedForward)=PayloadTagManaged|PayloadTagMutable|PayloadTagHasDestructor;
	}
	else kernel::StorePayload(&Payload,P,MT);
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Forking.

struct kernel_cloner: managed::cloner {
	var<map<managed*,nat>> Clones;
	var<bag<managed*>>     Cloned;
	kernel_cloner(const pin<iterate>& Iterate0): managed::cloner(Iterate0) {}
	void Clone(future<>& A0) {
		pin<future<>>& A1=reinterpret_cast<pin<future<>>&>(A0); // Because InsideManaged does AdvancePinned.
		if(managed* SourceMan=InsideManaged(A1)) {
			if(SourceIterate<=SourceMan->Context() || SourceMan==kernel::CastManaged(SourceIterate)) {
				auto Offset = A1.Payload-nat(SourceMan);
				if(auto MO=Clones.Get(SourceMan)) {
					nat Payload1=MO.Coerce()+Offset;
					if(!PayloadIsInterior(Payload1) || Payload1-PayloadInterior!=nat(&A0)) // Copy with offset.
						kernel::StorePayload(&A0.Payload,Payload1);
					else // Translate copy-constructed SourceMan interior future<t>/var<t> to same in TargetMan.
						kernel::StorePayload(&A0.Payload,*reinterpret_cast<volatile nat*>(A1.Payload-PayloadInterior),PayloadTagMutable);
				}
				else {
					auto Size      = AllocationSize(SourceMan);
					auto TargetMan = kernel::Allocate(&A0.Payload,Size,Offset,SourceMan->ManagedForward,PayloadTagManaged|PayloadTagMutable|PayloadTagUninitialized);
					VERSE_ENSURE(!PayloadIsInterior(A0.Payload) || A0.Payload-PayloadInterior!=nat(&A0)); // No interior handling necessary here because it's always handled in advance.
					Clones.Set(SourceMan,nat(TargetMan));
					Cloned.Set(TargetMan);
					SourceMan->OnCopy(TargetMan);
					for(auto AP=(future<>*)TargetMan+1,EndP=(future<>*)(nat(TargetMan)+Size); AP<EndP; AP++)
						if(auto MT=*MemoryTagsOf(AP); MT&PayloadTagManaged) // Can traverse more efficiently.
							Clone(*AP);
					kernel::AllocateInitialized(TargetMan,MemoryTagsOf(SourceMan)[1]&PayloadTagHasDestructor);
				}
			}
		}
	}
	void Fixup(managed* TargetManaged) {
		if(Cloned.Remove(TargetManaged))
			TargetManaged->OnCloned(*this);
	}
	void Fixup(const future<>& A0) override {
		if(const pin<future<>>& A=reinterpret_cast<const pin<future<>>&>(A0); managed* M=InsideManaged(A))
			Fixup(M);
	}
};
tuple<iterate,step> kernel::CloneContext(const iterate& Iterate,const step& Step) {
	VERSE_ENSURE(!Iterate->IsReady && !Iterate->IsCommitted);
	auto Cloner     = kernel_cloner(Iterate); Cloner.Clone(Cloner.TargetIterate);
	auto TargetStep = Step;                   Cloner.Clone(TargetStep);
	VERSE_ENSURE(!Cloner.TargetIterate->IsReady);
	return Cloner.TargetIterate->Run([&]{
		for(auto[TargetManaged,_]:Cloner.Cloned)
			Cloner.Fixup(TargetManaged);
		return tuple{Cloner.TargetIterate,TargetStep};
	});
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Garbage collector.

nat kernel::ReachedPayload(volatile nat* OuterPP,nat OuterP) {
	// Don't optimize with mtp_, as that won't be feasible When we reduce heap-allocated futures and vars to 8B.
	if(PayloadIsInterior(OuterP)) { // Even if wrapped in special.
		auto IPP = reinterpret_cast<volatile nat*>(OuterP&PayloadPointerMask);
		auto IP  = *IPP;
		if(PayloadIsCopyable(IP)) {
			nat InnerP = ReachedPayload(IPP,IP);
			nat NewP   = (OuterP&PayloadTransactionMask)|InnerP;
			VERSE_ASSERT(PayloadIsCopyable(InnerP));
			*OuterPP   = NewP;
			return NewP;
		}
	}
	return OuterP;
}
nat8 kernel::ReachedPayloadScan(volatile nat* OldPP) {
	if(nat NewP=ReachedPayload(OldPP,*OldPP); auto Managed=InsideManaged(reinterpret_cast<const pin<future<>>&>(NewP)))
		return Scan(Managed,MemoryTagsOf(Managed)),1;
	return 0;
}
void kernel::Scan(managed* Managed,volatile nat8* ManMemoryTags) {
	VERSE_ASSERT(ManMemoryTags[0]&MemoryTagManagedStart);
	if(ManMemoryTags[0]!=MemoryTagScanned) {
		VERSE_ASSERT(IsPageManaged(*PageTagsOf(Managed)));
		ManMemoryTags[0]=MemoryTagScanned;
		ReachedCount++;
		nat8 Status=ManMemoryTags[1];
		// Optimize: not if it contains nothing managed|mutable beyond 1st field, to optimize futher scans.
		if(Status&PayloadTagManaged)
			ReachedPayloadScan(&Managed->ManagedForward);
		for(auto MTP=ManMemoryTags+1,ManMemoryTagsEnd=ManMemoryTags+AllocationSize(Managed)/8; MTP<ManMemoryTagsEnd; MTP++) {
			auto MT=*MTP;
			VERSE_ASSERT(!(MT&MemoryTagManagedStart));
			if(MT&PayloadTagManaged)
				ReachedPayloadScan((nat*)Managed+(MTP-ManMemoryTags));
		}
	}
}
static nat ToBitMask(volatile nat8* bs,nat8 shift) {
	constexpr nat m=0x0101010101010101;
	constexpr nat c=0x0102040810204080;
	nat Result=0;
	for(nat i=64-8; i+8; i-=8)
		Result=(Result<<8)+((*(volatile nat*)(bs+i)>>shift&m)*c>>56);
	return Result;
}
VERSE_NO_INLINE void Verse::Collect(bool Checked,bool Verbose) {
	nat32 Index8;
	if(Checked) { 
		VERSE_ASSERT(kernel::RunningThreadCount==0);
		Collect(0,0); // Clean up MemoryTagReached left over following previous IsCollecting=0.
	}
	if(Verbose) Print("Collect:");
	RootCount=ReachedCount=DeadCount=0,UserPagesActive=0,UserPagesCount=0;
	CollectionsCount++;

	// Remove ended threads from thread list.
	for(thread_link **thlp=&FirstThread,*ThreadLink; (ThreadLink=*thlp);) {
		if(ThreadLink->ThreadStatus!=ThreadStateEnding) {
			thlp=&ThreadLink->Next;
		}
		else {
			// Remove ended thread from thread list, or defer to later iteration if racing.
			if(!LinkAtomic(thlp,ThreadLink->Next,&ThreadLink))
				continue;

			// Move ended thread's pages to graveyard, which is last in the thread list.
            // Can't do this in source thread, because of race where pages are exposed there and in graveyard.
			VERSE_ASSERT(FirstGraveyard);
			for(nat Bin=0; Bin<BinCount; Bin++)
				if(auto& thread_first=ThreadLink->ManagedPages[Bin]) {
					auto& graveyard_first=FirstGraveyard->ManagedPages[Bin];
					auto* thread_last=thread_first;
					while(thread_last->Next)
						thread_last=thread_last->Next;
					thread_last->Next = graveyard_first;
					graveyard_first   = thread_first;
					thread_first      = nullptr; // Prevent duplicate traversal in GC.
				}
            ThreadLink->NextKill=FirstKill;
			FirstKill=ThreadLink;
		}
	}

	// Before we take snapshot, ensure any managed paylods we subsequently managed-expose are marked MemoryTagReached.
	// We do this because, from here onward, managed-detection of newly managed-exposed payloads is timing-dependent.
	// In particular, we mustn't miss managed-exposure of pre-snapshot reference payloads within new post-snapshot objects.
	IsCollecting=1;
	FenceGlobal(); // Ensure that IsCollecting is visible for any ExposePayload calls occuring after we Start scanning.

	// Conservatively Scan unmanaged pages and mark MemoryTagReached to indicate they are roots,
	// keeping thread list pinned to avoid page fault cascade.
	for(tracked_region_link **trackp=&FirstUnmanagedRegion,*Track; (Track=*trackp);) {
		if(!Track->Duplicate) {
			UserPagesCount++;
			bool Keep = 0;
			auto RegionStart     = (nat*)Track->Address,       RegionStop     = RegionStart     + PageSize,   RP  = RegionStart;
			auto MemoryTagsStart = MemoryTagsOf(RegionStart),/*MemoryTagsStop = MemoryTagsStart + PageSize,*/ MTP = MemoryTagsStart;
			for(; RP<RegionStop; RP+=64,MTP+=64)
				if(!IsPageManaged(*PageTagsOf(RP)))
					if(nat Bits8=ToBitMask(MTP,MemoryTagManagedShift)) {
						for(; _BitScanForward64(&Index8,Bits8); Bits8&=~(1LL<<Index8)) {
							nat P=RP[Index8];
							// User memory may mutate or be freed, so unless we make ~future<> block,
							// we must guard this by an exception handler and read Payload speculatively.
							// NOTE: This assert was disabled a while ago, not sure why. Reenabled it 2020-04-25.
							VERSE_ASSERT(PayloadIsCopyable(P)); // Inline futures and vars aren't allowed in user memory.
							if(kernel::PayloadIsManaged(P)) // Check due to nonatomic Payload & MemoryTags Update.
								if(auto m=kernel::AllocationBaseSpeculative(P))
									if(auto reachedp=MemoryTagsOf(m); *reachedp==MemoryTagUnreached)
										*reachedp=MemoryTagReached,RootCount++;
						}
						Keep=1;
					}
			UserPagesActive+=Keep;
			if(!Keep) {
				// Deleting tracked page involves write-protecting all MemoryTags, allowing any fault to immediately resurrect,
				// reading to MemoryTags and resurrecting if nonzero, locking (region of) MemoryTags to prevent decommit racing resurrect.
			}
			trackp=&Track->Next;
		}
		else {
			// Delete race-safe duplicate Link.
			if(!LinkAtomic(trackp,Track->Next,&Track))
				continue;
			delete Track;
		}
	}
	IsCollecting=3;

	// Our roots are all objects that were referenced by payloads in unmanaged memory at the instant of gc snapshot.
	// However, we can't identify those objects exactly.
	// So, we conservatively identify all objects in the snapshot that either are referenced by payloads in unmanaged
	// memory now, or were copied since gc snapshot was taken (as indicated by MemoryTagReached).
	for(nat pass=0; pass<2; pass++) {
		// We have two passes because, when we allow mutation of payload references in managed memory other than
		// write-once (constructor, future-Resolve), e.g. through suspension operator-> or var<t>, the current algorithm
		// may miss them due to ping-ponging (mutator overwrites before collector reads) occuring after first pass has
		// looked for MemoryTagReached.
		FenceAtomic();
		for(thread_link* ThreadLink=FirstThread; ThreadLink; ThreadLink=ThreadLink->Next)
			for(nat Bin=0; Bin<BinCount; Bin++)
				for(page_link* Link=ThreadLink->ManagedPages[Bin]; Link; Link=Link->Next)
					for(auto MTP0=MemoryTagsOf(Link->Start),MTP1=MemoryTagsOf(Link->Stop); MTP0<MTP1; MTP0+=64)
						for(nat Bits8=ToBitMask(MTP0,MemoryTagStartShift); _BitScanForward64(&Index8,Bits8); Bits8&=~(1LL<<Index8))
							if(auto MTP=MTP0+Index8; MTP[0]==MemoryTagReached)
								kernel::Scan((managed*)((MTP-MemoryTags)*8),MTP);
	}

	// No more forwarding occurs after this point.

	// Catch all reference payloads that are being loaded via hazard pointer.
    // Must do this after all forwarding is done, and before freeing begins.
    // The only new objects this surfaces are those that were visible previously, but forwarded thus not MemoryTagReached.
	// This works as-is because forwarded objects currently contain no reference payloads, thus needn't be scanned.
    //		When we support GC-forwarding beyond futures, hazards will require scanning and forwarding,
    //		but we can assert that after first-order forwarding they won't reach yet-unreached objects.
    FenceAtomic();
	nat ThreadCount=0, HazardCount=0;
	for(thread_link* ThreadLink=FirstThread; ThreadLink; ThreadLink=ThreadLink->Next) // Consider all threads created prior to end of forwarding.
		if(CompareExchangeAtomic(&ThreadLink->ThreadStatus,ThreadStateLocked,ThreadStateNormal)==ThreadStateNormal) {
			if(nat P0=*ThreadLink->PayloadReadHazard; (P0!=0)&kernel::PayloadIsManaged(P0))
				if(auto Managed=kernel::AllocationBaseSpeculative(P0))
					if(auto reachedp=MemoryTagsOf(Managed); reachedp[0]==MemoryTagUnreached)
						reachedp[0]=MemoryTagReached, HazardCount++;
			ThreadLink->ThreadStatus=ThreadStateNormal;
			ThreadCount++;
		}

	// Update epoch. Any pointers loaded in prior epoch may be invalid in Next epoch.
	// From here on, new MemoryTagReached may surface further managed objects only if they're forwarded.
	FenceGlobal();

	// Traverse all objects and check whether reached.
	// Call destructors, free pages, compact page list, reset reach flag.
	for(thread_link* ThreadLink=FirstThread; ThreadLink; ThreadLink=ThreadLink->Next)
		for(nat Bin=0; Bin<BinCount; Bin++) {
			page_link* PriorPage=0;
			for(page_link *Link    = ThreadLink->ManagedPages[Bin],*Next; Link; PriorPage=Link,Link=Next) {
				Next               = Link->Next;
				bool KeepPages     = !PriorPage;
				auto AllocBeginMTP = MemoryTagsOf(Link->Start);
				auto AllocEndMTP   = MemoryTagsOf(Link->Stop );
				for(auto MTP64=AllocBeginMTP; MTP64<AllocEndMTP; MTP64+=64)
					for(nat Bits8=ToBitMask(MTP64,MemoryTagStartShift); _BitScanForward64(&Index8,Bits8); Bits8&=~(1LL<<Index8)) {
						auto ManStartMTP    = MTP64+Index8;
						nat8 MTS            = ManStartMTP[1];
						nat8 MTM            = ManStartMTP[0];
						bool KeepAllocation = (MTM!=MemoryTagUnreached) | (MTS&PayloadTagUninitialized);
						ManStartMTP[0]      = MemoryTagUnreached;
						KeepPages          |= KeepAllocation;
						if(!KeepAllocation) {
							ManStartMTP[0]=0;
							ManStartMTP[1]=0;
							DeadCount++;
							auto Managed=(managed*)((ManStartMTP-MemoryTags)*8);
							if(MTS&PayloadTagHasDestructor)
								Managed->OnDestructor();
#ifndef NDEBUG
							auto PTP = PageTagsOf(Link->Start);
							for(auto MTP=ManStartMTP,EndManMTP=MTP+AllocationSize(Link->Start,(nat*)PTP)/8; MTP<EndManMTP; MTP++)
								if(auto MT=*MTP)
									VERSE_ERR("MemoryTags not cleared after dtor: type=",Managed->OnNativeName()," Offset=",MTP-ManStartMTP," has-dtor=",MTS&PayloadTagHasDestructor);
#endif
						}
					}
				if(!KeepPages) {
					VERSE_ASSERT(ClearMemoryTags(Link));
					auto BeginPT = nat(Link->Start)/PageSize;
					auto EndPT   = nat(Link->Stop )/PageSize;
					AddAtomic(&PageFreed,EndPT-BeginPT);
					for(nat PT=BeginPT; PT<EndPT; PT++) {
						VERSE_ASSERT(IsPageManaged(PageTags[PT]));
						PageTags[PT]=0;
					}
					PriorPage->Next=Next;
					if(EndPT-BeginPT==1) {
						Link->Next=UnsizedSmallPages;
						while(!LinkAtomic(&UnsizedSmallPages,Link,&Link->Next));
					}
					else VERSE_ENSURE(VirtualFree(Link->Start,0,MEM_DECOMMIT));
					Link=PriorPage;
				}
			}
		}
	IsCollecting=0; // Must occur after free-loop since we no longer snapshot page-top.
	FenceAtomic();
	for(thread_link* Next; FirstKill && (Next=FirstKill->NextKill,true); FirstKill=Next)
		delete FirstKill;
	if(Verbose)
		Print("    snapshot roots=",RootCount," reached=",ReachedCount," dead=",DeadCount," CollectionsCount=",CollectionsCount);
	if(Checked) {
		nat PageHeld=0;
		for(thread_link* ThreadLink=FirstThread; ThreadLink; ThreadLink=ThreadLink->Next)
			for(nat Bin=0; Bin<BinCount; Bin++)
				for(auto Link=ThreadLink->ManagedPages[Bin]; Link; Link=Link->Next)
					PageHeld+=(Link->Stop-Link->Start)/PageSize;
		nat PageLeaked=PageAllocated-PageFreed-PageHeld;
		Print("    page taken=",PageTaken," allocated=",PageAllocated," freed=",PageFreed," held=",PageHeld," leaked=",PageLeaked);
		Print("    page User=",UserPagesActive,"/",UserPagesCount," SurpriseMemoryTags=",MemoryTagsPages," SurprisePageTags=",PageTagsPages);
        Print("    threads Tracked=",ThreadCount," CollectorThreads=",CollectorThreads," RunningThreadCount=",kernel::RunningThreadCount," Hazards=",HazardCount);
		if(Thread->HasSuspensions())
			Print(ToString(Coerce<suspension_managed>(Thread->ContextResumeStep)->OnDescribe()));
		VERSE_ENSURE(!Thread->HasSuspensions());     // Expect no suspensions When final GC called.
		VERSE_ENSURE(HazardCount==0);                // Expect no hazards.
		VERSE_ENSURE(RootCount==0);                  // Expect no allocations.
		VERSE_ENSURE(PageLeaked==0);                 // Expected all pages accounted for by tracked threads.
		VERSE_ENSURE(kernel::RunningThreadCount==0); // Expect no RunThread threads running.
		//VERSE_ENSURE(CollectorThreads==2);         // Expect this thread, graveyard.
		//VERSE_ENSURE(ThreadCount==2);              // Yet debugger seems to attach random ntdll threads.
	}
}
void Verse::SettleCollector() {
	nat n=CollectionsCount;
	while(CollectionsCount<n+2);
}
bool Verse::IsMultithreading() {
	VERSE_ASSERT(CollectorThreads>=2);
	return kernel::RunningThreadCount>0;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Garbage collection sensitive operations.

// Must set ManagedForward this way because aggregate-initialization of default-constructor-free subclasses of managed
// zero-initialize ManagedForward.
static thread_local nat ConstructManagedForward;
managed::managed() {
	ManagedForward=ConstructManagedForward;
	//!!fix new debug assert since reworking static boxing: VERSE_ASSERT(*PageTagsOf(this));
}
managed::managed(const managed& Source): ManagedForward(Source.ManagedForward) {
	VERSE_ASSERT(*PageTagsOf(this));
}

// Allocation.
managed* kernel::Allocate(volatile nat* Payload,nat Size0,nat Offset,nat ManagedForward,nat8 MT) {
	VERSE_ASSERT(Size0>=16);
	VERSE_ASSERT(MT&PayloadTagUninitialized);

	// Allocate large or small.
	managed* Managed;
	if(Size0<=PageSize) {
		nat Bin=CeilLog2(Size0), Size=1LL<<Bin;
		VERSE_ASSERT(!(nat(ThreadLocalTop[Bin])&(Size-1)));
		if(!(nat(ThreadLocalTop[Bin])&(PageSize-1)))
			AllocateSizedPage(Bin,Size);
		Managed              = (managed*)ThreadLocalTop[Bin];
		ThreadLocalTop[Bin] += Size;
	}
	else Managed=(managed*)AllocateMultiplePages(Size0);

	// Initialize and safely make object live.
	ConstructManagedForward = ManagedForward;
    auto* MTP               = MemoryTagsOf(Managed);
	VERSE_ASSERT(MTP[0]==0 && MTP[1]==0);
	MTP[1]                  = MT;                  Preempt(); // Block copying with mtm_uninited between becoming GC-visible and constructor completing.
    MTP[0]                  = MemoryTagUnreached;  Preempt(); // Makes it GC-visible.
	*Payload                = nat(Managed)+Offset; Preempt(); // Must expose after MTP[0]&MemoryTagManagedStart.
	*MemoryTagsOf(Payload)  = PayloadTagManaged;
	Preempt();
    if(IsCollecting) {
        Preempt();
		MTP[0]=MemoryTagReached; 
		Preempt();
	}
	return Managed;
}
managed* kernel::Allocate(volatile nat* TargetP,nat Size,nat Offset,const pin<context>& TargetContext) {
	return Allocate(TargetP,Size,Offset,TargetContext.Payload|PayloadContext,PayloadTagManaged|PayloadTagMutable|PayloadTagUninitialized);
}
void kernel::AllocateInitialized(managed* Managed,bool ManagedDestructor) {
	*MemoryTagsOf(&Managed->ManagedForward) = PayloadTagManaged | PayloadTagMutable | PayloadTagHasDestructor*ManagedDestructor;
}
void kernel::AllocateAbandoned(void* MemorySomewhereInsideManaged) {
	// We allocated an option but its initialization failed, for example because we were casting
	// from a source array into a target array and the cast failed midway through. When this happens,
	// cleanup is the caller's responsibility, and this function must prevent AllocateInitialized
	// from exposing PayloadTagHasDestructor, so garbage collector never calls this object's destructor.
	//!!todo
}
managed* InsideManaged(const pin<future<>>& A) {
	kernel::AdvancePinned(A);
	auto P=A.Payload;
	return kernel::PayloadIsManaged(P)? kernel::AllocationBase(P&PayloadPointerMask): nullptr;
}
managed* kernel::CastManaged(const pin<future<>>& A) {
	if(auto P=A.Payload; kernel::PayloadIsBox(P))
		return kernel::AllocationBase(P&PayloadPointerMask);
	return nullptr;
}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Move out of kernel?

bool Verse::IsFlexible(const pin<future<>>& Target) {
	if(auto NonValueType=kernel::AdvancePinned(Target); !kernel::IsResolvedFuture(NonValueType))
		if(auto Managed=InsideManaged(Target))
			return Managed->Context()->Visibility()==Thread->Visibility();
	return false;
}
pin<context> managed::Context() const {
	kernel::non_value_accessor Work(reinterpret_cast<const future<>&>(ManagedForward)); // Advances because committed contexts are forwarded.
	if(auto NonValueType=Work.NonValueType; NonValueType==NonValueContext) {
		if(!Work.Target.Payload)
			return TopContext;//!!Apparently initialized at startup.
		return reinterpret_cast<const pin<context>&>(Work.Target);
	}
	else if(NonValueType==NonValueSuspension) {
		VERSE_ENSURE(reinterpret_cast<const box<when_future_suspension>&>(Work.Target)->FutureContext.Payload);
		return reinterpret_cast<const box<when_future_suspension>&>(Work.Target)->FutureContext;
	}
	else if(NonValueType==NonValueValue) // Needed for chain of managed_future.
		return Work.Source.Context();
	else
		VERSE_UNEXPECTED;
}
pin<context> future<>::Context() const {
	if(auto Managed=InsideManaged(*this))
		return Managed->Context();
	else return TopContext;
}
pin<context> kernel::ContextOf(const managed* M) {
	VERSE_ENSURE((M->ManagedForward>>PayloadNonValueShift)==NonValueContext);
	pin<context> Result(construct_no_init{});
	StorePayload(&Result.Payload,PayloadSignExtend(M->ManagedForward));
	return Result;
}
bool kernel::IsSameBox(const pin<future<>>& A,const pin<future<>>& B) {
	if(auto AP=A.Payload,BP=B.Payload; AP==BP) // Quick check for exact payloads.
		return true;
	else if(kernel::PayloadIsBox(AP)&&kernel::PayloadIsBox(BP)) // If managed objects, check base to support polymorphism.
		return kernel::AllocationBase(AP)==kernel::AllocationBase(BP);
	else if(PayloadIsInterior(AP)||PayloadIsInterior(BP)) { // Check a consistent view of forwardable interiors.
		auto AQ=pin(A); kernel::AdvancePinned(AQ);
		auto BQ=pin(B); kernel::AdvancePinned(BQ);
		return AQ.Payload==BQ.Payload;
	}
	else return false;
}
bool kernel::AddSuspension(kernel::non_value_accessor& Work,const pin<box<when_future_suspension>>& NewSus) {
	for(;;) {
		VERSE_ASSERT(IsSameBox(NewSus->FutureNextSuspension,Work.Waiter()));
		if(Work.CompareExchangeInterior(NewSus,NonValueSuspension))
			return false;
		Work.Advance();
		if(!Work.NonValueType)
			return true;
		NewSus->SuspensionFuture     = Work.Source;
		NewSus->FutureNextSuspension = Work.Waiter();
	}
}
VERSE_NO_INLINE bool future<>::ResolveBatch(future<> Target,bool ResolveLocal) const {
	kernel::non_value_accessor Self(*this);
	for(kernel::non_value_accessor Latest(Target);;) {

		// Atomically update Self exactly once to resolve it to Latest.
		if(!kernel::IsResolvedFuture(Self.NonValueType)) {
			if(ResolveLocal && !(Thread->Visibility()<=Self.Source.Context()->Visibility()))
				VERSE_ERR("resolved out-of-context: ",Thread->Visibility()->Depth," : ",Self.Source.Context()->Visibility()->Depth);
			if(kernel::IsSameBox(Latest.Source,Self.Source)) // Resolving a future to itself is a no-op.
				return false;
			if(Self.CompareExchangeInterior(Latest.Source,0)) {

				// Successfully resolved Self. If nothing is waiting, exit without resume.
				if(!kernel::IsWaitingFuture(Self.NonValueType))
					return false;

				// While Latest is still unresolved, copy all of Self's waiting suspensions to it.
				while(!kernel::IsResolvedFuture(Latest.NonValueType)) {
					auto& MoveSuspension=reinterpret_cast<pin<box<when_future_suspension>>&>(Self.Target);
					pin<future<>> FutureNextSuspension(MoveSuspension->FutureNextSuspension);
					MoveSuspension->FutureNextSuspension=Latest.Waiter();
					if(Latest.CompareExchangeInterior(Self.Target,NonValueSuspension)) {
						// Successfully moved suspension, keeping Latest.nvflag.
						// If nothing more is waiting, exit without resume as target is unresolved.
						if(IsZero(FutureNextSuspension))
							return false;
						Self.Target=FutureNextSuspension;
					}
					Latest.Advance();
				}

				// We've resolved Target, so release waiting suspensions.
				for(;;) {
					auto Sus=reinterpret_cast<pin<box<when_future_suspension>>&>(Self.Target);
					Sus->ReadySuspensionBatch();
					if(IsZero(Sus->FutureNextSuspension))
						return true;
					Self.Target=pin<future<>>(Sus->FutureNextSuspension);
				}
			}
			VERSE_ERR("coverage ResolveBatch racing WhenResolved");
			continue;
		}
		VERSE_ERR("future already resolved");
	}
}
