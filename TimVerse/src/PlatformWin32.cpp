//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Platform porting layer.

#if _MSC_VER
#include "Terse.h"
#pragma warning(disable: 4100)
using namespace Verse;


//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Platform implementation.

// Constants.
static const nat32 FILE_TYPE_CHAR=2;

// Intrinsics.
extern "C" nat   __rdtsc();
extern "C" nat   _umul128(nat a,nat b,nat* r_hi);
extern "C" int64 _mul128(int64 a,int64 b,int64* r_hi);
extern "C" int64 _InterlockedExchangeAdd64(int64 volatile*,int64);
extern "C" char  _InterlockedOr8(char volatile*,char);
extern "C" char  _InterlockedAnd8(char volatile*,char);
extern "C" int64 _InterlockedOr64(int64 volatile*,int64);
extern "C" int64 _InterlockedAnd64(int64 volatile*,int64);
extern "C" char  _InterlockedCompareExchange8(char volatile* Target,char x,char cmp);
extern "C" int64 _InterlockedCompareExchange64(int64 volatile* Target,int64 x,int64 cmp);
extern "C" nat8  _InterlockedCompareExchange128(int64 volatile* dstlohi,int64 xhi,int64 xlo,int64* result);
extern "C" nat8  _interlockedbittestandset64(volatile long long*,long long);
extern "C" nat8  _interlockedbittestandreset64(volatile long long*,long long);
extern "C" char  _InterlockedExchange8(char volatile* Target,char new_value);
extern "C" int64 _InterlockedExchange64(int64 volatile* Target,int64 new_value);
extern "C" unsigned char _BitScanReverse64(unsigned long* Index,unsigned __int64 Mask);

// Math intrinsics.
nat Verse::CeilLog2(nat a) {
	unsigned long Index;
	auto Found=_BitScanReverse64(&Index,a-1);
	return Index+Found;
}
#if __clang__
	extern "C" nat8 _addcarry_u64(nat8 cf,nat a,nat b,nat *r)  {
		*r=a+b+cf;
		return (a+b<a) | (a+b+cf<a+b);
	}
	extern "C" nat8 _subborrow_u64(nat8 bf,nat a,nat b,nat *r) {
		*r=a-b-bf;
		return (a-b>a) | (a-b-bf>a-b);
	}
	nat _udiv128(nat Dh,nat Dl,nat d,nat* r) {
		__asm__("divq %4": "=a"(Dl), "=d"(Dh): "0"(Dl), "d"(Dh), "rm" (d));
		return *r=Dh,Dl; // ALT: unsigned __int128 D=(unsigned __int128)(Dh)<<64 + Dl; return *r=D%d,D/d;
	}
#else
	extern "C" nat8 _addcarry_u64(nat8 cf,nat a,nat b,nat *r);
	extern "C" nat8 _subborrow_u64(nat8 bf,nat a,nat b,nat *r);
	extern "C" nat64 _udiv128(nat64 Dh,nat64 Dl,nat64 d,nat64* r);
#endif

// Windows structs and consts.
static const nat32 STACK_SIZE_PARAM_IS_A_RESERVATION=0x00010000;
struct MEMORYSTATUS {
	nat32 dwLength,dwMemoryLoad;
	nat dwTotalPhys,dwAvailPhys,dwTotalPageFile,dwAvailPageFile,dwTotalVirtual,dwAvailVirtual;
};

// Windows APIs.
extern "C" void  __stdcall Sleep(nat32 msec);
extern "C" void* __stdcall GetStdHandle(nat32);
extern "C" nat32 __stdcall IsDebuggerPresent();
extern "C" void  __stdcall OutputDebugStringA(const char*);
extern "C" void  __stdcall OutputDebugStringW(const char16*);
extern "C" nat32 __stdcall WriteFile(void*,void*,nat32,nat32*,void*);
extern "C" void  __stdcall PostQuitMessage(int32);
extern "C" void* __stdcall CreateFileMapping(void* hFile,void* lpAttributes,nat32 flProtect,nat32 dwMaximumSizeHigh,nat32 dwMaximumSizeLow,const char16* lpName); // flProtect: PAGE_READONLY=0x02,PAGE_READWRITE=0x04,PAGE_WRITECOPY=0x08,SEC_IMAGE_NO_EXECUTE=0x11000000
extern "C" void* __stdcall MapViewOfFile(void* hFileMappingObject,nat32 dwDesiredAccess,nat32 dwFileOffsetHigh,nat32 dwFileOffsetLow,nat dwNumberOfBytesToMap); // FILE_MAP_READ,FILE_MAP_WRITE
extern "C" nat32 __stdcall WaitForSingleObject(void* hHandle,nat32 dwMilliseconds);
extern "C" nat32 __stdcall GetFileType(void* hFile);
extern "C" nat32 __stdcall WriteConsoleW(void* hConsoleOutput,void* lpBuffer,nat32 nNumberOfCharsToWrite,nat32* lpNumberOfCharsWritten,void* lpReserved);
extern "C" nat32 __stdcall CloseHandle(void* h);
extern "C" void* __stdcall CreateThread(void* lpThreadAttributes,nat dwStackSize,nat32(__stdcall*lpStartAddress)(void*),void* lpParameter,nat32 dwCreationFlags,nat32* lpThreadId);
extern "C" void* __stdcall FlushProcessWriteBuffers();
extern "C" void  __stdcall GlobalMemoryStatus(MEMORYSTATUS*);
extern "C" void* __stdcall CreateFileW(char16* lpFileName,nat32 dwDesiredAccess,nat32 dwShareMode,void* lpSecurityAttributes,nat32 dwCreationDisposition,nat32 dwFlagsAndAttributes,void* hTemplateFile);
extern "C" int32 __stdcall GetFileSizeEx(void* hFile,int64* lpFileSize);
extern "C" int32 __stdcall ReadFile(void* hFile,void* lpBuffer,nat32 nNumberOfBytesToRead,nat32* lpNumberOfBytesRead,void* lpOverlapped);
extern "C" [[noreturn]] void __stdcall ExitProcess(unsigned int);

// Entry point.
extern "C" int32 main();
int32 WinMain(void* hInstance,void* hPrevInstance,char16* lpCmdLine,int32 nCmdShow) {return main();}

// Core.
[[noreturn]] void Verse::Exit(nat32 Code) {ExitProcess(Code);}

// Arithmetic.
nat8 Verse::SumCarry               (nat8  cf,nat64 a ,nat64  b ,nat64* r) {return _addcarry_u64(cf,a,b,r);}
nat8 Verse::DifferenceBorrow       (nat8  bf,nat64 a ,nat64  b ,nat64* r) {return _subborrow_u64(bf,a,b,r);}
nat  Verse::ProductCarry           (nat64 a ,nat64 b ,nat64* rh)          {return _umul128(a,b,rh);}
nat  Verse::ProductCarry           (int64 a ,int64 b ,int64* rh)          {return _mul128(a,b,rh);}
nat  Verse::TruncatingDivisionCarry(nat64 Dh,nat64 Dl,nat64 d  ,nat64* r) {return _udiv128(Dh,Dl,d,r);}

// Platform APIs.
nat Verse::Clock() {
	return __rdtsc();
}

// Shared Memory.
nat8  Verse::AndAtomic  (nat8  volatile* ap,nat8  b) {return _InterlockedAnd8((volatile char*)ap,b);}
nat8  Verse::OrAtomic   (nat8  volatile* ap,nat8  b) {return _InterlockedOr8((volatile char*)ap,b);}
nat64 Verse::AndAtomic  (nat64 volatile* ap,nat64 b) {return (nat64)_InterlockedAnd64((int64*)ap,(int64)b);}
nat64 Verse::OrAtomic   (nat64 volatile* ap,nat64 b) {return (nat64)_InterlockedOr64((int64*)ap,(int64)b);}
nat64 Verse::AddAtomic  (nat64 volatile* ap,int64 b) {return (nat64)_InterlockedExchangeAdd64((int64*)ap,b);}
nat8  Verse::CompareExchangeAtomic(nat8 volatile* Target,nat8 NewValue,nat8 IfValue) {
	return _InterlockedCompareExchange8((char*)Target,char(NewValue),char(IfValue));
}
nat64 Verse::CompareExchangeAtomic(nat64 volatile* Target,nat64 NewValue,nat64 IfValue) {
	return (nat64)_InterlockedCompareExchange64((int64 volatile*)Target,(int64)NewValue,(int64)IfValue);
}
bool Verse::LinkAtomic(nat8 volatile* Target,nat8 NewValue,nat8* IfValue) {
	nat8 old=*IfValue;
	return *IfValue=CompareExchangeAtomic(Target,NewValue,old), *IfValue==old;
}
bool Verse::LinkAtomic(nat64 volatile* Target,nat64 NewValue,nat64* IfValue) {
	// Can't unconditionally assign IfValue to result of CompareExchangeAtomic because it may be live.
	if(nat64 Expect=*IfValue, Found=CompareExchangeAtomic(Target,NewValue,Expect); Expect==Found)
		return true;
	else
		return *IfValue=Found,false;
}
void Verse::FenceAtomic() {
	nat64 Temp; AddAtomic(&Temp,0); // 18cyc, guarantees total volatile serialization.
}
void Verse::FenceGlobal() {
	FlushProcessWriteBuffers();
}

// Platform threading.
struct run_thread_helper {void(*Entry)(Internal::thread_startup*); Internal::thread_startup* ThreadStartup;};
VERSE_NO_INLINE static nat32 __stdcall ThreadEntry(void* ThreadStartup) {
	auto Helper=(run_thread_helper*)ThreadStartup;
	Helper->Entry(Helper->ThreadStartup);
	delete Helper;
	return 0;
}
VERSE_NO_INLINE void kernel::RunThread(Internal::thread_startup* Startup) {
	static MEMORYSTATUS MemoryStatus{};
	if(!MemoryStatus.dwTotalPhys)
		MemoryStatus.dwLength=sizeof(MemoryStatus),GlobalMemoryStatus(&MemoryStatus);
	AddAtomic(&RunningThreadCount,1);
	auto Entry=[](Internal::thread_startup* Startup) {
		Startup->PreThreadStartup();
		Startup->ThreadStartup();
		delete Startup;
	};
	auto Helper=new run_thread_helper{Entry,Startup};
	if(void* h=CreateThread(0,MemoryStatus.dwTotalPhys,ThreadEntry,Helper,STACK_SIZE_PARAM_IS_A_RESERVATION,0)) {
		CloseHandle(h);
		return;
	}
	AddAtomic(&RunningThreadCount,-1);
	VERSE_ERR("RunThread failed to create thread");
}
VERSE_NO_INLINE void Verse::Sleep(nat64 Milliseconds) {
	::Sleep(nat32(Min(Milliseconds,nat64(nat32(-1)))));
}

// Logging, without any dependency on library internals.
struct platform_thread {
	const char16* LogData;
	nat           LogLength;
};
static thread_local platform_thread PlatformThread{};
VERSE_NO_INLINE void Verse::Internal::PrintHelper(span<char8> s) {
	nat n=Length(s);
	char16* NewLogData=new char16[PlatformThread.LogLength+n+1];
	for(nat i=0; i<PlatformThread.LogLength; i++)                                 NewLogData[i                         ]=PlatformThread.LogData[i];
	for(nat i=0; i<n;       i++) VERSE_ASSERT(s[i]!=0),/*VERSE_ASSERT(s[i]<128),*/NewLogData[PlatformThread.LogLength+i]=s[i];
	NewLogData[PlatformThread.LogLength+n]=0;
	delete PlatformThread.LogData;
	PlatformThread.LogData=NewLogData;
	PlatformThread.LogLength+=n;
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(const char8* s) {
	return PrintHelper(span<char8>(s));
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(char8* s) {
	return PrintHelper(span<char8>(s));
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(const char* s) {
	return Internal::PrintHelper((const char8*)s);
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(char* s) {
	return Internal::PrintHelper((const char8*)s);
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(nat64 n) {
	char8 buffer[64],*bp=buffer+CountOf(buffer);
	*--bp=0;
	do {*--bp=u8"0123456789"[n%10]; n/=10;} while(n!=0);
	Internal::PrintHelper(bp);
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(int64    i) {if(i<0) Internal::PrintHelper("-"); return Internal::PrintHelper(nat(i>=0? i: -i));}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(int32    i) {return Internal::PrintHelper(int64(i));}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(nat32    i) {return Internal::PrintHelper(nat(i));}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(char    ch) {return Internal::PrintHelper(ToString(ch));}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(char8   ch) {return Internal::PrintHelper(ToString(ch));}
VERSE_NO_INLINE void Verse::Internal::PrintBreak(bool line) {
	// Static so they work at startup regardless of initialization order.
	static void* hConsoleWrite = GetStdHandle((nat32)-11);
	static bool  IsConsole     = GetFileType(hConsoleWrite)==FILE_TYPE_CHAR;
	static nat32 Debugger      = IsDebuggerPresent();
	if(line)
		Internal::PrintHelper("\r\n");
	nat32 tmp   = 0;
	nat   len   = PlatformThread.LogLength*(2-IsConsole);
	int32 len32 = nat32(len);
	VERSE_ASSERT(len==len32);
	if(IsConsole)
		WriteConsoleW(hConsoleWrite,(void*)PlatformThread.LogData,len32,&tmp,0);
	else
		WriteFile(hConsoleWrite,(void*)PlatformThread.LogData,len32,&tmp,0);
	if(Debugger) OutputDebugStringW(PlatformThread.LogData);
	delete PlatformThread.LogData;
	PlatformThread.LogData=0,PlatformThread.LogLength=0;
}
VERSE_NO_INLINE string Verse::LoadTextFile(const string& Filename) {
	auto s=string_as_utf16(Filename);
	int64 n=0,Offset=0;
	if(void* h=CreateFileW(s.UTF16,0x80000000,1,0,3,0,0); h!=(void*)-1 && GetFileSizeEx(h,&n)) {
		auto rs=string(construct_flat{},n,[&](char8* cs) {
			while(Offset<n) {
				nat32 n0=nat32(Min<int64>(n-Offset,0x40000000)),n1=0;
				if(!ReadFile(h,cs+Offset,n0,&n1,0) || n1!=n0)
					break;
				Offset+=n0;
			}
		});
		CloseHandle(h);
		if(Offset==n)
			return rs;
	}
	VERSE_ERR("LoadTextFile failed");
}
#endif
