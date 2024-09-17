//==============================================================================================================================================================
// Text implementation.

#include "Terse.h"
#pragma warning(disable:4100 4244 4706)
using namespace Verse;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------
// Text helpers.

static bool   IsU8(char8 ch)                                      {return ch>=0x80 && ch<=0xBF;}
static char32 MakeChar32(nat32 ch0)                               {return char32( ch0                                                                                 );}
static char32 MakeChar32(nat32 ch0,nat32 ch1)                     {return char32((ch0*0x40    + (ch1&0x3F)                                                ) & 0x7FF   );}
static char32 MakeChar32(nat32 ch0,nat32 ch1,nat32 ch2)           {return char32((ch0*0x1000  + (ch1&0x3F)*0x1000 + nat32(ch2&0x3F)                       ) & 0xFFFF  );}
static char32 MakeChar32(nat32 ch0,nat32 ch1,nat32 ch2,nat32 ch3) {return char32((ch0*0x40000 + (ch1&0x3F)*0x1000 + nat32(ch2&0x3F)*0x40 + nat32(ch3&0x3F)) & 0x1FFFFF);}
nat Verse::ParseUTF8(const string& s,nat i,char32& Char32) {
	nat n=Length(s);
	if(i>=n)
		return 0;
	char8 ch=s[i+0];
	nat32 ch0=ch,ch1,ch2,ch3;
	switch(ch) {
	case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06: case 0x07:
	case 0x08: case 0x09: case 0x0A: case 0x0B: case 0x0C: case 0x0D: case 0x0E: case 0x0F:
	case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15: case 0x16: case 0x17:
	case 0x18: case 0x19: case 0x1A: case 0x1B: case 0x1C: case 0x1D: case 0x1E: case 0x1F:
	case 0x20: case 0x21: case 0x22: case 0x23: case 0x24: case 0x25: case 0x26: case 0x27:
	case 0x28: case 0x29: case 0x2A: case 0x2B: case 0x2C: case 0x2D: case 0x2E: case 0x2F:
	case 0x30: case 0x31: case 0x32: case 0x33: case 0x34: case 0x35: case 0x36: case 0x37:
	case 0x38: case 0x39: case 0x3A: case 0x3B: case 0x3C: case 0x3D: case 0x3E: case 0x3F:
	case 0x40: case 0x41: case 0x42: case 0x43: case 0x44: case 0x45: case 0x46: case 0x47:
	case 0x48: case 0x49: case 0x4A: case 0x4B: case 0x4C: case 0x4D: case 0x4E: case 0x4F:
	case 0x50: case 0x51: case 0x52: case 0x53: case 0x54: case 0x55: case 0x56: case 0x57:
	case 0x58: case 0x59: case 0x5A: case 0x5B: case 0x5C: case 0x5D: case 0x5E: case 0x5F:
	case 0x60: case 0x61: case 0x62: case 0x63: case 0x64: case 0x65: case 0x66: case 0x67:
	case 0x68: case 0x69: case 0x6A: case 0x6B: case 0x6C: case 0x6D: case 0x6E: case 0x6F:
	case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76: case 0x77:
	case 0x78: case 0x79: case 0x7A: case 0x7B: case 0x7C: case 0x7D: case 0x7E: case 0x7F:
		return Char32=MakeChar32(ch0), 1;
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
	case 0xC2: case 0xC3: case 0xC4: case 0xC5: case 0xC6: case 0xC7:
	case 0xC8: case 0xC9: case 0xCA: case 0xCB: case 0xCC: case 0xCD: case 0xCE: case 0xCF:
	case 0xD0: case 0xD1: case 0xD2: case 0xD3: case 0xD4: case 0xD5: case 0xD6: case 0xD7:
	case 0xD8: case 0xD9: case 0xDA: case 0xDB: case 0xDC: case 0xDD: case 0xDE: case 0xDF:
		if(i+1>=n)
			return 0;
		ch1=s[i+1];
		return IsU8(ch1)? (Char32=MakeChar32(ch0,ch1), 2): 0;
	case 0xE0:
		if(i+2>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2];
		return ch1>=0xA0&&ch1<=0xBF && IsU8(ch2)? (Char32=MakeChar32(ch0,ch1,ch2), 3): 0;
	case 0xE1: case 0xE2: case 0xE3: case 0xE4: case 0xE5: case 0xE6: case 0xE7:
	case 0xE8: case 0xE9: case 0xEA: case 0xEB: case 0xEC:
	case 0xEE: case 0xEF:
		if(i+2>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2];
		return IsU8(ch1) && IsU8(ch2)? (Char32=MakeChar32(ch0,ch1,ch2), 3): 0;
	case 0xED:
		if(i+2>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2];
		return ch1>=0x80&&ch1<=0x9F && IsU8(ch2)? (Char32=MakeChar32(ch0,ch1,ch2), 3): 0;
	case 0xF0:
		if(i+3>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2],ch3=s[i+3];
		return ch1>=0x90&&ch1<=0xBF && IsU8(ch2) && IsU8(ch3)? (Char32=MakeChar32(ch0,ch1,ch2,ch3), 4): 0;
	case 0xF1: case 0xF2: case 0xF3:
		if(i+3>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2],ch3=s[i+3];
		return IsU8(ch1) && IsU8(ch2) && IsU8(ch3)? (Char32=MakeChar32(ch0,ch1,ch2,ch3), 4): 0;
	case 0xF4:
		if(i+3>=n)
			return 0;
		ch1=s[i+1],ch2=s[i+2],ch3=s[i+3];
		return ch1>=0x80&&ch1<=0x8F? (Char32=MakeChar32(ch0,ch1,ch2,ch3), 4): 0;
	}
	return 0;
}
bool Verse::IsValidUTF8(const string& s) {
    nat i=0,n=Length(s);
    while(i<n)
        if(char32 Char32; auto o=ParseUTF8(s,i,Char32))
            i+=o;
        else
            return false;
    return true;
}

// Conversion to low level UTF8.
VERSE_NO_INLINE string_as_utf8::string_as_utf8(string S): Length(Verse::Length(S)), UTF8(Storage) {
	// We support strings that aren't valid UTF8, so this mustn't be an error.
	nat i=0;
	if(Length>=sizeof(Storage))
		UTF8=new char8[Length+1]; 
	while(i<Length)
		UTF8[i]=S(i),i++;
	UTF8[i++]=0;
}
VERSE_NO_INLINE string_as_utf8::~string_as_utf8() {
	if(UTF8!=Storage)
		delete[]UTF8;
}

// Conversion to low level UTF16.
VERSE_NO_INLINE string_as_utf16::string_as_utf16(string S): Length(Verse::Length(S)), UTF16(Storage) {
	nat i=0,j=0;
	if(Length>=sizeof(Storage))
		UTF16=new char16[Length+1]; // Safe overestimate.
	while(i<Length) {
		char8 ch=S[i];
		if(ch<128)
			i++,UTF16[j++]=ch;
		else if(char32 Char32; auto n=ParseUTF8(S,i,Char32)) {
			if(Char32<=0xFFFF)
				UTF16[j++]=char16(Char32);
			else if(Char32<=0x10FFFF)
				UTF16[j++]=char16(0xD400+(Char32/0x400)),
				UTF16[j++]=char16(0xDC00+(Char32&0x3FF));
			i+=n;
			continue;
		}
		else VERSE_ERR("Bad UTF16 sequence");
	}
	UTF16[j++]=0;
}
VERSE_NO_INLINE string_as_utf16::~string_as_utf16() {
	if(UTF16!=Storage)
		delete[]UTF16;
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(const string& a) {
	auto b=string_as_utf8(a);
	return PrintHelper(b.UTF8);
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(const future<>& a) {
	auto b=string_as_utf8(ToString(a));
	return PrintHelper(b.UTF8);
}
VERSE_NO_INLINE void Verse::Internal::PrintHelper(bool B) {
	return PrintHelper(B? "true": "false");
}
