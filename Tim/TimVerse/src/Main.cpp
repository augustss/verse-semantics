#include <iostream>
#include <cstdio>
#include <fstream>
#include <map>
#include <cstdlib>
#include <string> 

// The theory group changs to the TimVerse are fenced by this pragma
#include "Main.h"

// Declaration of the function in Test.cpp
int runtests();
// From Verse.cpp
void TestScript(const char*);

using std::string; 
using std::map;

map<string, string> args;

int Has(string k) { return args.find(k) != args.end(); }
int MapTo(string k, string v) { return Has(k) != 0 && args[k] == v; }
string Get(string k) { return Has(k) ? args[k] : ""; }


int main(int argc, char* argv[]) {
#ifdef TG
	// Simulated command-line arguments, use when nothing else 
	char* simv[] = {"", "-test", "verse/New.verse", "-tim", "false"};
    int simc = sizeof(simv) / sizeof(simv[0]);	
	if (argc == 1) { argv = simv; argc = simc; }
	
	// Simple minded argv parser
	for (int i = 1; i < argc; ++i) 
		if (argv[i][0] == '-' && i + 1 < argc) {
			args[argv[i]] = argv[i+1];
			i++;
		}

	if (Has("-test")) {
		TestScript(Get("-test").c_str());
	}

	// Should we run the Tim Tests? They take a while... 
	if (MapTo("-tim","true")) runtests();

#else
	// Original behavior, running all the Tim Tests
	runtests();
#endif
}

