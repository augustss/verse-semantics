# TimVerse

This directory contain's Tim's verse imlementation.

The main() used to live in Test.cpp, it is now in Main.cpp.



## Changes

 - VS 22 complained when building about main() double defined.
   => added pragma TG to VerseGrammarTest.cpp and commented out
      main

 - The code throws Access Violations exceptions (nin RegisterStatic
   from VerseReference.cpp.  This is intended by Tim.
   The fix is to turn off the exception in the VS IDE when running
   in debugging mode

 - To speed up compilation and make debugging nicer, turn off optimizations.

  Configuration Properties
      --> C++
            --> Optimization /O2 goes to /Od
            --> Inline function expension /Ob2 goes /Ob0
            --> Whole program optimization  goes to No	    

 - The program does not ouput put to the terminal. `

   Configuration Properties -->
        --> Linker
	       --> System
	            --> SubSystem Windows goes to Console

 - Added a Main.cpp to handle command line options