
#pragma hdrstop

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

#ifdef NEVER_COMPILE
#include <stdio.h>

// Dummy entry point
int main(int argc, char* argv[])
{
	printf("*** Compile XercesC Library with Deprecated DOM API for this test\n");
	return 0;
}
#else
 // The real entry point is in the Samples folder
 #include <DeprecatedDOMCount.cpp>
#endif
