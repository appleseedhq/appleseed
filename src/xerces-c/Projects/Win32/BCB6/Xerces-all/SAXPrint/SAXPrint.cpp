
#pragma hdrstop
#include <condefs.h>

//---------------------------------------------------------------------------
USELIB("..\..\..\..\..\Build\Win32\BCB6\XercesLib.lib");
USEUNIT("..\..\..\..\..\samples\SAXPrint\SAXPrintHandlers.cpp");

//---------------------------------------------------------------------------

#ifdef NEVER_COMPILE
// Dummy entry point to satisfy the BCB IDE
#pragma ARGS_USED
int main(int argc, char* argv[])
{
	return 0;
}
#endif

// The real entry point is in the Samples folder
#include <SAXPrint.cpp>
