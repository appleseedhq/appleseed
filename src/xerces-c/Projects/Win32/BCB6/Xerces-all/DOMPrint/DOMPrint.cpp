
#pragma hdrstop
#include <condefs.h>

//---------------------------------------------------------------------------
USELIB("..\..\..\..\..\Build\Win32\BCB6\XercesLib.lib");
USEUNIT("..\..\..\..\..\samples\DOMPrint\DOMTreeErrorReporter.cpp");
USEUNIT("..\..\..\..\..\samples\DOMPrint\DOMPrintFilter.cpp");
USEUNIT("..\..\..\..\..\samples\DOMPrint\DOMPrintErrorHandler.cpp");

//---------------------------------------------------------------------------

#ifdef NEVER_COMPILE
// Dummy entry point to satisfy the BCB IDE
int main(int argc, char* argv[])
{
	return 0;
}
#endif

// The real entry point is in the Samples folder
#include <DOMPrint.cpp>
