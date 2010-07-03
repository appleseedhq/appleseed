set srcdir=..\..\..\Iex
cd %srcdir%
set instdir=..\vc\vc8\include\OpenEXR
mkdir %instdir%
copy IexBaseExc.h %instdir%
copy IexMathExc.h %instdir%
copy IexThrowErrnoExc.h %instdir%
copy IexErrnoExc.h %instdir%
copy IexMacros.h %instdir%
copy Iex.h %instdir%
