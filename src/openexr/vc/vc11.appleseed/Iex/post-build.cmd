set srcdir=..\..\..\Iex
cd %srcdir%
set instdir=..\include\OpenEXR
mkdir %instdir%
copy IexBaseExc.h %instdir%
copy IexMathExc.h %instdir%
copy IexThrowErrnoExc.h %instdir%
copy IexErrnoExc.h %instdir%
copy IexMacros.h %instdir%
copy Iex.h %instdir%
