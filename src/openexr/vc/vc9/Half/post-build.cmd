set srcdir=..\..\..\Half
cd %srcdir%
set instdir=..\vc\vc8\include\OpenEXR
mkdir %instdir%
copy half.h %instdir%
copy halfFunction.h %instdir%
copy halfLimits.h %instdir%
