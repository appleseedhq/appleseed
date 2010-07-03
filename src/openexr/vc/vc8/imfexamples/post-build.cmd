set srcdir=..\..\..\IlmImfExamples
cd %srcdir%
set instdir=..\vc\vc8\examples
mkdir %instdir%
copy drawImage.cpp %instdir%
copy generalInterfaceExamples.cpp %instdir%
copy main.cpp %instdir%
copy rgbaInterfaceExamples.cpp %instdir%
copy drawImage.h %instdir%
copy generalInterfaceExamples.h %instdir%
copy rgbaInterfaceExamples.h %instdir%
cd ..\vc\vc8
copy imfexamples\imfexamples.vcproj examples\imfexamples.vcproj
