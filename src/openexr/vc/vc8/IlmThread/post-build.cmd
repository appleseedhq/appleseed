set srcdir=..\..\..\IlmThread
cd %srcdir%
set instdir=..\vc\vc8\include\OpenEXR
mkdir %instdir%
copy IlmThreadPool.h %instdir%
copy IlmThread.h %instdir%
copy IlmThreadSemaphore.h %instdir%
copy IlmThreadMutex.h %instdir%
set srcdir=..\config.windows
cd %srcdir%
copy OpenEXRConfig.h %instdir%
