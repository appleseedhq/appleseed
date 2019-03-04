#if defined(__arm__) || defined(__TARGET_ARCH_ARM)
             #error cmake_ARCH ARM
         #elif defined(__i386) || defined(__i386__) || defined(_M_IX86)
             #error cmake_ARCH i386
         #elif defined(__x86_64) || defined(__x86_64__) || defined(__amd64) || defined(_M_X64)
             #error cmake_ARCH x86_64
         #elif defined(__ppc__) || defined(__ppc) || defined(__powerpc__) \
             || defined(_ARCH_COM) || defined(_ARCH_PWR) || defined(_ARCH_PPC) \
             || defined(_M_MPPC) || defined(_M_PPC)
             #error cmake_ARCH PPC
         #endif
         #error cmake_ARCH unknown