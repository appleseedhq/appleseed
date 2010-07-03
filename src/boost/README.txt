
Version 1.39.0

New Libraries

     * Signals2: Managed signals & slots callback implementation (thread-safe version 2), from Frank Mori Hess.

Updated Libraries

     * Asio:
          + Implement automatic resizing of the bucket array in the internal hash maps. This is to improve performance for very large numbers of asynchronous operations and also to reduce memory usage for very small numbers. A new macro BOOST_ASIO_HASH_MAP_BUCKETS may be used to tweak the sizes used for the bucket arrays.
          + Add performance optimisation for the Windows IOCP backend for when no timers are used.
          + Prevent locale settings from affecting formatting of TCP and UDP endpoints (#2682).
          + Fix a memory leak that occurred when an asynchronous SSL operation's completion handler threw an exception (#2910).
          + Fix the implementation of io_control() so that it adheres to the documented type requirements for IoControlCommand (#2820).
          + Fix incompatibility between Asio and ncurses.h (#2156).
          + On Windows, specifically handle the case when an overlapped ReadFile call fails with ERROR_MORE_DATA. This enables a hack where a windows::stream_handle can be used with a message-oriented named pipe (#2936).
          + Fix system call wrappers to always clear the error on success, as POSIX allows successful system calls to modify errno (#2953).
          + Don't include termios.h if BOOST_ASIO_DISABLE_SERIAL_PORT is defined (#2917).
          + Cleaned up some more MSVC level 4 warnings (#2828).
          + Various documentation fixes (#2871).
     * Flyweight:
          + The refcounted component was not thread-safe due to an incorrect implementation and could deadlock under heavy usage conditions. This problem has been corrected.
     * Foreach:
          + Eliminate shadow warnings on gcc for nested FOREACH loops
          + Portability fix for Intel-Win toolset
     * Hash:
          + Remove deprecated headers for hashing containers. Everything that was in them is included in <functional/hash.hpp> (#2412).
          + Other minor changes, full details in the library change log.
     * Interprocess:
          + Increased portability and bug fixes. Full details in the library change log.
     * Intrusive:
          + Optimizations and bug fixes. Full details in the library change log.
     * Program.Options:
          + Multitoken options fixed (#469).
     * Proto:
          + Work around incompatibility with standard Linux header.
          + Add proto::noinvoke<> to block metafunction invocation in object transforms.
     * PtrContainer:
          + Bug fixes from Trac applied.
     * Range:
          + Bug fixes from Trac applied.
     * Unordered:
          + Fixed regression in 1.38 that prevented unordered from using more than about 1.5 million buckets (#2975).
          + Minor implementation changes, including #2756. Full details in the library change log.
     * Xpressive:
          + Work around for gcc optimization problem resulting in pure virtual function call runtime error (#2655).

Updated Tools

     * Boostbook:
          + Improved PDF generation.
          + Preliminary HTMLHelp support.
          + Add default path for callout images.
          + Include data members' <purpose> in the class synopsis.
          + Fix bug where a function's <purpose> wasn't displayed if it was just plain text.
          + Support the alt tag in <headername> and <macroname>. Use this if the header or macro name is different to the contents of the tag (#1977).
          + Support links relative to the boost root in <ulink> tags, using a custom url, see the linking documentation for details (#1166).
          + Avoid generating filenames that only differ in case for function, method and macro documentation.
          + Run the docbook chunker quietly, unless boostbook.verbose is set. This parameter might be used in other places in future releases.
          + Make the 1.1 DTD available.
          + Fill in some missing reference documentation (partially fixes #2153).
          + Changes to doxygen integration:
               o Support \throw.
               o Support global variables and enums.
               o Better support for documentation written in function and method bodies.
               o Workaround a problem with doxygen 1.5.8's xml output (#2937).
     * Quickbook:
          + Return an error code and error count if there are any errors (#1399).
          + Support both windows and cygwin paths at the compile line when compiled with cygwin.
          + Fix some issues with C++ and Python code:
               o Fail gracefully for a mismatched ''.
               o Warn if any unexpected character are encountered and write them out properly (#1170).
               o Fix a bug for hex encoded characters in strings (#2860).
          + Improved testing, including tests for expected failures.
          + Generate valid document info for document types other than library (#2711):
               o Remove library specific attributes.
               o Put title before info block.
          + Fix a bug when calling templates.
          + Less warnings when built using gcc.
          + Small documentation improvements (#1213, #2701).
          + Fix a bug with xinclude pages when outdir is the current directory (#2921).

Compilers Tested

   Boost's primary test compilers are:
     * OS X:
          + GCC 4.0.1 on Intel Tiger and Leopard
          + GCC 4.0.1 on PowerPC Tiger
     * Linux:
          + GCC 4.3.2 on Ubuntu Linux.
          + GCC 4.3.3 on Debian "unstable".
     * Windows:
          + Visual C++ 7.1 SP1, 8.0 SP1 and 9.0 SP1 on Windows XP.

   Boost's additional test compilers include:
     * Linux:
          + Intel 9.0 on Red Hat Enterprise Linux
          + Intel 10.0 on Red Hat Enterprise Linux
          + Intel 10.1 on 64-bit Linux Redhat 5.1 Server.
          + Intel 10.1 on Suse Linux on 64 bit Itanium
          + Intel 11.0 on Red Hat Enterprise Linux
          + GCC 4.1.1, 4.2.1 on 64-bit Red Hat Enterprise Linux
          + GCC 4.1.2 on 64-bit Redhat Server 5.1
          + GCC 4.1.2 on Suse Linux on 64 bit Itanium
          + GCC 3.4.3, GCC 4.0.1, GCC 4.2.4 and GCC 4.3.2 on Red Hat Enterprise Linux
          + GCC 4.3.2 with C++0x extensions on Red Hat Enterprise Linux
          + GCC 4.2.1 on OpenSuSE Linux
          + QLogic PathScale(TM) Compiler Suite: Version 3.1 on Red Hat Enterprise Linux
          + GNU gcc version 4.2.0 (PathScale 3.2 driver) on 64-bit Red Hat Enterprise Linux
          + Sun 5.9 on Red Hat Enterprise Linux
     * OS X:
          + Intel 9.1, 10.1 on Tiger
          + Intel 10.1, 11.0 on Leopard
          + GCC 4.2.1 on Leopard
     * Windows:
          + Visual C++ 9.0 on Vista.
          + Visual C++ 9.0, using STLport 5.2, on XP and Windows Mobile 5.0.
          + Borland 5.9.3
          + Borland 6.1.0
          + Intel C++ 11.0, with a Visual C++ 9.0 backend, on XP 32-bit.
          + Intel C++ 11.0, with a Visual C++ 9.0 backend, on Vista 64-bit. (TODO: not recently)
          + GCC 4.3.3, on Mingw
     * AIX:
          + IBM XL C/C++ Enterprise Edition for AIX, V10.1.0.0, on AIX Version 5.3.0.40
     * NetBSD:
          + GCC 4.1.2 on NetBSD 4.0/i386 and NetBSD 4.0/amd64.
     * Solaris:
          + Sun C++ 5.7, 5.8, 5.9 on Solaris 5.10
          + GCC 3.4.6 on Solaris 5.10

Acknowledgements

   Beman Dawes, Eric Niebler, Rene Rivera, and Daniel James managed this release.
