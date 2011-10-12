/* windows/src/H5pubconf.h. Adapted from generated Linux src/H5pubconf.h   */
/* src/H5config.h.  Generated from H5config.h.in by configure.  */
/* src/H5config.h.in.  Generated from configure.in by autoheader.  */

/*
 * Windows Specific Definitions
 */

/* Define if the Windows virtual file driver should be compiled */
#define H5_HAVE_WINDOWS 1

/* Define if the Windows virtual file driver should use buffered IO functions */
/* #undef WINDOWS_USE_STDIO */

/* Define the maximum write size for the Windows file driver.  Larger writes
   will be split into many writes.  Safe values are 1 <= WINDOWS_MAX_BUF <= 2GB-1. */
#define WINDOWS_MAX_BUF (1024 * 1024 * 1024)

/*
 * End of Windows Specific Definitions
 */
 
/* Define if your system generates wrong code for log2 routine. */
/* #undef H5_BAD_LOG2_CODE_GENERATED */

/* Define if the memory buffers being written to disk should be cleared before
   writing. */
#define H5_CLEAR_MEMORY 1

/* Define if your system can handle converting denormalized floating-point
   values. */
#define H5_CONVERT_DENORMAL_FLOAT 1

/* Define if C++ compiler recognizes offsetof */
#define H5_CXX_HAVE_OFFSETOF 1

/* Define the default virtual file driver to compile */
#define H5_DEFAULT_VFD H5FD_WINDOWS

/* Define if `dev_t' is a scalar */
#define H5_DEV_T_IS_SCALAR 1

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef H5_FC_DUMMY_MAIN */

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef H5_FC_DUMMY_MAIN_EQ_F77 */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define H5_FC_FUNC(name,NAME) NAME

/* As FC_FUNC, but for C identifiers containing underscores. */
#define H5_FC_FUNC_(name,NAME) NAME

/* LAHEY compiler for C identifiers containing underscores. */
/* #define H5_FC_FUNC(name,NAME) name ## _ */
/* #define H5_FC_FUNC_(name,NAME) name ## _ */

/* Define if your system can handle overflow converting floating-point to
   integer values. */
#define H5_FP_TO_INTEGER_OVERFLOW_WORKS 1

/* Define if your system roundup accurately converting floating-point to
   unsigned long long values. */
#define H5_FP_TO_ULLONG_ACCURATE 1

/* Define if your system has right maximum convert floating-point to unsigned
   long long values. */
/* #undef H5_FP_TO_ULLONG_RIGHT_MAXIMUM 1 */

/* Define if gettimeofday() populates the tz pointer passed in */
#define H5_GETTIMEOFDAY_GIVES_TZ 1

/* Define to 1 if you have the `alarm' function. */
/* #undef H5_HAVE_ALARM */

/* Define if the __attribute__(()) extension is present */
/* #undef H5_HAVE_ATTRIBUTE */

/* Define to 1 if you have the `BSDgettimeofday' function. */
/* #undef H5_HAVE_BSDGETTIMEOFDAY */

/* Define if the function stack tracing code is to be compiled in */
/* #undef H5_HAVE_CODESTACK */

/* Define to 1 if you have the declaration of `tzname', and to 0 if you don't.
   */
#define H5_HAVE_DECL_TZNAME 1

/* Define to 1 if you have the `difftime' function. */
#define H5_HAVE_DIFFTIME 1

/* Define if the direct I/O virtual file driver should be compiled */
/* #undef H5_HAVE_DIRECT */

/* Define to 1 if you have the <dlfcn.h> header file. */
/* #undef H5_HAVE_DLFCN_H */

/* Define to 1 if you have the <dmalloc.h> header file. */
/* #undef H5_HAVE_DMALLOC_H */

/* Define to 1 if you have the <features.h> header file. */
/* #undef H5_HAVE_FEATURES_H */

/* Define if support for deflate (zlib) filter is enabled */
#define H5_HAVE_FILTER_DEFLATE 1

/* Define if support for Fletcher32 checksum is enabled */
#define H5_HAVE_FILTER_FLETCHER32 1

/* Define if support for nbit filter is enabled */
#define H5_HAVE_FILTER_NBIT 1

/* Define if support for scaleoffset filter is enabled */
#define H5_HAVE_FILTER_SCALEOFFSET 1

/* Define if support for shuffle filter is enabled */
#define H5_HAVE_FILTER_SHUFFLE 1

/* Define if support for szip filter is enabled */
#define H5_HAVE_FILTER_SZIP 1

/* Define to 1 if you have the `fork' function. */
/* #undef H5_HAVE_FORK */

/* Define to 1 if you have the `frexpf' function. */
/* #undef H5_HAVE_FREXPF */

/* Define to 1 if you have the `frexpl' function. */
/* #undef H5_HAVE_FREXPL */

/* Define to 1 if you have the `fseek64' function. */
/* #undef H5_HAVE_FSEEK64 */

/* Define to 1 if you have the `fseeko' function. */
/* #undef H5_HAVE_FSEEKO */

/* Define to 1 if you have the `fstat64' function. */
/* #undef H5_HAVE_FSTAT64 */

/* Define to 1 if you have the `ftello' function. */
/* #undef H5_HAVE_FTELLO */

/* Define if the compiler understand the __FUNCTION__ keyword */
#define H5_HAVE_FUNCTION 1

/* Define to 1 if you have the `GetConsoleScreenBufferInfo' function. */
#define H5_HAVE_GETCONSOLESCREENBUFFERINFO 1

/* Define to 1 if you have the `gethostname' function. */
#define H5_HAVE_GETHOSTNAME 1

/* Define to 1 if you have the `getpwuid' function. */
/* #undef H5_HAVE_GETPWUID */

/* Define to 1 if you have the `getrusage' function. */
/* #define H5_HAVE_GETRUSAGE 1 */

/* Define to 1 if you have the `gettextinfo' function. */
/* #undef H5_HAVE_GETTEXTINFO */

/* Define to 1 if you have the `gettimeofday' function. */
#define H5_HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the `get_fpc_csr' function. */
/* #undef H5_HAVE_GET_FPC_CSR */

/* Define if we have GPFS support */
/* #undef H5_HAVE_GPFS */

/* Define to 1 if you have the <gpfs.h> header file. */
/* #undef H5_HAVE_GPFS_H */

/* Define if h5dump packed bits feature is enabled */
#define H5_HAVE_H5DUMP_PACKED_BITS

/* Define if library will contain instrumentation to detect correct
   optimization operation */
/* #undef H5_HAVE_INSTRUMENTED_LIBRARY */

/* Define to 1 if you have the <inttypes.h> header file. */
/* #undef H5_HAVE_INTTYPES_H */

/* Define to 1 if you have the `ioctl' function. */
/* #undef H5_HAVE_IOCTL */

/* Define to 1 if you have the <io.h> header file. */
#define H5_HAVE_IO_H

/* Define to 1 if you have the `dmalloc' library (-ldmalloc). */
/* #undef H5_HAVE_LIBDMALLOC */

/* Define to 1 if you have the `lmpe' library (-llmpe). */
/* #undef H5_HAVE_LIBLMPE */

/* Define to 1 if you have the `m' library (-lm). */
/* #undef H5_HAVE_LIBM */

/* Define to 1 if you have the `mpe' library (-lmpe). */
/* #undef H5_HAVE_LIBMPE */

/* Define to 1 if you have the `mpi' library (-lmpi). */
/* #undef H5_HAVE_LIBMPI */

/* Define to 1 if you have the `mpich' library (-lmpich). */
/* #undef H5_HAVE_LIBMPICH */

/* Define to 1 if you have the `mpio' library (-lmpio). */
/* #undef H5_HAVE_LIBMPIO */

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef H5_HAVE_LIBNSL */

/* Define to 1 if you have the `pthread' library (-lpthread). */
/* #undef H5_HAVE_LIBPTHREAD */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef H5_HAVE_LIBSOCKET */

/* Define to 1 if you have the `sz' library (-lsz). */
#define H5_HAVE_LIBSZ 1

/* Define to 1 if you have the `z' library (-lz). */
#define H5_HAVE_LIBZ 1

/* Define to 1 if you have the `longjmp' function. */
#define H5_HAVE_LONGJMP 1

/* Define to 1 if you have the `lseek64' function. */
/* #undef H5_HAVE_LSEEK64 */

/* Define to 1 if you have the <memory.h> header file. */
#define H5_HAVE_MEMORY_H 1

/* Define if we have MPE support */
/* #undef H5_HAVE_MPE */

/* Define to 1 if you have the <mpe.h> header file. */
/* #undef H5_HAVE_MPE_H */

/* Define if MPI_File_get_size works correctly */
#define H5_HAVE_MPI_GET_SIZE 1

/* Define if `MPI_Comm_c2f' and `MPI_Comm_f2c' exists */
/* #undef H5_HAVE_MPI_MULTI_LANG_Comm */

/* Define if `MPI_Info_c2f' and `MPI_Info_f2c' exists */
/* #undef H5_HAVE_MPI_MULTI_LANG_Info */

/* Define if we have parallel support */
/* #undef H5_HAVE_PARALLEL */

/* Define to 1 if you have the <pthread.h> header file. */
/* #undef H5_HAVE_PTHREAD_H */

/* Define to 1 if you have the `random' function. */
/* #undef H5_HAVE_RANDOM */

/* Define to 1 if you have the `rand_r' function. */
/* #undef H5_HAVE_RAND_R */

/* Define to 1 if you have the <setjmp.h> header file. */
#define H5_HAVE_SETJMP_H 1

/* Define to 1 if you have the `setsysinfo' function. */
/* #undef H5_HAVE_SETSYSINFO */

/* Define to 1 if you have the `sigaction' function. */
/* #undef H5_HAVE_SIGACTION */

/* Define to 1 if you have the `siglongjmp' function. */
/* #undef H5_HAVE_SIGLONGJMP */

/* Define to 1 if you have the `signal' function. */
#define H5_HAVE_SIGNAL 1

/* Define to 1 if you have the `snprintf' function. */
/* #undef H5_HAVE_SNPRINTF */

/* Define to 1 if you have the `srandom' function. */
/* #undef H5_HAVE_SRANDOM */

/* Define to 1 if you have the `stat64' function. */
/* #undef H5_HAVE_STAT64 */

/* Define if `struct stat' has the `st_blocks' field */
/* #undef H5_HAVE_STAT_ST_BLOCKS */

/* Define to 1 if you have the <stddef.h> header file. */
#define H5_HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
/* #undef H5_HAVE_STDINT_H */

/* Define to 1 if you have the <stdlib.h> header file. */
#define H5_HAVE_STDLIB_H 1

/* Define to 1 if you have the `strdup' function. */
#define H5_HAVE_STRDUP 1

/* Define to 1 if you have the <strings.h> header file. */
/* #undef H5_HAVE_STRINGS_H */

/* Define to 1 if you have the <string.h> header file. */
#define H5_HAVE_STRING_H 1

/* Define if `struct text_info' is defined */
/* #undef H5_HAVE_STRUCT_TEXT_INFO */

/* Define if `struct timezone' is defined */
#define H5_HAVE_STRUCT_TIMEZONE 1

/* Define to 1 if `tm_zone' is member of `struct tm'. */
/* #undef H5_HAVE_STRUCT_TM_TM_ZONE */

/* Define if `struct videoconfig' is defined */
/* #undef H5_HAVE_STRUCT_VIDEOCONFIG */

/* Define to 1 if you have the `system' function. */
#define H5_HAVE_SYSTEM 1

/* Define to 1 if you have the <sys/fpu.h> header file. */
/* #undef H5_HAVE_SYS_FPU_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
/* #undef H5_HAVE_SYS_IOCTL_H */

/* Define to 1 if you have the <sys/proc.h> header file. */
/* #undef H5_HAVE_SYS_PROC_H */

/* Define to 1 if you have the <sys/resource.h> header file. */
/* #undef H5_HAVE_SYS_RESOURCE_H */

/* Define to 1 if you have the <sys/socket.h> header file. */
/* #undef H5_HAVE_SYS_SOCKET_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define H5_HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysinfo.h> header file. */
/* #undef H5_HAVE_SYS_SYSINFO_H */

/* Define to 1 if you have the <sys/timeb.h> header file. */
#define H5_HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
/* #undef H5_HAVE_SYS_TIME_H */

/* Define to 1 if you have the <sys/types.h> header file. */
#define H5_HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <szlib.h> header file. */
#define H5_HAVE_SZLIB_H 1

/* Define if we have thread safe support */
/* #undef H5_HAVE_THREADSAFE */

/* Define if `timezone' is a global variable */
#define H5_HAVE_TIMEZONE 1

/* Define if the ioctl TIOCGETD is defined */
/* #undef H5_HAVE_TIOCGETD */

/* Define if the ioctl TIOGWINSZ is defined */
/* #undef H5_HAVE_TIOCGWINSZ */

/* Define to 1 if you have the `tmpfile' function. */
#define H5_HAVE_TMPFILE 1

/* Define if `tm_gmtoff' is a member of `struct tm' */
/* #undef H5_HAVE_TM_GMTOFF */

/* Define to 1 if your `struct tm' has `tm_zone'. Deprecated, use
   `HAVE_STRUCT_TM_TM_ZONE' instead. */
/* #undef H5_HAVE_TM_ZONE */

/* Define to 1 if you don't have `tm_zone' but do have the external array
   `tzname'. */
#define H5_HAVE_TZNAME 1

/* Define to 1 if you have the <unistd.h> header file. */
/* #undef H5_HAVE_UNISTD_H */

/* Define to 1 if you have the `vasprintf' function. */
/* #undef H5_HAVE_VASPRINTF */

/* Define to 1 if you have the `vsnprintf' function. */
/* #undef H5_HAVE_VSNPRINTF */

/* Define to 1 if you have the `waitpid' function. */
/* #undef H5_HAVE_WAITPID */

/* Define if your system has window style path name. */
#define H5_HAVE_WINDOW_PATH 1

/* Define to 1 if you have the <winsock.h> header file. */
#define H5_HAVE_WINSOCK_H 1

/* Define to 1 if you have the <zlib.h> header file. */
#define H5_HAVE_ZLIB_H 1

/* Define to 1 if you have the `_getvideoconfig' function. */
/* #undef H5_HAVE__GETVIDEOCONFIG */

/* Define to 1 if you have the `_scrsize' function. */
/* #undef H5_HAVE__SCRSIZE */

/* Define if `__tm_gmtoff' is a member of `struct tm' */
/* #undef H5_HAVE___TM_GMTOFF */

/* Define if your system can't handle converting floating-point values to long
   long. */
/* #undef H5_HW_FP_TO_LLONG_NOT_WORKS */

/* Define if HDF5's high-level library headers should be included in hdf5.h */
#define H5_INCLUDE_HL 1

/* Define if your system can accurately convert from integers to long double
   values. */
#define H5_INTEGER_TO_LDOUBLE_ACCURATE 1

/* Define if your system can convert long double to integers accurately. */
#define H5_LDOUBLE_TO_INTEGER_ACCURATE 1

/* Define if your system can convert from long double to integer values. */
#define H5_LDOUBLE_TO_INTEGER_WORKS 1

/* Define if your system can convert long double to (unsigned) long long
   values correctly. */
#define H5_LDOUBLE_TO_LLONG_ACCURATE 1

/* Define if your system can convert long double to unsigned int values
   correctly. */
#define H5_LDOUBLE_TO_UINT_ACCURATE 1

/* Define if your system can compile long long to floating-point casts. */
#define H5_LLONG_TO_FP_CAST_WORKS 1

/* Define if your system can convert (unsigned) long long to long double
   values correctly. */
#define H5_LLONG_TO_LDOUBLE_CORRECT 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define H5_LT_OBJDIR ".libs/"

/* Define if the metadata trace file code is to be compiled in */
/* #undef H5_METADATA_TRACE_FILE */

/* Define if your system can handle complicated MPI derived datatype
   correctly. */
#define H5_MPI_COMPLEX_DERIVED_DATATYPE_WORKS 1

/* Define if your system's `MPI_File_set_size' function works for files over
   2GB. */
#define H5_MPI_FILE_SET_SIZE_BIG 1

/* Define if your system can handle special collective IO properly. */
#define H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS 1

/* Define if we can violate pointer alignment restrictions */
#define H5_NO_ALIGNMENT_RESTRICTIONS 1

/* Define if deprecated public API symbols are disabled */
/* #undef H5_NO_DEPRECATED_SYMBOLS */

/* Define if shared writing must be disabled (CodeWarrior only) */
/* #undef H5_NO_SHARED_WRITING */

/* Name of package */
#define H5_PACKAGE "hdf5"

/* Define to the address where bug reports for this package should be sent. */
#define H5_PACKAGE_BUGREPORT "help@hdfgroup.org"

/* Define to the full name of this package. */
#define H5_PACKAGE_NAME "HDF5"

/* Define to the full name and version of this package. */
#define H5_PACKAGE_STRING "HDF5 1.8.7"

/* Define to the one symbol short name of this package. */
#define H5_PACKAGE_TARNAME "hdf5"

/* Define to the version of this package. */
#define H5_PACKAGE_VERSION "1.8.7"

/* Width for printf() for type `long long' or `__int64', use `ll' */
#define H5_PRINTF_LL_WIDTH "I64"

/* The size of `char', as computed by sizeof. */
#define H5_SIZEOF_CHAR 1

/* The size of `double', as computed by sizeof. */
#define H5_SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define H5_SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define H5_SIZEOF_INT 4

/* The size of `int16_t', as computed by sizeof. */
#define H5_SIZEOF_INT16_T 0

/* The size of `int32_t', as computed by sizeof. */
#define H5_SIZEOF_INT32_T 0

/* The size of `int64_t', as computed by sizeof. */
#define H5_SIZEOF_INT64_T 0

/* The size of `int8_t', as computed by sizeof. */
#define H5_SIZEOF_INT8_T 0

/* The size of `int_fast16_t', as computed by sizeof. */
#define H5_SIZEOF_INT_FAST16_T 0

/* The size of `int_fast32_t', as computed by sizeof. */
#define H5_SIZEOF_INT_FAST32_T 0

/* The size of `int_fast64_t', as computed by sizeof. */
#define H5_SIZEOF_INT_FAST64_T 0

/* The size of `int_fast8_t', as computed by sizeof. */
#define H5_SIZEOF_INT_FAST8_T 0

/* The size of `int_least16_t', as computed by sizeof. */
#define H5_SIZEOF_INT_LEAST16_T 0

/* The size of `int_least32_t', as computed by sizeof. */
#define H5_SIZEOF_INT_LEAST32_T 0

/* The size of `int_least64_t', as computed by sizeof. */
#define H5_SIZEOF_INT_LEAST64_T 0

/* The size of `int_least8_t', as computed by sizeof. */
#define H5_SIZEOF_INT_LEAST8_T 0

/* The size of `long', as computed by sizeof. */
#define H5_SIZEOF_LONG 4

/* The size of `long double', as computed by sizeof. */
#define H5_SIZEOF_LONG_DOUBLE 8

/* The size of `long long', as computed by sizeof. */
#define H5_SIZEOF_LONG_LONG 8

/* The size of `off64_t', as computed by sizeof. */
#define H5_SIZEOF_OFF64_T 0

/* The size of `off_t', as computed by sizeof. */
#define H5_SIZEOF_OFF_T 4

/* The size of `short', as computed by sizeof. */
#define H5_SIZEOF_SHORT 2

/* The size of `size_t', as computed by sizeof. */
#ifndef _WIN64
#define H5_SIZEOF_SIZE_T 4
#else
#define H5_SIZEOF_SIZE_T 8
#endif /* _WIN64 */

/* The size of `ssize_t', as computed by sizeof. */
#define H5_SIZEOF_SSIZE_T 0

/* The size of `uint16_t', as computed by sizeof. */
#define H5_SIZEOF_UINT16_T 0

/* The size of `uint32_t', as computed by sizeof. */
#define H5_SIZEOF_UINT32_T 0

/* The size of `uint64_t', as computed by sizeof. */
#define H5_SIZEOF_UINT64_T 0

/* The size of `uint8_t', as computed by sizeof. */
#define H5_SIZEOF_UINT8_T 0

/* The size of `uint_fast16_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_FAST16_T 0

/* The size of `uint_fast32_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_FAST32_T 0

/* The size of `uint_fast64_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_FAST64_T 0

/* The size of `uint_fast8_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_FAST8_T 0

/* The size of `uint_least16_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_LEAST16_T 0

/* The size of `uint_least32_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_LEAST32_T 0

/* The size of `uint_least64_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_LEAST64_T 0

/* The size of `uint_least8_t', as computed by sizeof. */
#define H5_SIZEOF_UINT_LEAST8_T 0

/* The size of `__int64', as computed by sizeof. */
#define H5_SIZEOF___INT64 8

/* Define to 1 if you have the ANSI C header files. */
#define H5_STDC_HEADERS 1

/* Define if strict file format checks are enabled */
/* #undef H5_STRICT_FORMAT_CHECKS */

/* Define if your system supports pthread_attr_setscope(&attribute,
   PTHREAD_SCOPE_SYSTEM) call. */
#define H5_SYSTEM_SCOPE_THREADS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
/* #undef H5_TIME_WITH_SYS_TIME */

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef H5_TM_IN_SYS_TIME */

/* Define if your system can compile unsigned long long to floating-point
   casts. */
#define H5_ULLONG_TO_FP_CAST_WORKS 1

/* Define if your system can convert unsigned long long to long double with
   correct precision. */
#define H5_ULLONG_TO_LDOUBLE_PRECISION 1

/* Define if your system can accurately convert unsigned (long) long values to
   floating-point values. */
/* #undef H5_ULONG_TO_FP_BOTTOM_BIT_ACCURATE */

/* Define using v1.6 public API symbols by default */
/* #undef H5_USE_16_API_DEFAULT */

/* Define if a memory checking tool will be used on the library, to cause
   library to be very picky about memory operations and also disable the
   internal free list manager code. */
/* #undef H5_USING_MEMCHECKER */

/* Version number of package */
#define H5_VERSION "1.8.7"

/* Define if vsnprintf() returns the correct value for formatted strings that
   don't fit into size allowed */
/* #undef H5_VSNPRINTF_WORKS */

/* Data accuracy is prefered to speed during data conversions */
#define H5_WANT_DATA_ACCURACY 1

/* Check exception handling functions during data conversions */
#define H5_WANT_DCONV_EXCEPTION 1

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef H5_WORDS_BIGENDIAN */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef H5_const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#define H5_inline __inline
#endif

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef H5_off_t */

/* Define to `unsigned long' if <sys/types.h> does not define. */
/* #undef H5_size_t */

/* Define to `long' if <sys/types.h> does not define. */
#define H5_ssize_t long
