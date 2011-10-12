/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef SIO_STANDALONE_H__
#define SIO_PERF_H__

/* Header file for building h5perf by standalone mode.
 * Created: Christian Chilan, 2005/5/18.
 */

/** From H5private.h **/

#include "H5public.h"		 /* Include Public Definitions		*/


/*
 * Include ANSI-C header files.
 */
#ifdef H5_STDC_HEADERS
#   include <assert.h>
#   include <ctype.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <float.h>
#   include <limits.h>
#   include <math.h>
#   include <signal.h>
#   include <stdarg.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>
#endif

/* maximum of two, three, or four values */
#undef MAX
#define MAX(a,b)                (((a)>(b)) ? (a) : (b))
#define MAX2(a,b)               MAX(a,b)
#define MAX3(a,b,c)             MAX(a,MAX(b,c))
#define MAX4(a,b,c,d)           MAX(MAX(a,b),MAX(c,d))

/*
 * A macro to portably increment enumerated types.
 */
#ifndef H5_INC_ENUM
#  define H5_INC_ENUM(TYPE,VAR) (VAR)=((TYPE)((VAR)+1))
#endif

/*
 * Redefine all the POSIX functions.  We should never see a POSIX
 * function (or any other non-HDF5 function) in the source!
 */
#define HDabort()               abort()
#define HDabs(X)                abs(X)
#define HDaccess(F,M)           access(F, M)
#define HDacos(X)               acos(X)
#ifdef H5_HAVE_ALARM
#define HDalarm(N)              alarm(N)
#else /* H5_HAVE_ALARM */
#define HDalarm(N)              (0)
#endif /* H5_HAVE_ALARM */
#define HDasctime(T)            asctime(T)
#define HDasin(X)               asin(X)
#define HDassert(X)             assert(X)
#define HDatan(X)               atan(X)
#define HDatan2(X,Y)            atan2(X,Y)
#define HDatexit(F)             atexit(F)
#define HDatof(S)               atof(S)
#define HDatoi(S)               atoi(S)
#define HDatol(S)               atol(S)
#define HDBSDgettimeofday(S,P)  BSDgettimeofday(S,P)
#define HDbsearch(K,B,N,Z,F)    bsearch(K,B,N,Z,F)
#define HDcalloc(N,Z)           calloc(N,Z)
#define HDceil(X)               ceil(X)
#define HDcfgetispeed(T)        cfgetispeed(T)
#define HDcfgetospeed(T)        cfgetospeed(T)
#define HDcfsetispeed(T,S)      cfsetispeed(T,S)
#define HDcfsetospeed(T,S)      cfsetospeed(T,S)
#define HDchdir(S)              chdir(S)
#define HDchmod(S,M)            chmod(S,M)
#define HDchown(S,O,G)          chown(S,O,G)
#define HDclearerr(F)           clearerr(F)
#define HDclock()               clock()
#define HDclose(F)              close(F)
#define HDclosedir(D)           closedir(D)
#define HDcos(X)                cos(X)
#define HDcosh(X)               cosh(X)
#define HDcreat(S,M)            creat(S,M)
#define HDctermid(S)            ctermid(S)
#define HDctime(T)              ctime(T)
#define HDcuserid(S)            cuserid(S)
#ifdef H5_HAVE_DIFFTIME
#define HDdifftime(X,Y)         difftime(X,Y)
#else
#define HDdifftime(X,Y)         ((double)(X)-(double)(Y))
#endif
#define HDdiv(X,Y)              div(X,Y)
#define HDdup(F)                dup(F)
#define HDdup2(F,I)             dup2(F,I)
/* execl() variable arguments */
/* execle() variable arguments */
/* execlp() variable arguments */
#define HDexecv(S,AV)           execv(S,AV)
#define HDexecve(S,AV,E)        execve(S,AV,E)
#define HDexecvp(S,AV)          execvp(S,AV)
#define HDexit(N)               exit(N)
#define HD_exit(N)              _exit(N)
#define HDexp(X)                exp(X)
#define HDfabs(X)               fabs(X)
/* use ABS() because fabsf() fabsl() are not common yet. */
#define HDfabsf(X)              ABS(X)
#define HDfabsl(X)              ABS(X)
#define HDfclose(F)             fclose(F)
/* fcntl() variable arguments */
#define HDfdopen(N,S)           fdopen(N,S)
#define HDfeof(F)               feof(F)
#define HDferror(F)             ferror(F)
#define HDfflush(F)             fflush(F)
#define HDfgetc(F)              fgetc(F)
#define HDfgetpos(F,P)          fgetpos(F,P)
#define HDfgets(S,N,F)          fgets(S,N,F)
#ifdef _WIN32
#define HDfileno(F)             _fileno(F)
#else /* _WIN32 */
#define HDfileno(F)             fileno(F)
#endif /* _WIN32 */
#define HDfloor(X)              floor(X)
#define HDfmod(X,Y)             fmod(X,Y)
#define HDfopen(S,M)            fopen(S,M)
#define HDfork()                fork()
#define HDfpathconf(F,N)        fpathconf(F,N)
H5_DLL int HDfprintf (FILE *stream, const char *fmt, ...);
#define HDfputc(C,F)            fputc(C,F)
#define HDfputs(S,F)            fputs(S,F)
#define HDfread(M,Z,N,F)        fread(M,Z,N,F)
#define HDfree(M)               free(M)
#define HDfreopen(S,M,F)        freopen(S,M,F)
#define HDfrexp(X,N)            frexp(X,N)
/* Check for Cray-specific 'frexpf()' and 'frexpl()' routines */
#ifdef H5_HAVE_FREXPF
#define HDfrexpf(X,N)           frexpf(X,N)
#else /* H5_HAVE_FREXPF */
#define HDfrexpf(X,N)           frexp(X,N)
#endif /* H5_HAVE_FREXPF */
#ifdef H5_HAVE_FREXPL
#define HDfrexpl(X,N)           frexpl(X,N)
#else /* H5_HAVE_FREXPL */
#define HDfrexpl(X,N)           frexp(X,N)
#endif /* H5_HAVE_FREXPL */
/* fscanf() variable arguments */
#ifdef H5_HAVE_FSEEKO
     #define HDfseek(F,O,W)     fseeko(F,O,W)
#else
     #define HDfseek(F,O,W)     fseek(F,O,W)
#endif
#define HDfsetpos(F,P)          fsetpos(F,P)
/* definitions related to the file stat utilities.
 * Windows have its own function names.
 * For Unix, if off_t is not 64bit big, try use the pseudo-standard
 * xxx64 versions if available.
 */
#ifdef _WIN32
    #define HDfstat(F,B)        _fstati64(F,B)
    #define HDlstat(S,B)        _lstati64(S,B)
    #define HDstat(S,B)         _stati64(S,B)
    typedef struct _stati64     h5_stat_t;
    typedef __int64             h5_stat_size_t;
#elif H5_SIZEOF_OFF_T!=8 && H5_SIZEOF_OFF64_T==8 && defined(H5_HAVE_STAT64)
    #define HDfstat(F,B)        fstat64(F,B)
    #define HDlstat(S,B)        lstat64(S,B)
    #define HDstat(S,B)         stat64(S,B)
    typedef struct stat64       h5_stat_t;
    typedef off64_t             h5_stat_size_t;
#else
    #define HDfstat(F,B)        fstat(F,B)
    #define HDlstat(S,B)        lstat(S,B)
    #define HDstat(S,B)         stat(S,B)
    typedef struct stat         h5_stat_t;
    typedef off_t               h5_stat_size_t;
#endif

#define HDftell(F)              ftell(F)
#define HDftruncate(F,L)        ftruncate(F,L)
#define HDfwrite(M,Z,N,F)       fwrite(M,Z,N,F)
#define HDgetc(F)               getc(F)
#define HDgetchar()             getchar()
#define HDgetcwd(S,Z)           getcwd(S,Z)
#define HDgetegid()             getegid()
#define HDgetenv(S)             getenv(S)
#define HDgeteuid()             geteuid()
#define HDgetgid()              getgid()
#define HDgetgrgid(G)           getgrgid(G)
#define HDgetgrnam(S)           getgrnam(S)
#define HDgetgroups(Z,G)        getgroups(Z,G)
#define HDgetlogin()            getlogin()
#define HDgetpgrp()             getpgrp()
#define HDgetpid()              getpid()
#define HDgetppid()             getppid()
#define HDgetpwnam(S)           getpwnam(S)
#define HDgetpwuid(U)           getpwuid(U)
#define HDgetrusage(X,S)        getrusage(X,S)
#define HDgets(S)               gets(S)
#define HDgettimeofday(S,P)     gettimeofday(S,P)
#define HDgetuid()              getuid()
#define HDgmtime(T)             gmtime(T)
#define HDisalnum(C)            isalnum((int)(C)) /*cast for solaris warning*/
#define HDisalpha(C)            isalpha((int)(C)) /*cast for solaris warning*/
#define HDisatty(F)             isatty(F)
#define HDiscntrl(C)            iscntrl((int)(C)) /*cast for solaris warning*/
#define HDisdigit(C)            isdigit((int)(C)) /*cast for solaris warning*/
#define HDisgraph(C)            isgraph((int)(C)) /*cast for solaris warning*/
#define HDislower(C)            islower((int)(C)) /*cast for solaris warning*/
#define HDisprint(C)            isprint((int)(C)) /*cast for solaris warning*/
#define HDispunct(C)            ispunct((int)(C)) /*cast for solaris warning*/
#define HDisspace(C)            isspace((int)(C)) /*cast for solaris warning*/
#define HDisupper(C)            isupper((int)(C)) /*cast for solaris warning*/
#define HDisxdigit(C)           isxdigit((int)(C)) /*cast for solaris warning*/
#define HDkill(P,S)             kill(P,S)
#define HDlabs(X)               labs(X)
#define HDldexp(X,N)            ldexp(X,N)
#define HDldiv(X,Y)             ldiv(X,Y)
#define HDlink(OLD,NEW)         link(OLD,NEW)
#define HDlocaleconv()          localeconv()
#define HDlocaltime(T)          localtime(T)
#define HDlog(X)                log(X)
#define HDlog10(X)              log10(X)
#define HDlongjmp(J,N)          longjmp(J,N)
#ifdef _WIN32
    #define HDlseek(F,O,W)  _lseeki64(F,O,W)
#else
    #ifdef H5_HAVE_LSEEK64
        #define HDlseek(F,O,W)  lseek64(F,O,W)
    #else
        #define HDlseek(F,O,W)  lseek(F,O,W)
    #endif
#endif
#define HDmalloc(Z)             malloc(Z)
#define HDposix_memalign(P,A,Z) posix_memalign(P,A,Z)
#define HDmblen(S,N)            mblen(S,N)
#define HDmbstowcs(P,S,Z)       mbstowcs(P,S,Z)
#define HDmbtowc(P,S,Z)         mbtowc(P,S,Z)
#define HDmemchr(S,C,Z)         memchr(S,C,Z)
#define HDmemcmp(X,Y,Z)         memcmp(X,Y,Z)
/*
 * The (char*) casts are required for the DEC when optimizations are turned
 * on and the source and/or destination are not aligned.
 */
#define HDmemcpy(X,Y,Z)         memcpy((char*)(X),(const char*)(Y),Z)
#define HDmemmove(X,Y,Z)        memmove((char*)(X),(const char*)(Y),Z)
/*
 * The (void*) cast just avoids a compiler warning in _WIN32
 */
#ifdef _WIN32
#define HDmemset(X,C,Z)         memset((void*)(X),C,Z)
#else /* _WIN32 */
#define HDmemset(X,C,Z)         memset(X,C,Z)
#endif /* _WIN32 */
#ifdef _WIN32
#define HDmkdir(S,M)            _mkdir(S)
#else /* _WIN32 */
#define HDmkdir(S,M)            mkdir(S,M)
#endif /* _WIN32 */
#define HDmkfifo(S,M)           mkfifo(S,M)
#define HDmktime(T)             mktime(T)
#define HDmodf(X,Y)             modf(X,Y)
#ifdef _O_BINARY
#define HDopen(S,F,M)           open(S,F|_O_BINARY,M)
#else
#define HDopen(S,F,M)           open(S,F,M)
#endif
#define HDopendir(S)            opendir(S)
#define HDpathconf(S,N)         pathconf(S,N)
#define HDpause()               pause()
#define HDperror(S)             perror(S)
#define HDpipe(F)               pipe(F)
#define HDpow(X,Y)              pow(X,Y)
/* printf() variable arguments */
#define HDputc(C,F)             putc(C,F)
#define HDputchar(C)            putchar(C)
#define HDputs(S)               puts(S)
#define HDqsort(M,N,Z,F)        qsort(M,N,Z,F)
#define HDraise(N)              raise(N)

#ifdef H5_HAVE_RAND_R
#define HDrandom()              HDrand()
H5_DLL int HDrand(void);
#elif H5_HAVE_RANDOM
#define HDrand()                random()
#define HDrandom()              random()
#else
#define HDrand()                rand()
#define HDrandom()              rand()
#endif

#define HDread(F,M,Z)           read(F,M,Z)
#define HDreaddir(D)            readdir(D)
#define HDrealloc(M,Z)          realloc(M,Z)
#ifdef H5_VMS
#ifdef __cplusplus
extern "C" {
#endif
int HDremove_all(const char * fname);
#ifdef __cplusplus
}
#endif
#define HDremove(S)             HDremove_all(S)
#else
#define HDremove(S)             remove(S)
#endif /*H5_VMS*/
#define HDrename(OLD,NEW)       rename(OLD,NEW)
#define HDrewind(F)             rewind(F)
#define HDrewinddir(D)          rewinddir(D)
#define HDrmdir(S)              rmdir(S)
/* scanf() variable arguments */
#define HDsetbuf(F,S)           setbuf(F,S)
#define HDsetgid(G)             setgid(G)
#define HDsetjmp(J)             setjmp(J)
#define HDsetlocale(N,S)        setlocale(N,S)
#define HDsetpgid(P,PG)         setpgid(P,PG)
#define HDsetsid()              setsid()
#define HDsetuid(U)             setuid(U)
/* Windows does not permit setting the buffer size to values
   less than 2.  */
#ifndef _WIN32
#define HDsetvbuf(F,S,M,Z)      setvbuf(F,S,M,Z)
#else
#define HDsetvbuf(F,S,M,Z)  setvbuf(F,S,M,(Z>1?Z:2))
#endif
#define HDsigaddset(S,N)        sigaddset(S,N)
#define HDsigdelset(S,N)        sigdelset(S,N)
#define HDsigemptyset(S)        sigemptyset(S)
#define HDsigfillset(S)         sigfillset(S)
#define HDsigismember(S,N)      sigismember(S,N)
#define HDsiglongjmp(J,N)       siglongjmp(J,N)
#define HDsignal(N,F)           signal(N,F)
#define HDsigpending(S)         sigpending(S)
#define HDsigprocmask(H,S,O)    sigprocmask(H,S,O)
#define HDsigsetjmp(J,N)        sigsetjmp(J,N)
#define HDsigsuspend(S)         sigsuspend(S)
#define HDsin(X)                sin(X)
#define HDsinh(X)               sinh(X)
#define HDsleep(N)              sleep(N)
#ifdef _WIN32
#define HDsnprintf              _snprintf /*varargs*/
#else
#define HDsnprintf              snprintf /*varargs*/
#endif
/* sprintf() variable arguments */
#define HDsqrt(X)               sqrt(X)
#ifdef H5_HAVE_RAND_R
H5_DLL void HDsrand(unsigned int seed);
#define HDsrandom(S)            HDsrand(S)
#elif H5_HAVE_RANDOM
#define HDsrand(S)              srandom(S)
#define HDsrandom(S)            srandom(S)
#else
#define HDsrand(S)              srand(S)
#define HDsrandom(S)            srand(S)
#endif
/* sscanf() variable arguments */

#define HDstrcasecmp(X,Y)	strcasecmp(X,Y)
#define HDstrcat(X,Y)           strcat(X,Y)
#define HDstrchr(S,C)           strchr(S,C)
#define HDstrcmp(X,Y)           strcmp(X,Y)
#define HDstrcoll(X,Y)          strcoll(X,Y)
#define HDstrcpy(X,Y)           strcpy(X,Y)
#define HDstrcspn(X,Y)          strcspn(X,Y)
#define HDstrerror(N)           strerror(N)
#define HDstrftime(S,Z,F,T)     strftime(S,Z,F,T)
#define HDstrlen(S)             strlen(S)
#define HDstrncat(X,Y,Z)        strncat(X,Y,Z)
#define HDstrncmp(X,Y,Z)        strncmp(X,Y,Z)
#define HDstrncpy(X,Y,Z)        strncpy(X,Y,Z)
#define HDstrpbrk(X,Y)          strpbrk(X,Y)
#define HDstrrchr(S,C)          strrchr(S,C)
#define HDstrspn(X,Y)           strspn(X,Y)
#define HDstrstr(X,Y)           strstr(X,Y)
#define HDstrtod(S,R)           strtod(S,R)
#define HDstrtok(X,Y)           strtok(X,Y)
#define HDstrtol(S,R,N)         strtol(S,R,N)
H5_DLL int64_t HDstrtoll (const char *s, const char **rest, int base);
#define HDstrtoul(S,R,N)        strtoul(S,R,N)
#define HDstrxfrm(X,Y,Z)        strxfrm(X,Y,Z)
#define HDsysconf(N)            sysconf(N)
#define HDsystem(S)             system(S)
#define HDtan(X)                tan(X)
#define HDtanh(X)               tanh(X)
#define HDtcdrain(F)            tcdrain(F)
#define HDtcflow(F,A)           tcflow(F,A)
#define HDtcflush(F,N)          tcflush(F,N)
#define HDtcgetattr(F,T)        tcgetattr(F,T)
#define HDtcgetpgrp(F)          tcgetpgrp(F)
#define HDtcsendbreak(F,N)      tcsendbreak(F,N)
#define HDtcsetattr(F,O,T)      tcsetattr(F,O,T)
#define HDtcsetpgrp(F,N)        tcsetpgrp(F,N)
#define HDtime(T)               time(T)
#define HDtimes(T)              times(T)
#define HDtmpfile()             tmpfile()
#define HDtmpnam(S)             tmpnam(S)
#define HDtolower(C)            tolower(C)
#define HDtoupper(C)            toupper(C)
#define HDttyname(F)            ttyname(F)
#define HDtzset()               tzset()
#define HDumask(N)              umask(N)
#define HDuname(S)              uname(S)
#define HDungetc(C,F)           ungetc(C,F)
#ifdef _WIN32
#define HDunlink(S)             _unlink(S)
#else
#define HDunlink(S)             unlink(S)
#endif
#define HDutime(S,T)            utime(S,T)
#define HDva_arg(A,T)           va_arg(A,T)
#define HDva_end(A)             va_end(A)
#define HDva_start(A,P)         va_start(A,P)
#define HDvasprintf(RET,FMT,A)  vasprintf(RET,FMT,A)
#define HDvfprintf(F,FMT,A)     vfprintf(F,FMT,A)
#define HDvprintf(FMT,A)        vprintf(FMT,A)
#define HDvsprintf(S,FMT,A)     vsprintf(S,FMT,A)
#ifdef _WIN32
#   define HDvsnprintf(S,N,FMT,A) _vsnprintf(S,N,FMT,A)
#else
#   define HDvsnprintf(S,N,FMT,A) vsnprintf(S,N,FMT,A)
#endif
#define HDwait(W)               wait(W)
#define HDwaitpid(P,W,O)        waitpid(P,W,O)
#define HDwcstombs(S,P,Z)       wcstombs(S,P,Z)
#define HDwctomb(S,C)           wctomb(S,C)
#define HDwrite(F,M,Z)          write(F,M,Z)

/*
 * And now for a couple non-Posix functions...  Watch out for systems that
 * define these in terms of macros.
 */
#ifdef _WIN32
#define HDstrdup(S)    _strdup(S)
#else /* _WIN32 */

#if !defined strdup && !defined H5_HAVE_STRDUP
extern char *strdup(const char *s);
#endif

#define HDstrdup(S)     strdup(S)

#endif /* _WIN32 */

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE 1
#endif

/** From h5test.h **/

#ifdef H5_HAVE_PARALLEL
extern MPI_Info h5_io_info_g;         /* MPI INFO object for IO */
#endif

#ifdef H5_HAVE_PARALLEL
H5TEST_DLL int h5_set_info_object(void);
H5TEST_DLL void h5_dump_info_object(MPI_Info info);
#endif



/** From h5tools_utils.h **/

extern int         opt_err;     /* getoption prints errors if this is on    */
extern int         opt_ind;     /* token pointer                            */
extern const char *opt_arg;     /* flag argument (or value)                 */


enum {
    no_arg = 0,         /* doesn't take an argument     */
    require_arg,        /* requires an argument	        */
    optional_arg        /* argument is optional         */
};


typedef struct long_options {
    const char  *name;          /* name of the long option              */
    int          has_arg;       /* whether we should look for an arg    */
    char         shortval;      /* the shortname equivalent of long arg
                                 * this gets returned from get_option   */
} long_options;

extern int    get_option(int argc, const char **argv, const char *opt,
                         const struct long_options *l_opt);
#endif
