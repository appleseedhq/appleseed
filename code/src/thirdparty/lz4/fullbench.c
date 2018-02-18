/*
    bench.c - Demo program to benchmark open-source compression algorithm
    Copyright (C) Yann Collet 2012-2013
    GPL v2 License

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

    You can contact the author at :
    - LZ4 homepage : http://fastcompression.blogspot.com/p/lz4.html
    - LZ4 source repository : http://code.google.com/p/lz4/
*/

//**************************************
// Compiler Options
//**************************************
// Disable some Visual warning messages
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE     // VS2005

// Unix Large Files support (>4GB)
#if (defined(__sun__) && (!defined(__LP64__)))   // Sun Solaris 32-bits requires specific definitions
#  define _LARGEFILE_SOURCE 
#  define _FILE_OFFSET_BITS 64
#elif ! defined(__LP64__)                        // No point defining Large file for 64 bit
#  define _LARGEFILE64_SOURCE
#endif

// S_ISREG & gettimeofday() are not supported by MSVC
#if defined(_MSC_VER)
#  define S_ISREG(x) (((x) & S_IFMT) == S_IFREG)
#  define BMK_LEGACY_TIMER 1
#endif

// GCC does not support _rotl outside of Windows
#if !defined(_WIN32)
#  define _rotl(x,r) ((x << r) | (x >> (32 - r)))
#endif


//**************************************
// Includes
//**************************************
#include <stdlib.h>      // malloc
#include <stdio.h>       // fprintf, fopen, ftello64
#include <sys/types.h>   // stat64
#include <sys/stat.h>    // stat64

// Use ftime() if gettimeofday() is not available on your target
#if defined(BMK_LEGACY_TIMER)
#  include <sys/timeb.h>   // timeb, ftime
#else
#  include <sys/time.h>    // gettimeofday
#endif

#include "lz4.h"
#define COMPRESSOR0 LZ4_compress
#include "lz4hc.h"
#define COMPRESSOR1 LZ4_compressHC
#define DEFAULTCOMPRESSOR COMPRESSOR0

#include "xxhash.h"


//**************************************
// Basic Types
//**************************************
#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L   // C99
# include <stdint.h>
  typedef uint8_t  BYTE;
  typedef uint16_t U16;
  typedef uint32_t U32;
  typedef  int32_t S32;
  typedef uint64_t U64;
#else
  typedef unsigned char       BYTE;
  typedef unsigned short      U16;
  typedef unsigned int        U32;
  typedef   signed int        S32;
  typedef unsigned long long  U64;
#endif


//****************************
// Constants
//****************************
#define COMPRESSOR_NAME "Full LZ4 speed analyzer"
#define COMPRESSOR_VERSION ""
#define COMPILED __DATE__
#define AUTHOR "Yann Collet"
#define WELCOME_MESSAGE "*** %s %s, by %s (%s) ***\n", COMPRESSOR_NAME, COMPRESSOR_VERSION, AUTHOR, COMPILED

#define NBLOOPS    6
#define TIMELOOP   2500

#define KNUTH      2654435761U
#define MAX_MEM    (1984<<20)
#define DEFAULT_CHUNKSIZE   (4<<20)


//**************************************
// Local structures
//**************************************
struct chunkParameters
{
    U32   id;
    char* origBuffer;
    char* compressedBuffer;
    int   origSize;
    int   compressedSize;
};


//**************************************
// MACRO
//**************************************
#define DISPLAY(...) fprintf(stderr, __VA_ARGS__)



//**************************************
// Benchmark Parameters
//**************************************
static int chunkSize = DEFAULT_CHUNKSIZE;
static int nbIterations = NBLOOPS;
static int BMK_pause = 0;
static int compressionTest = 1;
static int decompressionTest = 1;

void BMK_SetBlocksize(int bsize)
{
    chunkSize = bsize;
    DISPLAY("-Using Block Size of %i KB-\n", chunkSize>>10);
}

void BMK_SetNbIterations(int nbLoops)
{
    nbIterations = nbLoops;
    DISPLAY("- %i iterations -\n", nbIterations);
}

void BMK_SetPause()
{
    BMK_pause = 1;
}

//*********************************************************
//  Private functions
//*********************************************************

#if defined(BMK_LEGACY_TIMER)

static int BMK_GetMilliStart()
{
  // Based on Legacy ftime()
  // Rolls over every ~ 12.1 days (0x100000/24/60/60)
  // Use GetMilliSpan to correct for rollover
  struct timeb tb;
  int nCount;
  ftime( &tb );
  nCount = (int) (tb.millitm + (tb.time & 0xfffff) * 1000);
  return nCount;
}

#else

static int BMK_GetMilliStart()
{
  // Based on newer gettimeofday()
  // Use GetMilliSpan to correct for rollover
  struct timeval tv;
  int nCount;
  gettimeofday(&tv, NULL);
  nCount = (int) (tv.tv_usec/1000 + (tv.tv_sec & 0xfffff) * 1000);
  return nCount;
}

#endif


static int BMK_GetMilliSpan( int nTimeStart )
{
  int nSpan = BMK_GetMilliStart() - nTimeStart;
  if ( nSpan < 0 )
    nSpan += 0x100000 * 1000;
  return nSpan;
}


static size_t BMK_findMaxMem(U64 requiredMem)
{
    size_t step = (64U<<20);   // 64 MB
    BYTE* testmem=NULL;

    requiredMem = (((requiredMem >> 25) + 1) << 26);
    if (requiredMem > MAX_MEM) requiredMem = MAX_MEM;

    requiredMem += 2*step;
    while (!testmem)
    {
        requiredMem -= step;
        testmem = (BYTE*) malloc ((size_t)requiredMem);
    }

    free (testmem);
    return (size_t) (requiredMem - step);
}


static U64 BMK_GetFileSize(char* infilename)
{
    int r;
#if defined(_MSC_VER)
    struct _stat64 statbuf;
    r = _stat64(infilename, &statbuf);
#else
    struct stat statbuf;
    r = stat(infilename, &statbuf);
#endif
    if (r || !S_ISREG(statbuf.st_mode)) return 0;   // No good...
    return (U64)statbuf.st_size;
}


//*********************************************************
//  Public function
//*********************************************************

static inline int local_LZ4_compress_limitedOutput(const char* in, char* out, int inSize)
{
    return LZ4_compress_limitedOutput(in, out, inSize, LZ4_compressBound(inSize));
}

static inline int local_LZ4_compressHC_limitedOutput(const char* in, char* out, int inSize)
{
    return LZ4_compressHC_limitedOutput(in, out, inSize, LZ4_compressBound(inSize));
}

static inline int local_LZ4_decompress_fast(const char* in, char* out, int inSize, int outSize)
{
    (void)inSize;
    LZ4_decompress_fast(in, out, outSize);
    return outSize;
}

static inline int local_LZ4_decompress_fast_withPrefix64k(const char* in, char* out, int inSize, int outSize)
{
    (void)inSize;
    LZ4_decompress_fast_withPrefix64k(in, out, outSize);
    return outSize;
}

static inline int local_LZ4_decompress_safe_partial(const char* in, char* out, int inSize, int outSize)
{
    return LZ4_decompress_safe_partial(in, out, inSize, outSize - 5, outSize);
}

int fullSpeedBench(char** fileNamesTable, int nbFiles)
{
  int fileIdx=0;
  FILE* fileIn;
  char* infilename;
  U64 largefilesize;
  size_t benchedSize;
  int nbChunks;
  int maxCChunkSize;
  size_t readSize;
  char* orig_buff;
  char* compressed_buff; int compressed_buff_size;
  struct chunkParameters* chunkP;
  U32 crcc, crcd=0;
# define NB_COMPRESSION_ALGORITHMS 4
  static char* compressionNames[] = { "LZ4_compress", "LZ4_compress_limitedOutput", "LZ4_compressHC", "LZ4_compressHC_limitedOutput" };
  double totalCTime[NB_COMPRESSION_ALGORITHMS] = {0};
  double totalCSize[NB_COMPRESSION_ALGORITHMS] = {0};
# define NB_DECOMPRESSION_ALGORITHMS 5
  static char* decompressionNames[] = { "LZ4_decompress_fast", "LZ4_decompress_fast_withPrefix64k", "LZ4_decompress_safe", "LZ4_decompress_safe_withPrefix64k", "LZ4_decompress_safe_partial" };
  double totalDTime[NB_DECOMPRESSION_ALGORITHMS] = {0};

  U64 totals = 0;


  // Loop for each file
  while (fileIdx<nbFiles)
  {
      // Check file existence
      infilename = fileNamesTable[fileIdx++];
      fileIn = fopen( infilename, "rb" );
      if (fileIn==NULL)
      {
        DISPLAY( "Pb opening %s\n", infilename);
        return 11;
      }

      // Memory allocation & restrictions
      largefilesize = BMK_GetFileSize(infilename);
      benchedSize = (size_t) BMK_findMaxMem(largefilesize) / 2;
      if ((U64)benchedSize > largefilesize) benchedSize = (size_t)largefilesize;
      if (benchedSize < largefilesize)
      {
          DISPLAY("Not enough memory for '%s' full size; testing %i MB only...\n", infilename, (int)(benchedSize>>20));
      }

      // Alloc
      chunkP = (struct chunkParameters*) malloc(((benchedSize / chunkSize)+1) * sizeof(struct chunkParameters));
      orig_buff = (char*) malloc((size_t)benchedSize);
      nbChunks = (int) (benchedSize / chunkSize) + 1;
      maxCChunkSize = LZ4_compressBound(chunkSize);
      compressed_buff_size = nbChunks * maxCChunkSize;
      compressed_buff = (char*)malloc((size_t)compressed_buff_size);


      if(!orig_buff || !compressed_buff)
      {
        DISPLAY("\nError: not enough memory!\n");
        free(orig_buff);
        free(compressed_buff);
        fclose(fileIn);
        return 12;
      }

      // Init chunks data
      {
          int i;
          size_t remaining = benchedSize;
          char* in = orig_buff;
          char* out = compressed_buff;
          for (i=0; i<nbChunks; i++)
          {
              chunkP[i].id = i;
              chunkP[i].origBuffer = in; in += chunkSize;
              if ((int)remaining > chunkSize) { chunkP[i].origSize = chunkSize; remaining -= chunkSize; } else { chunkP[i].origSize = (int)remaining; remaining = 0; }
              chunkP[i].compressedBuffer = out; out += maxCChunkSize;
              chunkP[i].compressedSize = 0;
          }
      }

      // Fill input buffer
      DISPLAY("Loading %s...       \r", infilename);
      readSize = fread(orig_buff, 1, benchedSize, fileIn);
      fclose(fileIn);

      if(readSize != benchedSize)
      {
        DISPLAY("\nError: problem reading file '%s' !!    \n", infilename);
        free(orig_buff);
        free(compressed_buff);
        return 13;
      }

      // Calculating input Checksum
      crcc = XXH32(orig_buff, (unsigned int)benchedSize,0);


      // Bench
      {
        int loopNb, nb_loops, chunkNb, cAlgNb, dAlgNb;
        size_t cSize=0;
        double ratio=0.;

        DISPLAY("\r%79s\r", "");
        DISPLAY(" %s : \n", infilename);

        // Compression Algorithms
        for (cAlgNb=0; (cAlgNb < NB_COMPRESSION_ALGORITHMS) && (compressionTest); cAlgNb++)
        {
            char* cName = compressionNames[cAlgNb];
            int (*compressionFunction)(const char*, char*, int);
            double bestTime = 100000000.;

            switch(cAlgNb)
            {
            case 0: compressionFunction = LZ4_compress; break;
            case 1: compressionFunction = local_LZ4_compress_limitedOutput; break;
            case 2: compressionFunction = LZ4_compressHC; break;
            case 3: compressionFunction = local_LZ4_compressHC_limitedOutput; break;
            default : DISPLAY("ERROR ! Bad algorithm Id !! \n"); return 1;
            }

            for (loopNb = 1; loopNb <= nbIterations; loopNb++)
            {
                double averageTime;
                int milliTime;

                DISPLAY("%1i-%-19.19s : %9i ->\r", loopNb, cName, (int)benchedSize);
                { size_t i; for (i=0; i<benchedSize; i++) compressed_buff[i]=(char)i; }     // warmimg up memory

                nb_loops = 0;
                milliTime = BMK_GetMilliStart();
                while(BMK_GetMilliStart() == milliTime);
                milliTime = BMK_GetMilliStart();
                while(BMK_GetMilliSpan(milliTime) < TIMELOOP)
                {
                    for (chunkNb=0; chunkNb<nbChunks; chunkNb++)
                    {
                        chunkP[chunkNb].compressedSize = compressionFunction(chunkP[chunkNb].origBuffer, chunkP[chunkNb].compressedBuffer, chunkP[chunkNb].origSize);
                        if (chunkP[chunkNb].compressedSize==0) DISPLAY("ERROR ! %s() = 0 !! \n", cName), exit(1);
                    }
                    nb_loops++;
                }
                milliTime = BMK_GetMilliSpan(milliTime);

                averageTime = (double)milliTime / nb_loops;
                if (averageTime < bestTime) bestTime = averageTime;
                cSize=0; for (chunkNb=0; chunkNb<nbChunks; chunkNb++) cSize += chunkP[chunkNb].compressedSize;
                ratio = (double)cSize/(double)benchedSize*100.;
                DISPLAY("%1i-%-19.19s : %9i -> %9i (%5.2f%%),%7.1f MB/s\r", loopNb, cName, (int)benchedSize, (int)cSize, ratio, (double)benchedSize / bestTime / 1000.);
            }

            if (ratio<100.)
                DISPLAY("%-21.21s : %9i -> %9i (%5.2f%%),%7.1f MB/s\n", cName, (int)benchedSize, (int)cSize, ratio, (double)benchedSize / bestTime / 1000.);
            else
                DISPLAY("%-21.21s : %9i -> %9i (%5.1f%%),%7.1f MB/s\n", cName, (int)benchedSize, (int)cSize, ratio, (double)benchedSize / bestTime / 1000.);

            totalCTime[cAlgNb] += bestTime;
            totalCSize[cAlgNb] += cSize;
        }

        // Prepare layout for decompression
        for (chunkNb=0; chunkNb<nbChunks; chunkNb++)
        {
            chunkP[chunkNb].compressedSize = LZ4_compress(chunkP[chunkNb].origBuffer, chunkP[chunkNb].compressedBuffer, chunkP[chunkNb].origSize);
            if (chunkP[chunkNb].compressedSize==0) DISPLAY("ERROR ! %s() = 0 !! \n", compressionNames[0]), exit(1);
        }
        { size_t i; for (i=0; i<benchedSize; i++) orig_buff[i]=0; }     // zeroing source area, for CRC checking

        // Decompression Algorithms
        for (dAlgNb=0; (dAlgNb < NB_DECOMPRESSION_ALGORITHMS) && (decompressionTest); dAlgNb++)
        {
            char* dName = decompressionNames[dAlgNb];
            int (*decompressionFunction)(const char*, char*, int, int);
            double bestTime = 100000000.;

            switch(dAlgNb)
            {
            case 0: decompressionFunction = local_LZ4_decompress_fast; break;
            case 1: decompressionFunction = local_LZ4_decompress_fast_withPrefix64k; break;
            case 2: decompressionFunction = LZ4_decompress_safe; break;
            case 3: decompressionFunction = LZ4_decompress_safe_withPrefix64k; break;
            case 4: decompressionFunction = local_LZ4_decompress_safe_partial; break;
            default : DISPLAY("ERROR ! Bad algorithm Id !! \n"); return 1;
            }

            for (loopNb = 1; loopNb <= nbIterations; loopNb++)
            {
                double averageTime;
                int milliTime;

                DISPLAY("%1i-%-19.19s : %9i ->\r", loopNb, dName, (int)benchedSize);

                nb_loops = 0;
                milliTime = BMK_GetMilliStart();
                while(BMK_GetMilliStart() == milliTime);
                milliTime = BMK_GetMilliStart();
                while(BMK_GetMilliSpan(milliTime) < TIMELOOP)
                {
                    for (chunkNb=0; chunkNb<nbChunks; chunkNb++)
                    {
                        int decodedSize = decompressionFunction(chunkP[chunkNb].compressedBuffer, chunkP[chunkNb].origBuffer, chunkP[chunkNb].compressedSize, chunkP[chunkNb].origSize);
                        if (chunkP[chunkNb].origSize != decodedSize) DISPLAY("ERROR ! %s() == %i != %i !! \n", dName, decodedSize, chunkP[chunkNb].origSize), exit(1);
                    }
                    nb_loops++;
                }
                milliTime = BMK_GetMilliSpan(milliTime);

                averageTime = (double)milliTime / nb_loops;
                if (averageTime < bestTime) bestTime = averageTime;

                DISPLAY("%1i-%-19.19s : %9i -> %7.1f MB/s\r", loopNb, dName, (int)benchedSize, (double)benchedSize / bestTime / 1000.);
            }

            // CRC Checking
            crcd = XXH32(orig_buff, (int)benchedSize, 0);
            if (crcc!=crcd) { DISPLAY("\n!!! WARNING !!! %14s : Invalid Checksum : %x != %x\n", infilename, (unsigned)crcc, (unsigned)crcd); exit(1); }
            DISPLAY("%-21.21s : %9i -> %7.1f MB/s\n", dName, (int)benchedSize, (double)benchedSize / bestTime / 1000.);

            totalDTime[dAlgNb] += bestTime;
        }

        totals += benchedSize;
      }

      free(orig_buff);
      free(compressed_buff);
      free(chunkP);
  }

  if (nbFiles > 1)
  {
      int AlgNb;

      DISPLAY(" ** TOTAL ** : \n");
      for (AlgNb = 0; (AlgNb < NB_COMPRESSION_ALGORITHMS) && (compressionTest); AlgNb ++)
      {
          char* cName = compressionNames[AlgNb];
          DISPLAY("%-21.21s :%10llu ->%10llu (%5.2f%%), %6.1f MB/s\n", cName, (long long unsigned int)totals, (long long unsigned int)totalCSize[AlgNb], (double)totalCSize[AlgNb]/(double)totals*100., (double)totals/totalCTime[AlgNb]/1000.);
      }
      for (AlgNb = 0; (AlgNb < NB_DECOMPRESSION_ALGORITHMS) && (decompressionTest); AlgNb ++)
      {
          char* dName = decompressionNames[AlgNb];
          DISPLAY("%-21.21s :%10llu -> %6.1f MB/s\n", dName, (long long unsigned int)totals, (double)totals/totalDTime[AlgNb]/1000.);
      }
  }

  if (BMK_pause) { printf("press enter...\n"); getchar(); }

  return 0;
}


int usage(char* exename)
{
    DISPLAY( "Usage :\n");
    DISPLAY( "      %s [arg] file1 file2 ... fileX\n", exename);
    DISPLAY( "Arguments :\n");
    DISPLAY( " -c     : compression tests only\n");
    DISPLAY( " -d     : decompression tests only\n");
    DISPLAY( " -H     : Help (this text + advanced options)\n");
    return 0;
}

int usage_advanced()
{
    DISPLAY( "\nAdvanced options :\n");
    DISPLAY( " -B#    : Block size [4-7](default : 7)\n");
    //DISPLAY( " -BD    : Block dependency (improve compression ratio)\n");
    DISPLAY( " -i#    : iteration loops [1-9](default : 6)\n");
    return 0;
}

int badusage(char* exename)
{
    DISPLAY("Wrong parameters\n");
    usage(exename);
    return 0;
}

int main(int argc, char** argv)
{
    int i,
        filenamesStart=2;
    char* exename=argv[0];
    char* input_filename=0;

    // Welcome message
    DISPLAY( WELCOME_MESSAGE);

    if (argc<2) { badusage(exename); return 1; }

    for(i=1; i<argc; i++)
    {
        char* argument = argv[i];

        if(!argument) continue;   // Protection if argument empty

        // Decode command (note : aggregated commands are allowed)
        if (argument[0]=='-')
        {
            while (argument[1]!=0)
            {
                argument ++;

                switch(argument[0])
                {
                    // Select compression algorithm only
                case 'c': decompressionTest = 0; break;

                    // Select decompression algorithm only
                case 'd': compressionTest = 0; break;

                    // Display help on usage
                case 'H': usage(exename); usage_advanced(); return 0;

                    // Modify Block Properties
                case 'B':
                    while (argument[1]!=0)
                    switch(argument[1])
                    {
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    { 
                        int B = argument[1] - '0'; 
                        int S = 1 << (8 + 2*B); 
                        BMK_SetBlocksize(S); 
                        argument++;
                        break;
                    }
                    case 'D': argument++; break;
                    default : goto _exit_blockProperties;
                    }
_exit_blockProperties:
                    break;

                    // Modify Nb Iterations
                case 'i': 
                    if ((argument[1] >='1') && (argument[1] <='9'))
                    {
                        int iters = argument[1] - '0'; 
                        BMK_SetNbIterations(iters); 
                        argument++;
                    }
                    break;

                    // Pause at the end (hidden option)
                case 'p': BMK_SetPause(); break;

                    // Unrecognised command
                default : badusage(exename); return 1;
                }
            }
            continue;
        }

        // first provided filename is input
        if (!input_filename) { input_filename=argument; filenamesStart=i; continue; }

    }

    // No input filename ==> Error
    if(!input_filename) { badusage(exename); return 1; }

    return fullSpeedBench(argv+filenamesStart, argc-filenamesStart);

}

