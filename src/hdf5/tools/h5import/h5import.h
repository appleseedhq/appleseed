/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

/*
 *
 * Data and structure definitions for h5import
 *
 */

#ifndef H5IMPORT_H__
#define H5IMPORT_H__

/*
 * state table tokens
 */
#define FILNAME 0
/* filename */
#define OPT_o   1
/* output filename */
#define OPT_c   2   /* configuration filename */
#define OPT_h   3   /* request for explanation */
#define OPT_d   4   /* dimensions */
#define OPT_p   5   /* pathname */
#define OPT_t   6   /* data type */
#define OPT_s   7   /* data size */
#define ERR    20  /* invalid token */

#define MAX_GROUPS_IN_PATH  20
#define MAX_PATH_NAME_LENGTH 255
#define NUM_KEYS 14
#define MIN_NUM_DIMENSION  1
#define MAX_NUM_DIMENSION  32
#define BASE_10 10

#define CHUNK            0
#define COMPRESS         1
#define EXTEND           2
#define EXTERNAL         3
#define DIM              4
#define RANK             5
#define PATH             6
#define INPUT_CLASS      7
#define INPUT_SIZE       8
#define OUTPUT_CLASS     9
#define OUTPUT_SIZE     10
#define OUTPUT_ARCH     11
#define OUTPUT_B_ORDER  12
#define COMPRESS_PARAM  13

/* data types */
#define H5DT_INT8      signed char
#define H5DT_INT16     short
#define H5DT_INT32     int
#define H5DT_FLOAT32   float
#define H5DT_FLOAT64   double
#define VOIDP          void*
#define H5DT_UINT8     unsigned char
#define H5DT_UINT16    unsigned short
#define H5DT_UINT32    unsigned int
#define H5DT_INT64     long long
#define H5DT_UINT64    unsigned H5DT_INT64

struct path_info
{
  char group[MAX_GROUPS_IN_PATH][MAX_PATH_NAME_LENGTH];
  int count;
};

struct Input
{
  struct path_info path;
  int inputClass;
  int inputSize;
  int rank;
  hsize_t* sizeOfDimension;
  int outputClass;
  int outputSize;
  int outputArchitecture;
  int outputByteOrder;
  hsize_t* sizeOfChunk;
  hsize_t* maxsizeOfDimension;
  int compressionType;
  int compressionParam;
  char *externFilename;
  VOIDP data;
  int configOptionVector[NUM_KEYS];
};

struct infilesformat
{
  char datafile[255];
  char configfile[255];
  struct Input in;
  int config; /* Configfile present? No - 0. Yes - 1 */
};

struct Options
{
  struct infilesformat  infiles[30];  /* structure to hold the list of input file names. Limited to 30*/
  char   outfile[256];  /* output file name */
  int    fcount;       /* number of input files */
};

char keytable[NUM_KEYS][30] = {
  "PATH",
  "INPUT-CLASS",
  "INPUT-SIZE",
  "RANK",
  "DIMENSION-SIZES",
  "OUTPUT-CLASS",
  "OUTPUT-SIZE",
  "OUTPUT-ARCHITECTURE",
  "OUTPUT-BYTE-ORDER",
  "CHUNKED-DIMENSION-SIZES",
  "COMPRESSION-TYPE",
  "COMPRESSION-PARAM",
  "EXTERNAL-STORAGE",
  "MAXIMUM-DIMENSIONS"
};

static int  state_table[15][8] =
{
  /* token ordering:
     FILNAME      OPT_o   OPT_c  OPT_h  OPT_d  OPT_p  OPT_t  OPT_s   */

  /* state 0: start */
  {1, ERR, ERR, 6, ERR, ERR, ERR, ERR},

  /* state 1: input files */
  {ERR, ERR, 2, ERR, 7, ERR, ERR, ERR},

  /* state 2: -c[onfigfile] */
  {3, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 3: configfile */
  {1, 4, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 4: -o[utfile] */
  {5, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 5: outfile */
  {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 6: -h[elp] */
  {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 7: -d[ims] */
  {8, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 8: dimensions */
  {1, 4, ERR, ERR, ERR, 9, 11, 13},

  /* state 9: -p[ath] */
  {10, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 10: path name */
  {1, 4, ERR, ERR, ERR, ERR, 11, 13},

  /* state 11: -t[ype] */
  {12, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 12: data type */
  {1, 4, ERR, ERR, ERR, ERR, ERR, 13},

  /* state 13: -s[ize] */
  {14, ERR, ERR, ERR, ERR, ERR, ERR, ERR},

  /* state 14: data size */
  {1, 4, ERR, ERR, ERR, ERR, ERR, ERR}

};

/*
 *
 *  Function declarations for h5import
 *
 */
void  usage(char *);
void  setDefaultValues(struct Input *in, int count);
void  help(char *);

static int  gtoken(char *s);
static int  process(struct Options *opt);
static int  processConfigurationFile(char *infile, struct Input *in, FILE **strm);
static int  mapKeywordToIndex(char *key);
static int  parsePathInfo(struct path_info *path, char *strm);
static int  parseDimensions(struct Input *in, char *strm);
static int  getInputSize(struct Input *in, int ival);
static int  getInputClass(struct Input *in, char * strm);
static int  InputClassStrToInt(char *temp);
static int  getRank(struct Input *in, FILE** strm);
static int  getDimensionSizes(struct Input *in, FILE** strm);
static int  getOutputSize(struct Input *in, FILE** strm);
static int  getOutputClass(struct Input *in, FILE** strm);
static int  OutputClassStrToInt(char *temp);
static int  getOutputArchitecture(struct Input *in, FILE** strm);
static int  OutputArchStrToInt(char *temp);
static int  getOutputByteOrder(struct Input *in, FILE** strm);
static int  OutputByteOrderStrToInt(char *temp);
static int  getChunkedDimensionSizes(struct Input *in, FILE **strm);
static int  getCompressionType(struct Input *in, FILE** strm);
static int  CompressionTypeStrToInt(char *temp);
static int  getCompressionParameter(struct Input *in, FILE** strm);
static int  getExternalFilename(struct Input *in, FILE** strm);
static int  getMaximumDimensionSizes(struct Input *in, FILE **strm);
static int  processDataFile(char *infile, struct Input *in, FILE **strm, hid_t file_id);
static int  readIntegerData(FILE **strm, struct Input *in);
static int  readFloatData(FILE **strm, struct Input *in);
static int  allocateIntegerStorage(struct Input *in);
static int  allocateFloatStorage(struct Input *in);
hid_t       createOutputDataType(struct Input *in);
hid_t       createInputDataType(struct Input *in);
static int  readUIntegerData(FILE **strm, struct Input *in);
static int  allocateUIntegerStorage(struct Input *in);
static int  validateConfigurationParameters(struct Input * in);
static int  processStrData(FILE **strm, struct Input *in, hid_t file_id);

#endif  /* H5IMPORT_H__ */

