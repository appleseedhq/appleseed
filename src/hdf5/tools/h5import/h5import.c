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

#include "hdf5.h"
#include "H5private.h"
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "h5import.h"
#include "h5tools_utils.h"


/* Name of tool */
#define PROGRAMNAME "h5import"

int main(int argc, char *argv[])
{
  struct  Options opt;
  int    outfile_named = FALSE;
  int    token;
  int    i;
  int    state = 0;
  struct Input *in=NULL;

    const char *err1 = "Invalid number of arguments:  %d.\n";
    const char *err2 = "Error in state table.\n";
    const char *err3 = "No output file given.\n";
    const char *err4 = "Program aborted.\n";
    const char *err5 = "Invalid path %s.\n";
    const char *err6 = "Invalid dimensions - %s.\n";
    const char *err7 = "Invalid type of data - %s.\n";
    const char *err8 = "Invalid size of data - %s.\n";
    const char *err9 = "Cannot specify more than 30 input files in one call to h5import.\n";

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

   (void)HDsetvbuf(stderr, (char *) NULL, _IOLBF, 0);
   (void)HDsetvbuf(stdout, (char *) NULL, _IOLBF, 0);

    if ( argv[1] && (strcmp("-V",argv[1])==0) )
    {
        print_version("h5import");
        exit(EXIT_SUCCESS);

    }

 /*
  * validate the number of command line arguments
  */

  if (argc < 2)
  {
    (void) fprintf(stderr, err1, argc);
    usage(argv[0]);
    goto err;
  }

  /* Initialize the file count to 0 */
  opt.fcount = 0;

 /*
  * parse the command line
  */

  for (i = 1; i < argc; i++)
  {
    if ((token = gtoken(argv[i])) == ERR)
    {
      usage(argv[0]);
      goto err;
    }

    state = state_table[state][token];

    switch (state)
    {

      case 1: /* counting input files */
				if (opt.fcount < 29) {
	        (void) HDstrcpy(opt.infiles[opt.fcount].datafile, argv[i]);
					in = &(opt.infiles[opt.fcount].in);
					opt.infiles[opt.fcount].config = 0;
					setDefaultValues(in, opt.fcount);
	        opt.fcount++;
				}
				else {
					(void) fprintf(stderr, err9, argv[i]);
					goto err;
				}

      break;

      case 2: /* -c found; look for configfile */
      break;

      case 3: /* get configfile name */
        (void) HDstrcpy(opt.infiles[opt.fcount-1].configfile, argv[i]);
	opt.infiles[opt.fcount-1].config = 1;
      break;

      case 4: /* -o found; look for outfile */
      break;

      case 5: /* get outfile found */
        (void) HDstrcpy(opt.outfile, argv[i]);
        outfile_named = TRUE;
      break;

      case 6:    /* -h found; help, then exit */
        help(argv[0]);
        exit(EXIT_SUCCESS);
      break;

      case 7:    /* -d found; look for dimensions */
      break;

      case 8:    /* read dimensions */
	if (parseDimensions(in, argv[i]) == -1)
        {
          (void) fprintf(stderr, err6, argv[i]);
					goto err;
        }
      break;

      case 9:    /* -p found; look for path name */
      break;

      case 10:    /* read path name */
 	if (parsePathInfo(&in->path, argv[i]) == -1)
        {
          (void) fprintf(stderr, err5, argv[i]);
					goto err;
        }
      break;

      case 11:    /* -t found; look for data type */
      break;

      case 12:    /* read data type */
        if (getInputClass(in, argv[i]) == -1)
        {
          (void) fprintf(stderr, err7, argv[i]);
					goto err;
        }

        if (in->inputClass == 0 || in->inputClass == 4)
            in->outputClass = 0;
        if (in->inputClass == 1 || in->inputClass == 2 || in->inputClass == 3)
            in->outputClass = 1;
        if (in->inputClass == 6 || in->inputClass == 7)
            in->outputClass = 2;
      break;

      case 13:    /* -s found; look for data size */
      break;

      case 14:    /* read data size */
        if (getInputSize(in, (int)HDstrtol(argv[i], NULL, BASE_10)) == -1)
        {
          (void) fprintf(stderr, err8, argv[i]);
					goto err;
        }
        /*set default value for output-size */
        in->outputSize = in->inputSize;
      break;

      case ERR:   /* command syntax error */
      default:
        (void) fprintf(stderr, "%s", err2);
        usage(argv[0]);
        goto err;
    }
  }

  if (FALSE == outfile_named)
  {
    (void) fprintf(stderr, "%s", err3);
    usage(argv[0]);
    goto err;
  }

  if (process(&opt) == -1)
    goto err;

  return(EXIT_SUCCESS);
  err:
    (void) fprintf(stderr, "%s", err4);
    return(EXIT_FAILURE);
}

static int
gtoken(char *s)
{
  size_t      len;
  int         token;

  const char *err1 = "Illegal argument: %s.\n";

  /*
   * identify the token type
   */
  if (s[0] == '-')
  {     /* option name (or negative number) */
    token = ERR;
    len = HDstrlen(&s[1]);
    switch (s[1])
    {
      case 'o':
        if (!HDstrncmp("outfile", &s[1], len))
          token = OPT_o;
      break;

      case 'c':
        if (!HDstrncmp("config", &s[1], len))
          token = OPT_c;
      break;

      case 'h':
        if (!HDstrncmp("help", &s[1], len))
          token = OPT_h;
      break;

      case 'd':
        if (!HDstrncmp("dims", &s[1], len))
          token = OPT_d;
      break;

      case 'p':
        if (!HDstrncmp("path", &s[1], len))
          token = OPT_p;
      break;

      case 't':
        if (!HDstrncmp("type", &s[1], len))
          token = OPT_t;
      break;

      case 's':
        if (!HDstrncmp("size", &s[1], len))
          token = OPT_s;
      break;
    }

    if (token == ERR)
      (void) fprintf(stderr, err1, s);
  }
  else    /* filename */
  {
    token = FILNAME;
  }
  return (token);
}



/*-------------------------------------------------------------------------
 * Function:    processDataFile
 *
 * Purpose:     allocate memory and read data file
 *
 * Return:      0, success, -1, error
 *
 * Programmer:  pkmat
 *
 * Modifications: pvn
 *  7/23/2007. Added support for STR type, extra parameter FILE_ID
 *
 *-------------------------------------------------------------------------
 */

static int
processDataFile(char *infile, struct Input *in, FILE **strm, hid_t file_id)
{
  const char *err1 = "Unable to open the input file  %s for reading.\n";
  const char *err2 = "Error in allocating integer data storage.\n";
  const char *err3 = "Error in allocating floating-point data storage.\n";
  const char *err4 = "Error in reading integer data.\n";
  const char *err5 = "Error in reading floating-point data.\n";
  const char *err6 = "Error in allocating unsigned integer data storage.\n";
  const char *err7 = "Error in reading unsigned integer data.\n";
  const char *err10 = "Unrecognized input class type.\n";
  const char *err11 = "Error in reading string data.\n";

 /*-------------------------------------------------------------------------
  * special case for opening binary classes in WIN32
  * "FP" denotes a floating point binary file,
  * "IN" denotes a signed integer binary file,
  * "UIN" denotes an unsigned integer binary file,
  *-------------------------------------------------------------------------
  */
  if ( in->inputClass == 4 /* "IN" */ ||
       in->inputClass == 3 /* "FP" */ ||
       in->inputClass == 7 /* "UIN" */

      )
  {

#ifdef WIN32

      if ((*strm = fopen(infile, "rb")) == NULL)
      {
          (void) fprintf(stderr, err1, infile);
          return(-1);
      }
#else

      if ((*strm = fopen(infile, "r")) == NULL)
      {
          (void) fprintf(stderr, err1, infile);
          return(-1);
      }

#endif

  }
 /*-------------------------------------------------------------------------
  * if the input class is not binary, just use "r"
  *-------------------------------------------------------------------------
  */
  else
  {
      if ((*strm = fopen(infile, "r")) == NULL)
      {
          (void) fprintf(stderr, err1, infile);
          return(-1);
      }
  }



  switch(in->inputClass)
  {
    case 0: /*  TEXTIN */
    case 4: /*  IN  */
      if (allocateIntegerStorage(in) == -1)
      {
        (void) fprintf(stderr, err2, infile);
        return(-1);
      }

      if (readIntegerData(strm, in) == -1)
      {
        (void) fprintf(stderr, err4, infile);
        return(-1);
      }
    break;

    case 1: /*  TEXTFP */
    case 2: /*  TEXTFPE  */
    case 3: /*  FP  */
      if (allocateFloatStorage(in) == -1)
      {
        (void) fprintf(stderr, err3, infile);
        return(-1);

      }

      if (readFloatData(strm, in) == -1)
      {
        (void) fprintf(stderr, err5, infile);
          return(-1);
      }
    break;

    case 5: /*  STR  */

        if (processStrData(strm, in, file_id) == -1)
        {
            (void) fprintf(stderr, err11, infile);
            return(-1);
        }



    break;

    case 6: /* TEXTUIN */
    case 7: /* UIN */
      if (allocateUIntegerStorage(in) == -1)
      {
        (void) fprintf(stderr, err6, infile);
          return(-1);
      }
      if (readUIntegerData(strm, in) == -1)
      {
        (void) fprintf(stderr, err7, infile);
         return(-1);
      }
    break;

    default:
        (void) fprintf(stderr, "%s", err10);
        return(-1);
  }
  return (0);
}

static int
readIntegerData(FILE **strm, struct Input *in)
{
  H5DT_INT8 *in08;
  H5DT_INT16 *in16, temp;
  H5DT_INT32 *in32;
#ifndef WIN32
  H5DT_INT64 *in64;
  char buffer[256];
#endif
  hsize_t len=1;
  hsize_t i;
  int j;

  const char *err1 = "Unable to get integer value from file.\n";
  const char *err2 = "Unrecognized input class type.\n";
  const char *err3 = "Invalid input size.\n";

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 8:
      switch(in->inputClass)
      {
        case 0: /* TEXTIN */
          in08 = (H5DT_INT8 *) in->data;
          for (i = 0; i < len; i++, in08++)
          {
            if (fscanf(*strm, "%hd", &temp) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
            (*in08) = (H5DT_INT8)temp;
          }
        break;

        case 4: /* IN */
          in08 = (H5DT_INT8 *) in->data;
          for (i = 0; i < len; i++, in08++)
          {
            if (fread((char *) in08, sizeof(H5DT_INT8), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    case 16:
      in16 = (H5DT_INT16 *) in->data;
      switch(in->inputClass)
      {
        case 0: /* TEXTIN */
          for (i = 0; i < len; i++, in16++)
          {
            if (fscanf(*strm, "%hd", in16) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
          in16 = (H5DT_INT16 *) in->data;
        break;

        case 4: /* IN */
          for (i = 0; i < len; i++, in16++)
          {
            if (fread((char *) in16, sizeof(H5DT_INT16), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    case 32:
      in32 = (H5DT_INT32 *) in->data;
      switch(in->inputClass)
      {
        case 0: /* TEXTIN */
          for (i = 0; i < len; i++, in32++)
          {
            if (fscanf(*strm, "%d", in32) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        case 4: /* IN */
          for (i = 0; i < len; i++, in32++)
          {
            if (fread((char *) in32, sizeof(H5DT_INT32), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

#ifndef _WIN32
    case 64:
      in64 = (H5DT_INT64 *) in->data;
      switch(in->inputClass)
      {
        case 0: /* TEXTIN */
          for (i = 0; i < len; i++, in64++)
          {
            if (fscanf(*strm, "%s", buffer) < 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
            *in64 = (H5DT_INT64) HDstrtoll(buffer, NULL, 10);
          }
        break;

        case 4: /* IN */
          for (i = 0; i < len; i++, in64++)
          {
            if (fread((char *) in64, sizeof(H5DT_INT64), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
  	  break;
#endif /* ifndef _WIN32 */

    default:
      (void) fprintf(stderr, "%s", err3);
      break;
  }
  return(0);
}

static int
readUIntegerData(FILE **strm, struct Input *in)
{
  H5DT_UINT8 *in08;
  H5DT_UINT16 *in16, temp;
  H5DT_UINT32 *in32;
#ifndef _WIN32
  H5DT_UINT64 *in64;
  char buffer[256];
#endif
  hsize_t len=1;
  hsize_t i;
  int j;
  const char *err1 = "Unable to get unsigned integer value from file.\n";
  const char *err2 = "Unrecognized input class type.\n";
  const char *err3 = "Invalid input size.\n";

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 8:
      switch(in->inputClass)
      {
        case 6: /* TEXTUIN */
          in08 = (H5DT_UINT8 *) in->data;
          for (i = 0; i < len; i++, in08++)
          {
            if (fscanf(*strm, "%hu", &temp) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
            (*in08) = (H5DT_UINT8)temp;
          }
        break;

        case 7: /* UIN */
          in08 = (H5DT_UINT8 *) in->data;
          for (i = 0; i < len; i++, in08++)
          {
            if (fread((char *) in08, sizeof(H5DT_UINT8), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    case 16:
      in16 = (H5DT_UINT16 *) in->data;
      switch(in->inputClass)
      {
        case 6: /* TEXTUIN */
          for (i = 0; i < len; i++, in16++)
          {
            if (fscanf(*strm, "%hu", in16) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        case 7: /* UIN */
          for (i = 0; i < len; i++, in16++)
          {
            if (fread((char *) in16, sizeof(H5DT_UINT16), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    case 32:
      in32 = (H5DT_UINT32 *) in->data;
      switch(in->inputClass)
      {
        case 6: /* TEXTUIN */
          for (i = 0; i < len; i++, in32++)
          {
            if (fscanf(*strm, "%u", in32) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        case 7: /* UIN */
          for (i = 0; i < len; i++, in32++)
          {
            if (fread((char *) in32, sizeof(H5DT_UINT32), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

#ifndef _WIN32
    case 64:
      in64 = (H5DT_UINT64 *) in->data;
      switch(in->inputClass)
      {
        case 6: /* TEXTUIN */
          for (i = 0; i < len; i++, in64++)
          {
            if (fscanf(*strm, "%s", buffer) < 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
            *in64 = (H5DT_UINT64) HDstrtoll(buffer, NULL, 10);
          }
        break;

        case 7: /* UIN */
          for (i = 0; i < len; i++, in64++)
          {
            if (fread((char *) in64, sizeof(H5DT_UINT64), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;
#endif /* ifndef _WIN32 */

    default:
      (void) fprintf(stderr, "%s", err3);
      break;
  }
  return(0);
}

static int
readFloatData(FILE **strm, struct Input *in)
{
  H5DT_FLOAT32 *fp32;
  H5DT_FLOAT64 *fp64;

  hsize_t len=1;
  hsize_t i;
  int j;
  const char *err1 = "Unable to get integer value from file.\n";
  const char *err2 = "Unrecognized input class type.\n";
  const char *err3 = "Invalid input size type.\n";

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 32:
      fp32 = (H5DT_FLOAT32 *) in->data;
      switch(in->inputClass)
      {
        case 1: /* TEXTFP */
          for (i = 0; i < len; i++, fp32++)
          {
            if (fscanf(*strm, "%f", fp32) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }

          fp32 = (H5DT_FLOAT32 *) in->data;
        break;

        /* same as TEXTFP */
        case 2: /*TEXTFPE */

            for (i = 0; i < len; i++, fp32++)
            {
                if (fscanf(*strm, "%f", fp32) != 1)
                {
                    (void) fprintf(stderr, "%s", err1);
                    return (-1);
                }
            }

            fp32 = (H5DT_FLOAT32 *) in->data;
            break;

        case 3: /* FP */
          for (i = 0; i < len; i++, fp32++)
          {
            if (fread((char *) fp32, sizeof(H5DT_FLOAT32), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    case 64:
      fp64 = (H5DT_FLOAT64 *) in->data;
      switch(in->inputClass)
      {
        case 1: /* TEXTFP */
          for (i = 0; i < len; i++, fp64++)
          {
            if (fscanf(*strm, "%lf", fp64) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }

          fp64 = (H5DT_FLOAT64 *) in->data;
        break;

        /* same as TEXTFP */
        case 2: /*TEXTFPE */

            for (i = 0; i < len; i++, fp64++)
            {
                if (fscanf(*strm, "%lf", fp64) != 1)
                {
                    (void) fprintf(stderr, "%s", err1);
                    return (-1);
                }
            }

            fp64 = (H5DT_FLOAT64 *) in->data;
            break;

        case 3: /* FP */
          for (i = 0; i < len; i++, fp64++)
          {
            if (fread((char *) fp64, sizeof(H5DT_FLOAT64), 1, *strm) != 1)
            {
              (void) fprintf(stderr, "%s", err1);
              return (-1);
            }
          }
        break;

        default:
          (void) fprintf(stderr, "%s", err2);
          return (-1);
      }
    break;

    default:
      (void) fprintf(stderr, "%s", err3);
      break;
  }
  return(0);
}



/*-------------------------------------------------------------------------
 * Function: processStrData
 *
 * Purpose: read an ASCII file with string data and generate an HDF5 dataset
 *  with a variable length type
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: July, 26, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
processStrData(FILE **strm, struct Input *in, hid_t file_id)
{
    hid_t   group_id, dset_id, space_id, mspace_id, type_id, handle;
    hsize_t dims[1];
    char    str[1024];
    char    c;
    int     i = 0, j, nlines = 0, line;

/*-------------------------------------------------------------------------
 * get number of lines in the input file
 *-------------------------------------------------------------------------
 */

    while ( !feof( *strm ) )
    {
        c = fgetc( *strm );

        if ( c == 10 ) /* eol */
        {
            nlines++;

        }
    }

    if ( !nlines )
        return 0;

    /* number of records */
    dims[0] = nlines;

    /* rewind */
    fseek(*strm,0L,0);

/*-------------------------------------------------------------------------
 * read file again and generate an HDF5 dataset
 *-------------------------------------------------------------------------
 */

    if (( type_id = H5Tcopy(H5T_C_S1)) < 0 )
        goto out;

    if ( H5Tset_size (type_id,H5T_VARIABLE) < 0 )
        goto out;

    /* disable error reporting */
    H5E_BEGIN_TRY
    {

        /* create parent groups */
        if(in->path.count > 1) {
            j = 0;
            handle = file_id;
            while(j < in->path.count - 1) {
                if((group_id = H5Gopen2(handle, in->path.group[j], H5P_DEFAULT)) < 0) {
                    group_id = H5Gcreate2(handle, in->path.group[j++], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                    for(; j < in->path.count - 1; j++)
                        group_id = H5Gcreate2(group_id, in->path.group[j], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                    handle = group_id;
                    break;
                }
                handle = group_id;
                j++;
            }
        }
        else {
            handle = file_id;
            j = 0;
        }

        /*enable error reporting */
    } H5E_END_TRY;

    if((space_id = H5Screate_simple(1, dims, NULL)) < 0)
        goto out;

    if((mspace_id = H5Screate(H5S_SCALAR)) < 0)
        goto out;

    if((dset_id = H5Dcreate2(handle, in->path.group[j], type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    line = 0;

    while(!feof(*strm)) {
        c = fgetc(*strm);

        str[i] = c;

        i++;

        if(c == 10) /* eol */
        {
            char    *str2 = str;
            hid_t   fspace_id;
            hsize_t start[1];
            hsize_t count[1] = { 1 };

            str[ i-1 ] = '\0'; /* terminate string */

            if (( fspace_id = H5Dget_space (dset_id)) < 0 )
                goto out;

            start[0] = line ++ ;

            if ( H5Sselect_hyperslab(fspace_id,H5S_SELECT_SET,start,NULL,count,NULL) < 0 )
                goto out;

            if ( H5Dwrite(dset_id,type_id,mspace_id,fspace_id,H5P_DEFAULT, &str2 ) < 0 )
                goto out;

            if ( H5Sclose(fspace_id) < 0 )
                goto out;

            i = 0;
            str[ 0 ] = '\0';

        }
    }


    /* close */
    H5Dclose(dset_id);
    H5Sclose(space_id);
    H5Sclose(mspace_id);
    H5Tclose(type_id);

    return(0);

out:

    return (-1);
}


static int
allocateIntegerStorage(struct Input *in)
{
  hsize_t len=1;
  int j;
  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "Invalid storage size for integer input data.\n";

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 8:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_INT8))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    case 16:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_INT16))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    case 32:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_INT32))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    case 64:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_INT64))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    default:
      (void) fprintf(stderr, "%s", err2);
    break;
  }
  return(0);
}

static int allocateUIntegerStorage(struct Input *in)
{
  hsize_t len=1;
  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "Invalid storage size for unsigned integer input data.\n";
  int j;

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 8:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_UINT8))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
      break;

    case 16:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_UINT16))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
      break;

    case 32:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_UINT32))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
      break;

    case 64:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_UINT64))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
      break;

    default:
      (void) fprintf(stderr, "%s", err2);
      break;
  }
  return(0);
}

static int
allocateFloatStorage(struct Input *in)
{
  hsize_t len = 1;
  int j;
  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "Invalid storage size for float input data.\n";

  for (j=0; j<in->rank;j++)
    len *= in->sizeOfDimension[j];

  switch(in->inputSize)
  {
    case 32:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_FLOAT32))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    case 64:
      if ((in->data = (VOIDP) HDmalloc((size_t) len * sizeof(H5DT_FLOAT64))) == NULL)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }
    break;

    default:
      (void) fprintf(stderr, "%s", err2);
    break;
  }
  return(0);
}

static int
processConfigurationFile(char *infile, struct Input *in, FILE **strm)
{
  char key[255];
  int kindex;
  char temp[255];
  int ival;

  const char *err1 = "Unable to open the configuration file:  %s for reading.\n";
  const char *err2 = "Unknown keyword in configuration file: %s\n";
  const char *err3a = "PATH keyword appears twice in %s.\n";
  const char *err3b = "Error in parsing the path information from %s.\n";
  const char *err4a = "INPUT-CLASS keyword appears twice in %s.\n";
  const char *err4b = "Error in retrieving the input class from %s.\n";
  const char *err5a = "INPUT-SIZE keyword appears twice in %s.\n";
  const char *err5b = "Error in retrieving the input size from %s.\n";
  const char *err6a = "RANK keyword appears twice in %s.\n";
  const char *err6b = "Error in retrieving the rank from %s.\n";
  const char *err7a = "DIMENSION-SIZES keyword appears twice in %s.\n";
  const char *err7b = "DIMENSION-SIZES cannot appear before RANK is provided.\n";
  const char *err7c = "Error in retrieving the dimension sizes from %s.\n";
  const char *err8a = "OUTPUT-CLASS keyword appears twice in %s.\n";
  const char *err8b = "Error in retrieving the output class from %s.\n";
  const char *err9a = "OUTPUT-SIZE keyword appears twice in %s.\n";
  const char *err9b = "Error in retrieving the output size from %s.\n";
  const char *err10a = "OUTPUT-ARCHITECTURE keyword appears twice in %s.\n";
  const char *err10b = "Error in retrieving the output architecture from %s.\n";
  const char *err11a = "OUTPUT-BYTE-ORDER keyword appears twice in %s.\n";
  const char *err11b = "Error in retrieving the output byte order from %s.\n";
  const char *err12a = "CHUNKED-DIMENSION-SIZES keyword appears twice in %s.\n";
  const char *err12b = "CHUNKED-DIMENSION-SIZES cannot appear before DIMENSION-SIZES are provided.\n";
  const char *err12c = "Error in retrieving the chunked dimension sizes from %s.\n";
  const char *err13a = "COMPRESSION-TYPE keyword appears twice in %s.\n";
  const char *err13b = "Error in retrieving the compression type from %s.\n";
  const char *err14a = "COMPRESSION-PARAM keyword appears twice in %s.\n";
  const char *err14b = "Error in retrieving the compression parameter from %s.\n";
  const char *err15a = "EXTERNAL-STORAGE keyword appears twice in %s.\n";
  const char *err15b = "Error in retrieving the external storage paramters from %s.\n";
  const char *err16a = "MAXIMUM-DIMENSIONS keyword appears twice in %s.\n";
  const char *err16b = "MAXIMUM-DIMENSIONS cannot appear before DIMENSION-SIZES are provided.\n";
  const char *err16c = "Error in retrieving the maximum dimension sizes from %s.\n";
  const char *err17 =  "Configuration parameters are invalid in %s.\n";
  const char *err18 =  "Unable to get string value.\n";
  const char *err19 =  "Unable to get integer value.\n";

  /* create vector to map which keywords have been found
  check vector after each keyword to check for violation
  at the end check vector to see if required fields have been provided
  process the output file according to the options
  */

  if ((*strm = fopen(infile, "r")) == NULL)
  {
    (void) fprintf(stderr, err1, infile);
    return (-1);
  }

  while (fscanf(*strm, "%s", key) == 1)
  {
    if ((kindex = mapKeywordToIndex(key)) == -1)
    {
      (void) fprintf(stderr, err2, infile);
      return (-1);
    }
    switch (kindex)
    {
      case 0: /* PATH */
        if (in->configOptionVector[PATH] == 1)
        {
          (void) fprintf(stderr, err3a, infile);
          return (-1);
        }
	if (fscanf(*strm, "%s", temp) != 1)
	{
	  (void) fprintf(stderr, "%s", err18);
	  return (-1);
	}
        if (parsePathInfo(&in->path, temp) == -1)
        {
          (void) fprintf(stderr, err3b, infile);
          return (-1);
        }
        in->configOptionVector[PATH] = 1;
      break;

      case 1: /* INPUT-CLASS */
        if (in->configOptionVector[INPUT_CLASS] == 1)
        {
          (void) fprintf(stderr, err4a, infile);
          return (-1);
        }

	if (fscanf(*strm, "%s", temp) != 1)
	{
	  (void) fprintf(stderr, "%s", err18);
	  return (-1);
	}
        if (getInputClass(in, temp) == -1)
        {
          (void) fprintf(stderr, err4b, infile);
          return (-1);
        }

        in->configOptionVector[INPUT_CLASS] = 1;

        /*set default value for output-class */
        if (in->configOptionVector[OUTPUT_CLASS] == 0)
        {
          if (in->inputClass == 0 || in->inputClass == 4)
            in->outputClass = 0;
          if (in->inputClass == 1 || in->inputClass == 2 || in->inputClass == 3)
            in->outputClass = 1;
          if (in->inputClass == 6 || in->inputClass == 7)
            in->outputClass = 2;
        }
      break;

      case 2: /* INPUT-SIZE */
        if (in->configOptionVector[INPUT_SIZE] == 1)
        {
          (void) fprintf(stderr, err5a, infile);
          return (-1);
        }
	if (fscanf(*strm, "%d", (&ival)) != 1)
	{
	  (void) fprintf(stderr, "%s", err19);
	  return (-1);
	}
        if (getInputSize(in, ival) == -1)
        {
          (void) fprintf(stderr, err5b, infile);
          return (-1);
        }
        in->configOptionVector[INPUT_SIZE] = 1;

        /*set default value for output-size */
        if (in->configOptionVector[OUTPUT_SIZE] == 0)
          in->outputSize = in->inputSize;
      break;

      case 3: /* RANK */
        if (in->configOptionVector[RANK] == 1)
        {
          (void) fprintf(stderr, err6a, infile);
          return (-1);
        }

        if (getRank(in, strm) == -1)
        {
          (void) fprintf(stderr, err6b, infile);
          return (-1);
        }
        in->configOptionVector[RANK] = 1;
      break;

      case 4: /* DIMENSION-SIZES */
        if (in->configOptionVector[DIM] == 1)
        {
          (void) fprintf(stderr, err7a, infile);
          return (-1);
        }

        if (in->configOptionVector[RANK] == 0)
        {
          (void) fprintf(stderr, err7b, infile);
          return (-1);
        }
        if (getDimensionSizes(in, strm) == -1)
        {
          (void) fprintf(stderr, err7c, infile);
          return (-1);
        }
        in->configOptionVector[DIM] = 1;
      break;

      case 5: /* OUTPUT-CLASS */
        if (in->configOptionVector[OUTPUT_CLASS] == 1)
        {
          (void) fprintf(stderr, err8a, infile);
          return (-1);
        }

        if (getOutputClass(in, strm) == -1)
        {
          (void) fprintf(stderr, err8b, infile);
          return (-1);
        }
        in->configOptionVector[OUTPUT_CLASS] = 1;
      break;

      case 6: /* OUTPUT-SIZE */
        if (in->configOptionVector[OUTPUT_SIZE] == 1)
        {
          (void) fprintf(stderr, err9a, infile);
          return (-1);
        }

        if (getOutputSize(in, strm) == -1)
        {
          (void) fprintf(stderr, err9b, infile);
          return (-1);
        }
        in->configOptionVector[OUTPUT_SIZE] = 1;
      break;

      case 7: /* OUTPUT-ARCHITECTURE */
        if (in->configOptionVector[OUTPUT_ARCH] == 1)
        {
          (void) fprintf(stderr, err10a, infile);
          return (-1);
        }

        if (getOutputArchitecture(in, strm) == -1)
        {
          (void) fprintf(stderr, err10b, infile);
          return (-1);
        }
        in->configOptionVector[OUTPUT_ARCH] = 1;
      break;

      case 8: /* OUTPUT-BYTE-ORDER */
        if (in->configOptionVector[OUTPUT_B_ORDER] == 1)
        {
          (void) fprintf(stderr, err11a, infile);
          return (-1);
        }

        if (getOutputByteOrder(in, strm) == -1)
        {
          (void) fprintf(stderr, err11b, infile);
          return (-1);
        }
        in->configOptionVector[OUTPUT_B_ORDER] = 1;
      break;

      case 9: /* CHUNKED-DIMENSION-SIZES */
        if (in->configOptionVector[CHUNK] == 1)
        {
          (void) fprintf(stderr, err12a, infile);
          return (-1);
        }
        /* cant appear before dimension sizes have been provided */
        if (in->configOptionVector[DIM] == 0)
        {
          (void) fprintf(stderr, err12b, infile);
          return (-1);
        }

        if (getChunkedDimensionSizes(in, strm) == -1)
        {
          (void) fprintf(stderr, err12c, infile);
          return (-1);
        }
        in->configOptionVector[CHUNK] = 1;
      break;

      case 10: /* COMPRESSION-TYPE */
        if (in->configOptionVector[COMPRESS] == 1)
        {
          (void) fprintf(stderr, err13a, infile);
          return (-1);
        }

        if (getCompressionType(in, strm) == -1)
        {
          (void) fprintf(stderr, err13b, infile);
          return (-1);
        }
        in->configOptionVector[COMPRESS] = 1;

        if (in->configOptionVector[COMPRESS_PARAM] == 0)
        {
          if (in->compressionType == 0)
            in->compressionParam = 6; /* default value if compressionType is GZIP */
        }
      break;

      case 11: /* COMPRESSION-PARAM */
        if (in->configOptionVector[COMPRESS_PARAM] == 1)
        {
          (void) fprintf(stderr, err14a, infile);
          return (-1);
        }

        if (getCompressionParameter(in, strm) == -1)
        {
          (void) fprintf(stderr, err14b, infile);
          return (-1);
        }

        in->configOptionVector[COMPRESS_PARAM] = 1;

        if (in->configOptionVector[COMPRESS] == 0)
          in->compressionType = 0;


      break;

      case 12: /* EXTERNAL-STORAGE */
        if (in->configOptionVector[EXTERNAL] == 1)
        {
          (void) fprintf(stderr, err15a, infile);
          return (-1);
        }

        if (getExternalFilename(in, strm) == -1)
        {
          (void) fprintf(stderr, err15b, infile);
          return (-1);
        }
        in->configOptionVector[EXTERNAL] = 1;
      break;

      case 13: /* MAXIMUM-DIMENSIONS */
        if (in->configOptionVector[EXTEND] == 1)
        {
          (void) fprintf(stderr, err16a, infile);
          return (-1);
        }
        /* cant appear before dimension sizes have been provided */
        if (in->configOptionVector[DIM] == 0)
        {
          (void) fprintf(stderr, err16b, infile);
          return (-1);
        }
        if (getMaximumDimensionSizes(in, strm) == -1)
        {
          (void) fprintf(stderr, err16c, infile);
          return (-1);
        }
        in->configOptionVector[EXTEND] = 1;
      break;

      default:
        break;
    }
  }
  /*
    check if keywords obtained are valid
    if yes, return 0 else error
  */

  if (validateConfigurationParameters(in) == -1)
  {
    (void) fprintf(stderr, err17, infile);
    return (-1);
  }

  return (0);
}

static int
validateConfigurationParameters(struct Input * in)
{
  const char *err1 = "One or more of the required fields (RANK, DIMENSION-SIZES) missing.\n";
  const char *err2 = "Cannot specify chunking or compression or extendible data sets with the external file option.\n";
  const char *err3 = "Cannot specify the compression or the extendible data sets without the chunking option.\n";
  const char *err4a = "OUTPUT-ARCHITECTURE cannot be STD if OUTPUT-CLASS is floating point (FP).\n";
  const char *err4b = "OUTPUT-ARCHITECTURE cannot be IEEE if OUTPUT-CLASS is integer (IN).\n";
  const char *err5 = "For OUTPUT-CLASS FP, valid values for OUTPUT-SIZE are (32, 64) .\n";
#ifdef _WIN32
  const char *err6 = "No support for reading 64-bit integer (INPUT-CLASS: IN, TEXTIN, UIN, TEXTUIN files\n";
#endif

   /* for class STR other parameters are ignored */
  if (in->inputClass == 5) /* STR */
      return (0);

  if (
      (in->configOptionVector[DIM] != 1) ||
      (in->configOptionVector[RANK] != 1))
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  if (in->configOptionVector[EXTERNAL] == 1)
  {
    if ((in->configOptionVector[COMPRESS] == 1) ||
        (in->configOptionVector[CHUNK] == 1) ||
        (in->configOptionVector[EXTEND] == 1))
    {
      (void) fprintf(stderr, "%s", err2);
      return (-1);
    }
  }

  if ((in->configOptionVector[COMPRESS] == 1) ||
      (in->configOptionVector[EXTEND] == 1))
  {
    if (in->configOptionVector[CHUNK] != 1)
    {
      (void) fprintf(stderr, "%s", err3);
      return (-1);
    }
  }

  /* Arch cant be STD if O/p class is FP */
  if (in->outputArchitecture == 1)
    if (in->outputClass == 1)
    {
      (void) fprintf(stderr, "%s", err4a);
      return (-1);
    }

  /* Arch cant be IEEE if O/p class is IN */
  if (in->outputArchitecture == 2)
    if (in->outputClass == 0)
    {
      (void) fprintf(stderr, "%s", err4b);
      return (-1);
    }

  if (in->outputClass == 1)
    if(in->outputSize != 32 &&
       in->outputSize != 64 )
    {
      (void) fprintf(stderr, "%s", err5);
      return (-1);
    }

#ifdef _WIN32
  if (in->inputSize == 64 && (in->inputClass == 0 || in->inputClass == 4 || in->inputClass == 6 || in->inputClass == 7) )
	{
	  (void) fprintf(stderr, "%s", err6);
	  return -1;
	}
#endif
  return (0);
}

static int
mapKeywordToIndex(char *key)
{
  int i;

  for (i=0; i<NUM_KEYS; i++)
    if (!HDstrcmp(keytable[i], key)) return i;
  return -1;
}

static int
parsePathInfo(struct path_info *path, char *temp)
{
  const char delimiter[] = "/";
  char *token;
  int i=0;
  const char *err1 = "Path string larger than MAX_PATH_NAME_LENGTH.\n";

  token = HDstrtok (temp, delimiter);
  if (HDstrlen(token) >= MAX_PATH_NAME_LENGTH)
  {
    (void) fprintf(stderr, err1);
    return (-1);
  }
   HDstrcpy(path->group[i++],token);


  while (1)
  {
    token = HDstrtok (NULL, delimiter);
    if (token == NULL)
      break;
    if (HDstrlen(token) >= MAX_PATH_NAME_LENGTH)
    {
      (void) fprintf(stderr, err1);
      return (-1);
    }
    HDstrcpy(path->group[i++],token);
  }
  path->count = i;
  return (0);
}

static int
parseDimensions(struct Input *in, char *strm)
{
  const char delimiter[] = ",";
  char temp[255];
  char *token;
  int i=0;
  const char *err1 = "Unable to allocate dynamic memory.\n";

  HDstrncpy(temp, strm, sizeof(temp));
  temp[sizeof(temp)-1] = '\0';
  HDstrtok (temp, delimiter);

  while (1)
  {
    token = HDstrtok (NULL, delimiter);
    if (token == NULL)
      break;
    i++;
  }
  in->rank = i+1;
  if ((in->sizeOfDimension =
               (hsize_t *) HDmalloc ((size_t) in->rank * sizeof(hsize_t))) == NULL)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  i=0;
  HDstrncpy(temp, strm, sizeof(temp));
  temp[sizeof(temp)-1] = '\0';
  in->sizeOfDimension[i++] = HDstrtol(HDstrtok (temp, delimiter), NULL, BASE_10);

  while (1)
  {
    token = HDstrtok (NULL, delimiter);
    if (token == NULL)
      break;
    in->sizeOfDimension[i++] = HDstrtol(token, NULL, BASE_10);
  }
  return (0);
}

static int
getOutputClass(struct Input *in, FILE** strm)
{
  char temp[255];
  int kindex;
  const char *err1 = "Unable to get 'string' value.\n";
  const char *err2 = "Invalid value for output class.\n";

  if (fscanf(*strm, "%s", temp) != 1)
    {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  if ((kindex = OutputClassStrToInt(temp)) == -1)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  in->outputClass = kindex;
  return (0);
}

static int
OutputClassStrToInt(char *temp)
{
  int i;
  char classKeywordTable[3][15] = {
    "IN",
    "FP",
    "UIN"
  };
  for  (i=0; i<3; i++)
    if (!HDstrcmp(classKeywordTable[i], temp)) return i;

  return -1;
}
 /* same as getInputSize. But defined separately for extensibility */
static int
getOutputSize(struct Input *in, FILE** strm)
{
  int ival;
  int i;
  int outputSizeValidValues[4] = {8,16,32,64};
  const char *err1 = "Unable to get integer value.\n";
  const char *err2 = "Invalid value for output size.\n";

  if (fscanf(*strm, "%d", (&ival)) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
        return (-1);
  }

  for  (i=0; i<4; i++)
    if (outputSizeValidValues[i] == ival)
    {
      in->outputSize = ival;
      return (0);
    }
  (void) fprintf(stderr, "%s", err2);
  return(-1);
}

static int
getInputClass(struct Input *in, char * temp)
{
  int kindex;
  const char *err1 = "Invalid value for input class.\n";

  if ((kindex = InputClassStrToInt(temp)) == -1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  in->inputClass = kindex;
  return (0);
}

static int
InputClassStrToInt(char *temp)
{
  int i;
  char classKeywordTable[8][15] = {
    "TEXTIN",
    "TEXTFP",
    "TEXTFPE",
    "FP",
    "IN",
    "STR",
    "TEXTUIN",
    "UIN"
  };
  for  (i=0; i<8; i++)
    if (!HDstrcmp(classKeywordTable[i], temp)) return i;
  return -1;
}

 /* same as getOutputSize. But defined separately for extensibility */
static int
getInputSize(struct Input *in, int ival)
{
  int i;
  int inputSizeValidValues[4] = {8,16,32,64};
  const char *err1 = "Invalid value for input size.\n";

  for  (i=0; i<4; i++)
    if (inputSizeValidValues[i] == ival)
    {
      in->inputSize = ival;
      return (0);
    }
  (void) fprintf(stderr, "%s", err1);
  return(-1);
}

static int
getRank(struct Input *in, FILE** strm)
{
  int ival;

  const char *err1 = "Unable to get integer value.\n";
  const char *err2 = "Invalid value for rank.\n";

  if (fscanf(*strm, "%d", (&ival)) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }
  if (ival >=MIN_NUM_DIMENSION && ival <=MAX_NUM_DIMENSION )
  {
    in->rank = ival;
    return (0);
  }

  (void) fprintf(stderr, "%s", err2);
  return(-1);
}

 /* same as getChunkedDimensionSizes. But defined separately for extensibility */
static int
getDimensionSizes(struct Input *in, FILE **strm)
{
  int ival;
  int i=0;

  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "No. of dimensions for which dimension sizes provided is not equal to provided rank.\n";

  if ((in->sizeOfDimension =
               (hsize_t *) HDmalloc ((size_t) in->rank * sizeof(hsize_t))) == NULL)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  while (fscanf(*strm, "%d", (&ival)) == 1)
    in->sizeOfDimension[i++] = ival;

  if (in->rank != i)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }
  return (0);
}
 /* same as getDimensionSizes. But defined separately for extensibility */
static int
getChunkedDimensionSizes(struct Input *in, FILE **strm)
{
  int ival;
  int i=0;

  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "No. of dimensions for which chunked dimension sizes provided is not equal to provided rank.\n";
  const char *err3 = "The CHUNKED-DIMENSION-SIZES cannot exceed the sizes of DIMENSION-SIZES\n";

  if ((in->sizeOfChunk =
           (hsize_t *) HDmalloc ((size_t) in->rank * sizeof(hsize_t))) == NULL)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  while (fscanf(*strm, "%d", (&ival)) == 1)
      in->sizeOfChunk[i++] = ival;

  if (in->rank != i)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  for (i=0; i<in->rank; i++)
    if (in->sizeOfChunk[i] > in->sizeOfDimension[i])
    {
      (void) fprintf(stderr, "%s", err3);
      return (-1);
    }
  return (0);
}

static int
getMaximumDimensionSizes(struct Input *in, FILE **strm)
{
  int ival;
  int i=0;

  const char *err1 = "Unable to allocate dynamic memory.\n";
  const char *err2 = "No. of dimensions for which maximum dimension sizes provided is not equal to provided rank.\n";
  const char *err3 = "The MAXIMUM-DIMENSIONS cannot be less than the sizes of DIMENSION-SIZES. Exception: can be -1 to indicate unlimited size\n";

  if ((in->maxsizeOfDimension =
                (hsize_t *) HDmalloc ((size_t) in->rank * sizeof(hsize_t))) == NULL)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  while (fscanf(*strm, "%d", (&ival)) == 1)
  {
    if (ival == -1)
         in->maxsizeOfDimension[i++] = H5S_UNLIMITED;
    else
         in->maxsizeOfDimension[i++] = ival;
  }

  if (in->rank != i)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  for (i=0; i<in->rank; i++)
  {
    if (in->maxsizeOfDimension[i] != H5S_UNLIMITED)
      if (in->maxsizeOfDimension[i] < in->sizeOfDimension[i])
      {
        (void) fprintf(stderr, "%s", err3);
        return (-1);
      }
  }
  return (0);
}

static int
getOutputArchitecture(struct Input *in, FILE** strm)
{
  char temp[255];
  int kindex;
  const char *err1 = "Unable to get 'string' value.\n";
  const char *err2 = "Invalid value for output architecture.\n";

  if (fscanf(*strm, "%s", temp) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  if ((kindex = OutputArchStrToInt(temp)) == -1)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  in->outputArchitecture = kindex;
  return (0);
}

static int
OutputArchStrToInt(char *temp)
{
  int i;
  char outputArchKeywordTable[8][15] = {
    "NATIVE",
    "STD",
    "IEEE",
    "INTEL",
    "CRAY",
    "MIPS",
    "ALPHA",
    "UNIX"
  };
  for  (i=0; i<8; i++)
    if (!HDstrcmp(outputArchKeywordTable[i], temp)) return i;
  return -1;
}

static int
getOutputByteOrder(struct Input *in, FILE** strm)
{
  char temp[255];
  int kindex;
  const char *err1 = "Unable to get 'string' value.\n";
  const char *err2 = "Invalid value for output byte-order.\n";

  if (fscanf(*strm, "%s", temp) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  if ((kindex = OutputByteOrderStrToInt(temp)) == -1)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  in->outputByteOrder = kindex;
  return (0);
}

static int
OutputByteOrderStrToInt(char *temp)
{
  int i;
  char outputByteOrderKeywordTable[2][15] = {
    "BE",
    "LE"
  };
  for  (i=0; i<2; i++)
    if (!HDstrcmp(outputByteOrderKeywordTable[i], temp)) return i;
  return -1;
}

static int
getCompressionType(struct Input *in, FILE** strm)
{
  char temp[255];
  int kindex;
  const char *err1 = "Unable to get 'string' value.\n";
  const char *err2 = "Invalid value for compression.\n";

  if (fscanf(*strm, "%s", temp) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  if ((kindex = CompressionTypeStrToInt(temp)) == -1)
  {
    (void) fprintf(stderr, "%s", err2);
    return (-1);
  }

  in->compressionType = kindex;
  return (0);

}

static int
CompressionTypeStrToInt(char *temp)
{
  /* currently supports only GZIP */
  /* can be extended by adding fields to the table */

  int i;
  char CompressionTypeKeywordTable[1][15] = {
    "GZIP"
  };
  for  (i=0; i<1; i++)
    if (!HDstrcmp(CompressionTypeKeywordTable[i], temp)) return i;
  return -1;
}

static int
getCompressionParameter(struct Input *in, FILE** strm)
{
  /*  currently supports only GZIP */
  /*  can be extended by adding more values to COMPRESSION-TYPE and
    handling the paramters here by adding more cases  */

  int ival;
  const char *err1 = "Unable to get integer value.\n";
  const char *err2 = "Invalid value for compression paramter.\n";
  const char *err3 = "Unsupported Compression Type.\n";

  switch (in->compressionType)
  {
    case 0:    /* GZIP */
      if (fscanf(*strm, "%d", (&ival)) != 1)
      {
        (void) fprintf(stderr, "%s", err1);
        return (-1);
      }

      if (ival <0 || ival > 9)
      {
        (void) fprintf(stderr, "%s", err2);
        return (-1);
      }
      in->compressionParam = ival;
      return (0);

    default:
      (void) fprintf(stderr, "%s", err3);
      return (-1);
  }
}

static int
getExternalFilename(struct Input *in, FILE** strm)
{
  char temp[255];
  const char *err1 = "Unable to get 'string' value.\n";

  if (fscanf(*strm, "%s", temp) != 1)
  {
    (void) fprintf(stderr, "%s", err1);
    return (-1);
  }

  in->externFilename = (char *) HDmalloc ((size_t) (HDstrlen(temp)) * sizeof(char));
  (void) HDstrcpy(in->externFilename, temp);
  return (0);
}

void
setDefaultValues(struct Input *in, int count)
{
  int i;
  char temp[255];
  char num[255];

  in->inputClass = 3; /* FP */
  in->inputSize = 32;
  in->outputClass = 1; /* FP */
  in->outputSize = 32;
  in->rank = 0;
  in->path.count = 1;

  HDstrcpy(temp, "dataset");
  sprintf(num, "%d", count);
  HDstrcat(temp, num);
  HDstrcpy(in->path.group[0], temp);

  in->outputArchitecture = 0; /* NATIVE */
  in->outputByteOrder = -1;  /* use default    */
  in->compressionType = 0;  /* GZIP   */
  for (i=0; i<NUM_KEYS; i++)
    in->configOptionVector[i] = 0;
}

hid_t
createOutputDataType(struct Input *in)
{
  hid_t new_type = (-1);
  const char *err1 = "Invalid value for output class.\n";

  switch (in->outputClass)
  {
    case 0:
      switch (in->outputArchitecture)
      {
        case 0: /* NATIVE */
          switch(in->outputSize)
          {
            case 8:
              new_type = H5Tcopy (H5T_NATIVE_CHAR);
            break;

            case 16:
              new_type = H5Tcopy (H5T_NATIVE_SHORT);
            break;

            case 32:
              new_type = H5Tcopy (H5T_NATIVE_INT);
            break;

            case 64:
              new_type = H5Tcopy (H5T_NATIVE_LLONG);
            break;
          }
              switch(in->outputByteOrder)
              {
                case -1: /* default */
                break;
                case 0:
                  H5Tset_order (new_type,H5T_ORDER_BE);
                break;

                case 1:
                  H5Tset_order (new_type,H5T_ORDER_LE);
                break;
              }
        break;

        case 1: /* STD */
          switch(in->outputSize)
          {
            case 8:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_STD_I8BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_STD_I8LE);
                break;
              }
            break;

            case 16:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_STD_I16BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_STD_I16LE);
                break;
              }
            break;

            case 32:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_STD_I32BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_STD_I32LE);
                break;
              }
            break;

            case 64:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_STD_I64BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_STD_I64LE);
                break;
              }
            break;
          }
          break;

        }
        break;

    case 1:
      switch (in->outputArchitecture)
      {
        case 0:
          switch(in->outputSize)
          {
            case 32:
              new_type = H5Tcopy (H5T_NATIVE_FLOAT);
            break;

            case 64:
              new_type = H5Tcopy (H5T_NATIVE_DOUBLE);
            break;
          }
              switch(in->outputByteOrder)
              {
                case -1: /* DEFAULT */
                break;
                case 0:
                  H5Tset_order (new_type,H5T_ORDER_BE);
                break;

                case 1:
                  H5Tset_order (new_type,H5T_ORDER_LE);
                break;
              }
        break;

        case 1:
          /* STD not supported for float */
        break;

        case 2:
          switch(in->outputSize)
          {
            case 32:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_IEEE_F32BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_IEEE_F32LE);
                break;
              }
            break;

            case 64:
              switch(in->outputByteOrder)
              {
                case -1:
                case 0:
                  new_type = H5Tcopy (H5T_IEEE_F64BE);
                break;

                case 1:
                  new_type = H5Tcopy (H5T_IEEE_F64LE);
                break;
              }
            break;
          }
        break;

      }
      break;

    case 2:
        switch (in->outputArchitecture)
        {
          case 0:
            switch(in->outputSize)
            {
              case 8:
                new_type = H5Tcopy (H5T_NATIVE_UCHAR);
              break;

              case 16:
                new_type = H5Tcopy (H5T_NATIVE_USHORT);
              break;

              case 32:
                new_type = H5Tcopy (H5T_NATIVE_UINT);
              break;

              case 64:
                new_type = H5Tcopy (H5T_NATIVE_ULLONG);
              break;
            }
              switch(in->outputByteOrder)
              {
                case -1: /* Default */
                break;
                case 0:
                  H5Tset_order (new_type,H5T_ORDER_BE);
                break;

                case 1:
                  H5Tset_order (new_type,H5T_ORDER_LE);
                break;
              }
            break;

          case 1:
            switch(in->outputSize)
            {
              case 8:
                switch(in->outputByteOrder)
                {
                  case -1:
                  case 0:
                    new_type = H5Tcopy (H5T_STD_U8BE);
                  break;

                  case 1:
                    new_type = H5Tcopy (H5T_STD_U8LE);
                  break;
                }
                break;

              case 16:
                switch(in->outputByteOrder)
                {
                  case -1:
                  case 0:
                    new_type = H5Tcopy (H5T_STD_U16BE);
                  break;

                  case 1:
                    new_type = H5Tcopy (H5T_STD_U16LE);
                  break;
                }
                break;

              case 32:
                switch(in->outputByteOrder)
                {
                  case -1:
                  case 0:
                    new_type = H5Tcopy (H5T_STD_U32BE);
                  break;

                  case 1:
                    new_type = H5Tcopy (H5T_STD_U32LE);
                  break;
                }
                break;

              case 64:
                switch(in->outputByteOrder)
                {
                  case -1:
                  case 0:
                    new_type = H5Tcopy (H5T_STD_U64BE);
                  break;

                  case 1:
                    new_type = H5Tcopy (H5T_STD_U64LE);
                  break;
                }
                break;
            }
            break;

          case 2:
            /* IEEE not supported for INT */
          break;

        }

        break;

    default:
        (void) fprintf(stderr, "%s", err1);
        return (-1);
    }
    return new_type;
}

hid_t
createInputDataType(struct Input *in)
{
  hid_t new_type = (-1);
  const char *err1 = "Invalid value for input class.\n";

  switch (in->inputClass)
  {
    case 0:
    case 4:
        switch(in->inputSize)
        {
          case 8:
              new_type = H5Tcopy (H5T_NATIVE_CHAR);
              break;

          case 16:
              new_type = H5Tcopy (H5T_NATIVE_SHORT);
              break;

          case 32:
              new_type = H5Tcopy (H5T_NATIVE_INT);
              break;

          case 64:
              new_type = H5Tcopy (H5T_NATIVE_LLONG);
              break;
        }
        break;

    case 1:
    case 2:
    case 3:
        switch(in->inputSize)
        {
          case 32:
              new_type = H5Tcopy (H5T_NATIVE_FLOAT);
              break;

          case 64:
              new_type = H5Tcopy (H5T_NATIVE_DOUBLE);
              break;
        }
        break;

    case 5:
        break;

    case 6:
    case 7:
        switch(in->inputSize)
        {
          case 8:
              new_type = H5Tcopy (H5T_NATIVE_UCHAR);
              break;

          case 16:
              new_type = H5Tcopy (H5T_NATIVE_USHORT);
              break;

          case 32:
              new_type = H5Tcopy (H5T_NATIVE_UINT);
              break;

          case 64:
              new_type = H5Tcopy (H5T_NATIVE_ULLONG);
              break;
        }
        break;

    default:
        (void) fprintf(stderr, "%s", err1);
        return (-1);
    }
  return new_type;
}

static int
process(struct Options *opt)
{
  struct  Input *in;
  hid_t   file_id, group_id, handle;
  hid_t   dataset, dataspace = (-1);
  FILE *strm, *extfile;
  hid_t intype, outtype;
  hid_t proplist;
  hsize_t numOfElements = 1;
  int j,k;

  const char *err1 = "Error creating HDF output file: %s.\n";
  const char *err2 = "Error in processing the configuration file: %s.\n";
  const char *err3 = "Error in reading the input file: %s.\n";
  const char *err4 = "Error in creating or opening external file.\n";
  const char *err5 = "Error in creating the output data set. Dataset with the same name may exist at the specified path\n";
  const char *err6 = "Error in writing the output data set.\n";

  H5E_BEGIN_TRY {
    if ((file_id = H5Fopen(opt->outfile, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
      if ((file_id = H5Fcreate(opt->outfile, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == FAIL)
      {
        (void) fprintf(stderr, err1, opt->outfile);
        return (-1);
      }
    }
  } H5E_END_TRY;

  for (k = 0; k < opt->fcount; k++)
  {
    in = &(opt->infiles[k].in);
    if (opt->infiles[k].config == 1)
    {
    	if (processConfigurationFile(opt->infiles[k].configfile, in, &strm) == -1)
        {
          (void) fprintf(stderr, err2, opt->infiles[k].configfile);
          return (-1);
        }
    }

    if (processDataFile(opt->infiles[k].datafile, in, &strm, file_id ) == -1)
    {
      (void) fprintf(stderr, err3, opt->infiles[k].datafile);
      return (-1);
    }

    if (in->inputClass != 5) /* STR */
    {

    for (j=0; j<in->rank;j++)
      numOfElements *= in->sizeOfDimension[j];

    /* disable error reporting */
    H5E_BEGIN_TRY {
        /* create parent groups */
        if(in->path.count > 1) {
            j = 0;
            handle = file_id;
            while(j < in->path.count - 1) {
                if((group_id = H5Gopen2(handle, in->path.group[j], H5P_DEFAULT)) < 0) {
                  group_id = H5Gcreate2(handle, in->path.group[j++], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                  for (; j < in->path.count - 1; j++)
                    group_id = H5Gcreate2(group_id, in->path.group[j], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                  handle = group_id;
                  break;
                }
                handle = group_id;
                j++;
            }
        }
        else {
          handle = file_id;
          j=0;
        }

    /*enable error reporting */
    } H5E_END_TRY;

    /*create data type */
    intype = createInputDataType(in);
    outtype = createOutputDataType(in);

    /* create property list */
    proplist = H5Pcreate (H5P_DATASET_CREATE);
    if (in->configOptionVector[CHUNK] == 1)
    {
      H5Pset_layout (proplist, H5D_CHUNKED);
      /* not reqd chunking is implied if set_chunk is used  */
      H5Pset_chunk (proplist, in->rank, in->sizeOfChunk);
    }

    if (in->configOptionVector[COMPRESS] == 1)
    {
      H5Pset_deflate (proplist, (unsigned) in->compressionParam);
    }

    if (in->configOptionVector[EXTERNAL] == 1)
    {
      /* creating the external file if it doesnt exist */
      if ((extfile = HDfopen(in->externFilename, "ab")) == NULL)
      {
        (void) fprintf(stderr, "%s", err4);
        H5Pclose(proplist);
        H5Sclose(dataspace);
        H5Fclose(file_id);
        return (-1);
      }
      HDfclose(extfile);
      H5Pset_external (proplist, in->externFilename, (off_t)0, numOfElements * in->inputSize / 8);
    }

    /* create dataspace */
    if(in->configOptionVector[EXTEND] == 1)
      dataspace = H5Screate_simple(in->rank, in->sizeOfDimension, in->maxsizeOfDimension);
    else
      dataspace = H5Screate_simple(in->rank, in->sizeOfDimension, NULL);

    /* disable error reporting */
    H5E_BEGIN_TRY {
    /* create data set */
    if((dataset = H5Dcreate2(handle, in->path.group[j], outtype, dataspace, H5P_DEFAULT, proplist, H5P_DEFAULT)) < 0) {
      (void)fprintf(stderr, "%s", err5);
      H5Pclose(proplist);
      H5Sclose(dataspace);
      H5Fclose(file_id);
      return (-1);
    }

    /*enable error reporting */
    } H5E_END_TRY;

     /* write dataset */
    if(H5Dwrite(dataset, intype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (VOIDP)in->data) < 0) {
      (void) fprintf(stderr, "%s", err6);
      H5Dclose(dataset);
      H5Pclose(proplist);
      H5Sclose(dataspace);
      H5Fclose(file_id);
      return (-1);
    }

    H5Dclose(dataset);
    H5Pclose(proplist);
    H5Sclose(dataspace);
  }

  } /* STR */


  H5Fclose(file_id);
  return (0);
}

/*
 * Name:
 *      help
 *
 * Purpose:
 *      Print a helpful summary of command usage and features.
 */

void
help(char *name)
{
  (void) fprintf(stdout, "Name:\n\n");
  (void) fprintf(stdout, "\t%s\n\n", name);
  (void) fprintf(stdout, "\t  TOOL NAME:\n");
  (void) fprintf(stdout, "\t   %s\n", name);
  (void) fprintf(stdout, "\t   SYNTAX:\n");
  (void) fprintf(stdout, "\t   %s -h[elp], OR\n", name);
  (void) fprintf(stdout, "\t   %s <infile> -c[onfig] <configfile> [<infile> -c[config] <configfile>...]", name);
  (void) fprintf(stdout, "\t\t\t\t      -o[utfile] <outfile>\n\n");
  (void) fprintf(stdout, "\t   PURPOSE:\n");
  (void) fprintf(stdout, "\t 	To convert data stored in one or more ASCII or binary files\n");
  (void) fprintf(stdout, "\t	into one or more datasets (in accordance with the \n");
  (void) fprintf(stdout, "\t	user-specified type and storage properties) in an existing \n");
  (void) fprintf(stdout, "\t	or new HDF5 file.\n\n");
  (void) fprintf(stdout, "\t   DESCRIPTION:\n");
  (void) fprintf(stdout, "\t	The primary objective of the utility is to convert floating\n");
  (void) fprintf(stdout, "\t	point or integer data stored in ASCII text or binary form \n");
  (void) fprintf(stdout, "\t	into a data-set according to the type and storage properties\n");
  (void) fprintf(stdout, "\t	specified by the user. The utility can also accept ASCII\n");
  (void) fprintf(stdout, "\t	text files and store the contents in a compact form as an\n");
  (void) fprintf(stdout, "\t	array of one-dimensional strings.\n\n");
  (void) fprintf(stdout, "\t	The input data to be written as a data-set can be provided\n");
  (void) fprintf(stdout, "\t	to the utility in one of the following forms:\n");
  (void) fprintf(stdout, "\t	1. ASCII text file with numeric data (floating point or \n");
  (void) fprintf(stdout, "\t	integer data). \n");
  (void) fprintf(stdout, "\t	2. Binary file with native floating point data (32-bit or \n");
  (void) fprintf(stdout, "\t	64-bit) \n");
  (void) fprintf(stdout, "\t	3. Binary file with native integer (signed or unsigned)\n");
  (void) fprintf(stdout, "\t	data (8-bit or 16-bit or 32-bit or 64-bit). \n");
  (void) fprintf(stdout, "\t	4. ASCII text file containing strings (text data).\n");
  (void) fprintf(stdout, "\t    \n");
  (void) fprintf(stdout, "\t	Every input file is associated with a configuration file \n");
  (void) fprintf(stdout, "\t	also provided as an input to the utility. (See Section \n");
  (void) fprintf(stdout, "\t	\"CONFIGURATION FILE\" to know how it is to be organized).\n");
  (void) fprintf(stdout, "\t	The class, size and dimensions of the input data is \n");
  (void) fprintf(stdout, "\t	specified in this configuration file. A point to note is\n");
  (void) fprintf(stdout, "\t	that the floating point data in the ASCII text file may be\n");
  (void) fprintf(stdout, "\t	organized in the fixed floating form (for example 323.56)\n");
  (void) fprintf(stdout, "\t	or in a scientific notation (for example 3.23E+02). A \n");
  (void) fprintf(stdout, "\t	different input-class specification is to be used for both\n");
  (void) fprintf(stdout, "\t	forms.\n\n");
  (void) fprintf(stdout, "\t	The utility extracts the input data from the input file \n");
  (void) fprintf(stdout, "\t	according to the specified parameters and saves it into \n");
  (void) fprintf(stdout, "\t	an H5 dataset. \n\n");
  (void) fprintf(stdout, "\t	The user can specify output type and storage properties in \n");
  (void) fprintf(stdout, "\t	the configuration file. The user is requited to specify the \n");
  (void) fprintf(stdout, "\t	path of the dataset. If the groups in the path leading to \n");
  (void) fprintf(stdout, "\t	the data-set do not exist, the groups will be created by the\n");
  (void) fprintf(stdout, "\t	utility. If no group is specified, the dataset will be\n");
  (void) fprintf(stdout, "\t	created under the root group.\n\n");
  (void) fprintf(stdout, "\t	In addition to the name, the user is also required to \n");
  (void) fprintf(stdout, "\t	provide the class and size of output data to be written to \n");
  (void) fprintf(stdout, "\t	the dataset and may optionally specify the output-architecure,\n");
  (void) fprintf(stdout, "\t	and the output-byte-order. If output-architecture is not \n");
  (void) fprintf(stdout, "\t	specified the default is NATIVE. Output-byte-orders are fixed\n");
  (void) fprintf(stdout, "\t	for some architectures and may be specified only if output-\n");
  (void) fprintf(stdout, "\t	architecture is IEEE, UNIX or STD.\n\n");
  (void) fprintf(stdout, "\t 	Also, layout and other storage properties such as \n");
  (void) fprintf(stdout, "\t	compression, external storage and extendible data-sets may be\n");
  (void) fprintf(stdout, "\t	optionally specified.  The layout and storage properties \n");
  (void) fprintf(stdout, "\t	denote how raw data is to be organized on the disk. If these \n");
  (void) fprintf(stdout, "\t	options are not specified the default is Contiguous layout \n");
  (void) fprintf(stdout, "\t	and storage.\n\n");
  (void) fprintf(stdout, "\t	The dataset can be organized in any of the following ways:\n");
  (void) fprintf(stdout, "\t	1. Contiguous.\n");
  (void) fprintf(stdout, "\t	2. Chunked.\n");
  (void) fprintf(stdout, "\t	3. External Storage File    (has to be contiguous)\n");
  (void) fprintf(stdout, "\t	4. Extendible data sets     (has to be chunked)\n");
  (void) fprintf(stdout, "\t	5. Compressed.		    (has to be chunked)\n");
  (void) fprintf(stdout, "\t	6. Compressed & Extendible  (has to be chunked)\n\n");
  (void) fprintf(stdout, "\t	If the user wants to store raw data in a non-HDF file then \n");
  (void) fprintf(stdout, "\t	the external storage file option is to be used and the name \n");
  (void) fprintf(stdout, "\t	of the file is to be specified. \n\n");
  (void) fprintf(stdout, "\t	If the user wants the dimensions of the data-set to be\n");
  (void) fprintf(stdout, "\t	unlimited, the extendible data set option can be chosen. \n\n");
  (void) fprintf(stdout, "\t	The user may also specify the type of compression and the \n");
  (void) fprintf(stdout, "\t	level to which the data set must be compresses by setting \n");
  (void) fprintf(stdout, "\t	the compressed option.\n\n");
  (void) fprintf(stdout, "\t   SYNOPSIS:\n");
  (void) fprintf(stdout, "\t  h5import -h[elp], OR\n");
  (void) fprintf(stdout, "\t  h5import <infile> -c[onfig] <configfile> \
                    [<infile> -c[config] <confile2>...] -o[utfile] <outfile>\n\n");
  (void) fprintf(stdout, "\t   -h[elp]:\n");
  (void) fprintf(stdout, "\t           Prints this summary of usage, and exits.\n\n");
  (void) fprintf(stdout, "\t   <infile(s)>:\n");
  (void) fprintf(stdout, "\t           Name of the Input file(s), containing a \n");
  (void) fprintf(stdout, "\t		single n-dimensional floating point or integer array \n");
  (void) fprintf(stdout, "\t		in either ASCII text, native floating point(32-bit \n");
  (void) fprintf(stdout, "\t		or 64-bit) or native integer(8-bit or 16-bit or \n");
  (void) fprintf(stdout, "\t		32-bit or 64-bit). Data to be specified in the order\n");
  (void) fprintf(stdout, "\t		of fastest changing dimensions first.\n\n");
  (void) fprintf(stdout, "\t	-c[config] <configfile>:\n");
  (void) fprintf(stdout, "\t		Every input file should be associated with a \n");
  (void) fprintf(stdout, "\t		configuration file and this is done by the -c option.\n");
  (void) fprintf(stdout, "\t		<configfile> is the name of the configuration file.\n");
  (void) fprintf(stdout, "\t		(See Section \"CONFIGURATION FILE\")\n\n");
  (void) fprintf(stdout, "\t   -o[utfile] <outfile>:\n");
  (void) fprintf(stdout, "\t           Name of the HDF5 output file. Data from one or more \n");
  (void) fprintf(stdout, "\t		input files are stored as one or more data sets in \n");
  (void) fprintf(stdout, "\t		<outfile>. The output file may be an existing file or \n");
  (void) fprintf(stdout, "\t		it maybe new in which case it will be created.\n\n\n");
  (void) fprintf(stdout, "\t   CONFIGURATION FILE:\n");
  (void) fprintf(stdout, "\t	The configuration file is an ASCII text file and must be \n");
  (void) fprintf(stdout, "\t	organized as \"CONFIG-KEYWORD VALUE\" pairs, one pair on each \n");
  (void) fprintf(stdout, "\t	line.\n\n");
  (void) fprintf(stdout, "\t   The configuration file may have the following keywords each \n");
  (void) fprintf(stdout, "\t   followed by an acceptable value.\n\n");
  (void) fprintf(stdout, "\t	Required KEYWORDS:\n");
  (void) fprintf(stdout, "\t		PATH\n");
  (void) fprintf(stdout, "\t		INPUT-CLASS\n");
  (void) fprintf(stdout, "\t		INPUT-SIZE\n");
  (void) fprintf(stdout, "\t		RANK\n");
  (void) fprintf(stdout, "\t		DIMENSION-SIZES\n");
  (void) fprintf(stdout, "\t		OUTPUT-CLASS\n");
  (void) fprintf(stdout, "\t		OUTPUT-SIZE\n\n");
  (void) fprintf(stdout, "\t	Optional KEYWORDS:\n");
  (void) fprintf(stdout, "\t		OUTPUT-ARCHITECTURE\n");
  (void) fprintf(stdout, "\t		OUTPUT-BYTE-ORDER\n");
  (void) fprintf(stdout, "\t  		CHUNKED-DIMENSION-SIZES\n");
  (void) fprintf(stdout, "\t		COMPRESSION-TYPE\n");
  (void) fprintf(stdout, "\t		COMPRESSION-PARAM\n");
  (void) fprintf(stdout, "\t		EXTERNAL-STORAGE\n");
  (void) fprintf(stdout, "\t		MAXIMUM-DIMENSIONS\n\n\n");
  (void) fprintf(stdout, "\t  	Values for keywords:\n");
  (void) fprintf(stdout, "\t		PATH:\n");
  (void) fprintf(stdout, "\t			Strings separated by spaces to represent\n");
  (void) fprintf(stdout, "\t			the path of the data-set. If the groups in\n");
  (void) fprintf(stdout, "\t			the path do no exist, they will be created. \n");
  (void) fprintf(stdout, "\t			For example,\n");
  (void) fprintf(stdout, "\t				PATH grp1/grp2/dataset1\n");
  (void) fprintf(stdout, "\t				PATH: keyword\n");
  (void) fprintf(stdout, "\t				grp1: group under the root. If\n");
  (void) fprintf(stdout, "\t				      non-existent will be created.\n");
  (void) fprintf(stdout, "\t				grp2: group under grp1. If \n");
  (void) fprintf(stdout, "\t				      non-existent will be created \n");
  (void) fprintf(stdout, "\t				      under grp1.\n");
  (void) fprintf(stdout, "\t				dataset1: the name of the data-set \n");
  (void) fprintf(stdout, "\t					  to be created.\n\n");
  (void) fprintf(stdout, "\t               INPUT-CLASS:\n");
  (void) fprintf(stdout, "\t			String denoting the type of input data.\n");
  (void) fprintf(stdout, "\t			(\"TEXTIN\", \"TEXTFP\", \"FP\", \"IN\", \n");
  (void) fprintf(stdout, "\t			\"STR\", \"TEXTUIN\", \"UIN\"). \n");
  (void) fprintf(stdout, "\t			INPUT-CLASS \"TEXTIN\" denotes an ASCII text \n");
  (void) fprintf(stdout, "\t			file with signed integer data in ASCII form,\n");
  (void) fprintf(stdout, "\t			INPUT-CLASS \"TEXTUIN\" denotes an ASCII text \n");
  (void) fprintf(stdout, "\t			file with unsigned integer data in ASCII form,\n");
  (void) fprintf(stdout, "\t			\"TEXTFP\" denotes an ASCII text file containing\n");
  (void) fprintf(stdout, "\t			floating point data in the fixed notation\n");
  (void) fprintf(stdout, "\t			(325.34),\n");
  (void) fprintf(stdout, "\t			\"FP\" denotes a floating point binary file,\n");
  (void) fprintf(stdout, "\t			\"IN\" denotes a signed integer binary file,\n");
  (void) fprintf(stdout, "\t			\"UIN\" denotes an unsigned integer binary file,\n");
  (void) fprintf(stdout, "\t			 & \"STR\" denotes an ASCII text file the \n");
  (void) fprintf(stdout, "\t			contents of which should be stored as an 1-D \n");
  (void) fprintf(stdout, "\t			array of strings.\n");
  (void) fprintf(stdout, "\t			If INPUT-CLASS is \"STR\", then RANK, \n");
  (void) fprintf(stdout, "\t			DIMENSION-SIZES, OUTPUT-CLASS, OUTPUT-SIZE, \n");
  (void) fprintf(stdout, "\t			OUTPUT-ARCHITECTURE and OUTPUT-BYTE-ORDER \n");
  (void) fprintf(stdout, "\t			will be ignored.\n\n\n");
  (void) fprintf(stdout, "\t		INPUT-SIZE:\n");
  (void) fprintf(stdout, "\t			Integer denoting the size of the input data \n");
  (void) fprintf(stdout, "\t			(8, 16, 32, 64). \n\n");
  (void) fprintf(stdout, "\t			For floating point,\n");
  (void) fprintf(stdout, "\t			INPUT-SIZE can be 32 or 64.\n");
  (void) fprintf(stdout, "\t			For integers (signed and unsigned)\n");
  (void) fprintf(stdout, "\t			INPUT-SIZE can be 8, 16, 32 or 64.\n\n");
  (void) fprintf(stdout, "\t		RANK:\n");
  (void) fprintf(stdout, "\t			Integer denoting the number of dimensions.\n\n");
  (void) fprintf(stdout, "\t		DIMENSION-SIZES:\n");
  (void) fprintf(stdout, "\t		        Integers separated by spaces to denote the \n");
  (void) fprintf(stdout, "\t			dimension sizes for the no. of dimensions \n");
  (void) fprintf(stdout, "\t			determined by rank.\n\n");
  (void) fprintf(stdout, "\t		OUTPUT-CLASS:\n");
  (void) fprintf(stdout, "\t			String dentoting data type of the dataset to \n");
  (void) fprintf(stdout, "\t			be written (\"IN\",\"FP\", \"UIN\")\n\n");
  (void) fprintf(stdout, "\t		OUTPUT-SIZE:\n");
  (void) fprintf(stdout, "\t			Integer denoting the size of the data in the \n");
  (void) fprintf(stdout, "\t			output dataset to be written.\n");
  (void) fprintf(stdout, "\t			If OUTPUT-CLASS is \"FP\", OUTPUT-SIZE can be \n");
  (void) fprintf(stdout, "\t			32 or 64.\n");
  (void) fprintf(stdout, "\t			If OUTPUT-CLASS is \"IN\" or \"UIN\", OUTPUT-SIZE\n");
  (void) fprintf(stdout, "\t			can be 8, 16, 32 or 64.\n\n");
  (void) fprintf(stdout, "\t		OUTPUT-ARCHITECTURE:\n");
  (void) fprintf(stdout, "\t			STRING denoting the type of output \n");
  (void) fprintf(stdout, "\t			architecture. Can accept the following values\n");
  (void) fprintf(stdout, "\t			STD\n");
  (void) fprintf(stdout, "\t			IEEE\n");
  (void) fprintf(stdout, "\t			INTEL\n");
  (void) fprintf(stdout, "\t			CRAY\n");
  (void) fprintf(stdout, "\t			MIPS\n");
  (void) fprintf(stdout, "\t			ALPHA\n");
  (void) fprintf(stdout, "\t			NATIVE (default)\n");
  (void) fprintf(stdout, "\t			UNIX\n\n");
  (void) fprintf(stdout, "\t		OUTPUT-BYTE-ORDER:\n");
  (void) fprintf(stdout, "\t			String denoting the output-byte-order. Ignored\n");
  (void) fprintf(stdout, "\t			if the OUTPUT-ARCHITECTURE is not specified or\n");
  (void) fprintf(stdout, "\t			if it is IEEE, UNIX or STD. Can accept the \n");
  (void) fprintf(stdout, "\t			following values.\n");
  (void) fprintf(stdout, "\t			BE (default)\n");
  (void) fprintf(stdout, "\t			LE\n\n");
  (void) fprintf(stdout, "\t		CHUNKED-DIMENSION-SIZES:\n");
  (void) fprintf(stdout, "\t			Integers separated by spaces to denote the \n");
  (void) fprintf(stdout, "\t			dimension sizes of the chunk for the no. of \n");
  (void) fprintf(stdout, "\t			dimensions determined by rank. Required field\n");
  (void) fprintf(stdout, "\t			to denote that the dataset will be stored with\n");
  (void) fprintf(stdout, "\t			chunked storage. If this field is absent the\n");
  (void) fprintf(stdout, "\t			dataset will be stored with contiguous storage.\n\n");
  (void) fprintf(stdout, "\t		COMPRESSION-TYPE:\n");
  (void) fprintf(stdout, "\t			String denoting the type of compression to be\n");
  (void) fprintf(stdout, "\t			used with the chunked storage. Requires the\n");
  (void) fprintf(stdout, "\t			CHUNKED-DIMENSION-SIZES to be specified. The only \n");
  (void) fprintf(stdout, "\t			currently supported compression method is GZIP. \n");
  (void) fprintf(stdout, "\t			Will accept the following value\n");
  (void) fprintf(stdout, "\t			GZIP\n\n");
  (void) fprintf(stdout, "\t		COMPRESSION-PARAM:\n");
  (void) fprintf(stdout, "\t			Integer used to denote compression level and \n");
  (void) fprintf(stdout, "\t			this option is to be always specified when \n");
  (void) fprintf(stdout, "\t			the COMPRESSION-TYPE option is specified. The\n");
  (void) fprintf(stdout, "\t			values are applicable only to GZIP \n");
  (void) fprintf(stdout, "\t			compression.\n");
  (void) fprintf(stdout, "\t			Value 1-9: The level of Compression. \n");
  (void) fprintf(stdout, "\t				1 will result in the fastest \n");
  (void) fprintf(stdout, "\t				compression while 9 will result in \n");
  (void) fprintf(stdout, "\t				the best compression ratio. The default\n");
  (void) fprintf(stdout, "\t				level of compression is 6.\n\n");
  (void) fprintf(stdout, "\t		EXTERNAL-STORAGE:\n");
  (void) fprintf(stdout, "\t			String to denote the name of the non-HDF5 file \n");
  (void) fprintf(stdout, "\t			to store data to. Cannot be used if CHUNKED-\n");
  (void) fprintf(stdout, "\t			DIMENSIONS or COMPRESSION-TYPE or EXTENDIBLE-\n");
  (void) fprintf(stdout, "\t			DATASET is specified.\n");
  (void) fprintf(stdout, "\t			Value <external-filename>: the name of the \n");
  (void) fprintf(stdout, "\t			external file as a string to be used.\n\n");
  (void) fprintf(stdout, "\t		MAXIMUM-DIMENSIONS:\n");
  (void) fprintf(stdout, "\t			Integers separated by spaces to denote the \n");
  (void) fprintf(stdout, "\t			maximum dimension sizes of all the \n");
  (void) fprintf(stdout, "\t			dimensions determined by rank. Requires the\n");
  (void) fprintf(stdout, "\t			CHUNKED-DIMENSION-SIZES to be specified. A value of \n");
  (void) fprintf(stdout, "\t			-1 for any dimension implies UNLIMITED \n");
  (void) fprintf(stdout, "\t			DIMENSION size for that particular dimension.\n\n");
  (void) fprintf(stdout, "\t   EXAMPLES:\n");
  (void) fprintf(stdout, "\t	1. Configuration File may look like:\n\n");
  (void) fprintf(stdout, "\t		PATH work h5 pkamat First-set\n");
  (void) fprintf(stdout, "\t		INPUT-CLASS TEXTFP\n");
  (void) fprintf(stdout, "\t		RANK 3\n");
  (void) fprintf(stdout, "\t		DIMENSION-SIZES 5 2 4\n");
  (void) fprintf(stdout, "\t		OUTPUT-CLASS FP\n");
  (void) fprintf(stdout, "\t		OUTPUT-SIZE 64\n");
  (void) fprintf(stdout, "\t		OUTPUT-ARCHITECTURE IEEE\n");
  (void) fprintf(stdout, "\t		OUTPUT-BYTE-ORDER LE\n");
  (void) fprintf(stdout, "\t  		CHUNKED-DIMENSION-SIZES 2 2 2 \n\n");
  (void) fprintf(stdout, "\t	The above configuration will accept a floating point array \n");
  (void) fprintf(stdout, "\t	(5 x 2 x 4)  in an ASCII file with the rank and dimension sizes \n");
  (void) fprintf(stdout, "\t	specified and will save it in a chunked data-set (of pattern \n");
  (void) fprintf(stdout, "\t	2 X 2 X 2) of 64-bit floating point in the little-endian order \n");
  (void) fprintf(stdout, "\t	and IEEE architecture. The dataset will be stored at\n");
  (void) fprintf(stdout, "\t	\"/work/h5/pkamat/First-set\"\n\n");
  (void) fprintf(stdout, "\t	2. Another configuration could be:\n\n");
  (void) fprintf(stdout, "\t		PATH Second-set\n");
  (void) fprintf(stdout, "\t		INPUT-CLASS IN	\n");
  (void) fprintf(stdout, "\t		RANK 5\n");
  (void) fprintf(stdout, "\t		DIMENSION-SIZES 6 3 5 2 4\n");
  (void) fprintf(stdout, "\t		OUTPUT-CLASS IN\n");
  (void) fprintf(stdout, "\t		OUTPUT-SIZE 32\n");
  (void) fprintf(stdout, "\t  		CHUNKED-DIMENSION-SIZES 2 2 2 2 2\n");
  (void) fprintf(stdout, "\t		EXTENDIBLE-DATASET 1 3 \n");
  (void) fprintf(stdout, "\t		COMPRESSION-TYPE GZIP\n");
  (void) fprintf(stdout, "\t		COMPRESSION-PARAM 7\n\n\n");
  (void) fprintf(stdout, "\t	The above configuration will accept an integer array \n");
  (void) fprintf(stdout, "\t	(6 X 3 X 5 x 2 x 4)  in a binary file with the rank and \n");
  (void) fprintf(stdout, "\t	dimension sizes specified and will save it in a chunked data-set\n");
  (void) fprintf(stdout, "\t	(of pattern 2 X 2 X 2 X 2 X 2) of 32-bit floating point in \n");
  (void) fprintf(stdout, "\t	native format (as output-architecture is not specified). The \n");
  (void) fprintf(stdout, "\t	first and the third dimension will be defined as unlimited. The \n");
  (void) fprintf(stdout, "\t	data-set will be compressed using GZIP and a compression level \n");
  (void) fprintf(stdout, "\t	of 7.\n");
  (void) fprintf(stdout, "\t	The dataset will be stored at \"/Second-set\"\n\n");
  return;
}


void
usage(char *name)
{
  (void) fprintf(stdout, "\nUsage:\t%s -h[elp], OR\n", name);
  (void) fprintf(stdout, "\t%s <infile> -c[onfig] <configfile> \
  [<infile> -c[config] <configfile>...] -o[utfile] <outfile> \n\n", name);
  return;
}

