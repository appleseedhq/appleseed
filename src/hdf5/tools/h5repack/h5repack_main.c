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

#include "h5tools_utils.h"
#include "h5repack.h"

/* Name of tool */
#define PROGRAMNAME "h5repack"

static void usage(const char *prog);
static void parse_command_line(int argc, const char **argv, pack_opt_t* options);
static void read_info(const char *filename,pack_opt_t *options);


/* module-scoped variables */
static int  has_i_o = 0;
const char  *infile  = NULL;
const char  *outfile = NULL;


/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVvf:l:m:e:nLc:d:s:u:b:t:a:i:o:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "filter", require_arg, 'f' },
    { "layout", require_arg, 'l' },
    { "minimum", require_arg, 'm' },
    { "file", require_arg, 'e' },
    { "native", no_arg, 'n' },
    { "latest", no_arg, 'L' },
    { "compact", require_arg, 'c' },
    { "indexed", require_arg, 'd' },
    { "ssize", require_arg, 's' },
    { "ublock", require_arg, 'u' },
    { "block", require_arg, 'b' },
    { "threshold", require_arg, 't' },
    { "alignment", require_arg, 'a' },
    { "infile", require_arg, 'i' },   /* -i for backward compability */
    { "outfile", require_arg, 'o' },  /* -o for backward compability */
    { NULL, 0, '\0' }
};




/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5repack main program
 *
 * Return: Success: EXIT_SUCCESS(0)
 *
 * Failure: EXIT_FAILURE(1)
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *  July 2004: Introduced the extra EC or NN option for SZIP
 *  October 2006: Added a new switch -n, that allows to write the dataset
 *                using a native type. The default to write is the file type.
 *
 * Modification:
 *   Peter Cao, June 13, 2007
 *    Add "-L, --latest" option to pack a file with the latest file format
 *   PVN, November 19, 2007
 *    adopted the syntax h5repack [OPTIONS]  file1 file2
 *   PVN, November 28, 2007
 *    added support for multiple global filters
 *   PVN, May 16, 2008
 *    added  backward compatibility for -i infile -o outfile
 *   PVN, August 20, 2008
 *    add a user block to repacked file (switches -u -b)
 *   PVN, August 28, 2008
 *    add options to set alignment (H5Pset_alignment) (switches -t -a)
 *-------------------------------------------------------------------------
 */
int main(int argc, const char **argv)
{

    pack_opt_t    options;            /*the global options */
    int           ret=-1;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* initialize options  */
    h5repack_init(&options,0);

    parse_command_line(argc, argv, &options);

    /* get file names if they were not yet got */
    if ( has_i_o == 0 )
    {

        if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL )
        {
            infile = argv[ opt_ind ];
            outfile = argv[ opt_ind + 1 ];

            if ( HDstrcmp( infile, outfile ) == 0 )
            {
                error_msg("file names cannot be the same\n");
                usage(h5tools_getprogname());
                exit(EXIT_FAILURE);

            }
        }

        else
        {
            error_msg("file names missing\n");
            usage(h5tools_getprogname());
            exit(EXIT_FAILURE);
        }
    }


    /* pack it */
    ret=h5repack(infile,outfile,&options);

    /* free tables */
    h5repack_end(&options);

    if (ret==-1)
        return 1;
    else
        return 0;
}



/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void usage(const char *prog)
{
 printf("usage: %s [OPTIONS] file1 file2\n", prog);
 printf("  file1                    Input HDF5 File\n");
 printf("  file2                    Output HDF5 File\n");
 printf("  OPTIONS\n");
 printf("   -h, --help              Print a usage message and exit\n");
 printf("   -v, --verbose           Verbose mode, print object information\n");
 printf("   -V, --version           Print version number and exit\n");
 printf("   -n, --native            Use a native HDF5 type when repacking\n");

 printf("   -L, --latest            Use latest version of file format\n");
 printf("   -c L1, --compact=L1     Maximum number of links in header messages\n");
 printf("   -d L2, --indexed=L2     Minimum number of links in the indexed format\n");
 printf("   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");

 printf("   -m M, --minimum=M       Do not apply the filter to datasets smaller than M\n");
 printf("   -e E, --file=E          Name of file E with the -f and -l options\n");

 printf("   -u U, --ublock=U        Name of file U with user block data to be added\n");
 printf("   -b B, --block=B         Size of user block to be added\n");
 printf("   -t T, --threshold=T     Threshold value for H5Pset_alignment\n");
 printf("   -a A, --alignment=A     Alignment value for H5Pset_alignment\n");

 printf("   -f FILT, --filter=FILT  Filter type\n");
 printf("   -l LAYT, --layout=LAYT  Layout type\n");

 printf("\n");

 printf("    M - is an integer greater than 1, size of dataset in bytes (default is 1024) \n");
 printf("    E - is a filename.\n");
 printf("    S - is an integer\n");
 printf("    U - is a filename.\n");
 printf("    T - is an integer\n");
 printf("    A - is an integer greater than zero\n");
 printf("    B - is the user block size, any value that is 512 or greater and is\n");
 printf("        a power of 2 (1024 default)\n");
 printf("    F - is the shared object header message type, any of <dspace|dtype|fill|\n");
 printf("        pline|attr>. If F is not specified, S applies to all messages\n");

 printf("\n");

 printf("    FILT - is a string with the format:\n");
 printf("\n");
 printf("      <list of objects>:<name of filter>=<filter parameters>\n");
 printf("\n");
 printf("      <list of objects> is a comma separated list of object names, meaning apply\n");
 printf("        compression only to those objects. If no names are specified, the filter\n");
 printf("        is applied to all objects\n");
 printf("      <name of filter> can be:\n");
 printf("        GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
 printf("        SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
 printf("        SHUF, to apply the HDF5 shuffle filter\n");
 printf("        FLET, to apply the HDF5 checksum filter\n");
 printf("        NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
 printf("        SOFF, to apply the HDF5 Scale/Offset filter\n");
 printf("        NONE, to remove all filters\n");
 printf("      <filter parameters> is optional filter parameter information\n");
 printf("        GZIP=<deflation level> from 1-9\n");
 printf("        SZIP=<pixels per block,coding> pixels per block is a even number in\n");
 printf("            2-32 and coding method is either EC or NN\n");
 printf("        SHUF (no parameter)\n");
 printf("        FLET (no parameter)\n");
 printf("        NBIT (no parameter)\n");
 printf("        SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
 printf("            is either IN or DS\n");
 printf("        NONE (no parameter)\n");
 printf("\n");
 printf("    LAYT - is a string with the format:\n");
 printf("\n");
 printf("      <list of objects>:<layout type>=<layout parameters>\n");
 printf("\n");
 printf("      <list of objects> is a comma separated list of object names, meaning that\n");
 printf("        layout information is supplied for those objects. If no names are\n");
 printf("        specified, the layout type is applied to all objects\n");
 printf("      <layout type> can be:\n");
 printf("        CHUNK, to apply chunking layout\n");
 printf("        COMPA, to apply compact layout\n");
 printf("        CONTI, to apply continuous layout\n");
 printf("      <layout parameters> is optional layout information\n");
 printf("        CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
 printf("        COMPA (no parameter)\n");
 printf("        CONTI (no parameter)\n");
 printf("\n");
 printf("Examples of use:\n");
 printf("\n");
 printf("1) h5repack -v -f GZIP=1 file1 file2\n");
 printf("\n");
 printf("   GZIP compression with level 1 to all objects\n");
 printf("\n");
 printf("2) h5repack -v -f A:SZIP=8,NN file1 file2\n");
 printf("\n");
 printf("   SZIP compression with 8 pixels per block and NN coding method to object A\n");
 printf("\n");
 printf("3) h5repack -v -l A,B:CHUNK=20x10 -f C,D,F:NONE file1 file2\n");
 printf("\n");
 printf("   Chunked layout, with a layout size of 20x10, to objects A and B\n");
 printf("   and remove filters to objects C, D, F\n");
 printf("\n");
 printf("4) h5repack -L -c 10 -s 20:dtype file1 file2 \n");
 printf("\n");
 printf("   Using latest file format with maximum compact group size of 10 and\n");
 printf("   and minimum shared datatype size of 20\n");
 printf("\n");
 printf("5) h5repack -f SHUF -f GZIP=1 file1 file2 \n");
 printf("\n");
 printf("   Add both filters SHUF and GZIP in this order to all datasets\n");
 printf("\n");
}


/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

static
void parse_command_line(int argc, const char **argv, pack_opt_t* options)
{

    int opt;

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF)
    {
        switch ((char)opt)
        {

        /* -i for backward compability */
        case 'i':
            infile = opt_arg;
            has_i_o = 1;
            break;
        /* -o for backward compability */
        case 'o':
            outfile = opt_arg;
            has_i_o = 1;
            break;


        case 'h':
            usage(h5tools_getprogname());
            exit(EXIT_SUCCESS);
        case 'V':
            print_version(h5tools_getprogname());
            exit(EXIT_SUCCESS);
        case 'v':
            options->verbose = 1;
            break;
        case 'f':

            /* parse the -f filter option */
            if (h5repack_addfilter( opt_arg, options)<0)
            {
                error_msg("in parsing filter\n");
                exit(EXIT_FAILURE);
            }
            break;
        case 'l':

            /* parse the -l layout option */
            if (h5repack_addlayout( opt_arg, options)<0)
            {
                error_msg("in parsing layout\n");
                exit(EXIT_FAILURE);
            }
            break;


        case 'm':

            options->min_comp = atoi( opt_arg );
            if ((int)options->min_comp<=0)
            {
                error_msg("invalid minimum compress size <%s>\n", opt_arg );
                exit(EXIT_FAILURE);
            }
            break;

        case 'e':
            read_info( opt_arg, options);
            break;

        case 'n':
            options->use_native = 1;
            break;

        case 'L':
            options->latest = 1;
            break;

        case 'c':

            options->grp_compact = atoi( opt_arg );
            if (options->grp_compact>0)
                options->latest = 1; /* must use latest format */
            break;


        case 'd':

            options->grp_indexed = atoi( opt_arg );
            if (options->grp_indexed>0)
                options->latest = 1; /* must use latest format */
            break;

        case 's':

            {

                int idx = 0;
                int ssize = 0;
                char *msgPtr = HDstrchr( opt_arg, ':');
                options->latest = 1; /* must use latest format */
                if (msgPtr == NULL)
                {
                    ssize = atoi( opt_arg );
                    for (idx=0; idx<5; idx++)
                        options->msg_size[idx] = ssize;
                }
                else
                {
                    char msgType[10];
                    HDstrcpy(msgType, msgPtr+1);
                    msgPtr[0] = '\0';
                    ssize = atoi( opt_arg );
                    if (HDstrncmp(msgType, "dspace",6) == 0) {
                        options->msg_size[0] = ssize;
                    }
                    else if (HDstrncmp(msgType, "dtype", 5) == 0) {
                        options->msg_size[1] = ssize;
                    }
                    else if (HDstrncmp(msgType, "fill", 4) == 0) {
                        options->msg_size[2] = ssize;
                    }
                    else if (HDstrncmp(msgType, "pline", 5) == 0) {
                        options->msg_size[3] = ssize;
                    }
                    else if (HDstrncmp(msgType, "attr", 4) == 0) {
                        options->msg_size[4] = ssize;
                    }
                }
            }

            break;


        case 'u':

            options->ublock_filename = opt_arg;
            break;

        case 'b':

            options->ublock_size = (hsize_t)atol( opt_arg );
            break;

        case 't':

            options->threshold = (hsize_t)atol( opt_arg );
            break;

        case 'a':

            options->alignment = atol( opt_arg );
            if ( options->alignment < 1 )
            {
                error_msg("invalid alignment size\n", opt_arg );
                exit(EXIT_FAILURE);
            }
            break;

        } /* switch */


    } /* while */

    if ( has_i_o == 0 )
    {
        /* check for file names to be processed */
        if (argc <= opt_ind || argv[ opt_ind + 1 ] == NULL)
        {
            error_msg("missing file names\n");
            usage(h5tools_getprogname());
            exit(EXIT_FAILURE);
        }
    }

}

/*-------------------------------------------------------------------------
 * Function: read_info
 *
 * Purpose: read comp and chunk options from a file
 *
 * Return: void, exit on error
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September, 22, 2003
 *
 *-------------------------------------------------------------------------
 */

static
void read_info(const char *filename,
               pack_opt_t *options)
{

    char stype[10];
    char comp_info[1024];
    FILE *fp;
    char c;
    int  i, rc=1;
    char  *srcdir = getenv("srcdir"); /* the source directory */
    char  data_file[512]="";          /* buffer to hold name of existing file */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    if (srcdir){
        strcpy(data_file,srcdir);
        strcat(data_file,"/");
    }
    strcat(data_file,filename);


    if ((fp = fopen(data_file, "r")) == (FILE *)NULL) {
        error_msg("cannot open options file %s\n", filename);
        exit(EXIT_FAILURE);
    }

    /* cycle until end of file reached */
    while( 1 )
    {
        rc=fscanf(fp, "%s", stype);
        if (rc==-1)
            break;

       /*-------------------------------------------------------------------------
        * filter
        *-------------------------------------------------------------------------
        */
        if (HDstrcmp(stype,"-f") == 0) {

            /* find begining of info */
            i=0; c='0';
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                if (feof(fp)) break;
            }
            c='0';
            /* go until end */
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                comp_info[i]=c;
                i++;
                if (feof(fp)) break;
                if (c==10 /*eol*/) break;
            }
            comp_info[i-1]='\0'; /*cut the last " */

            if (h5repack_addfilter(comp_info,options)==-1){
                error_msg("could not add compression option\n");
                exit(EXIT_FAILURE);
            }
        }
        /*-------------------------------------------------------------------------
        * layout
        *-------------------------------------------------------------------------
        */
        else if (HDstrcmp(stype,"-l") == 0) {

            /* find begining of info */
            i=0; c='0';
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                if (feof(fp)) break;
            }
            c='0';
            /* go until end */
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                comp_info[i]=c;
                i++;
                if (feof(fp)) break;
                if (c==10 /*eol*/) break;
            }
            comp_info[i-1]='\0'; /*cut the last " */

            if (h5repack_addlayout(comp_info,options)==-1){
                error_msg("could not add chunck option\n");
                exit(EXIT_FAILURE);
            }
        }
        /*-------------------------------------------------------------------------
        * not valid
        *-------------------------------------------------------------------------
        */
        else {
            error_msg("bad file format for %s", filename);
            exit(EXIT_FAILURE);
        }
    }

    fclose(fp);
    return;
}
