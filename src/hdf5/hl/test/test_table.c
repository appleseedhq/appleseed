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

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5TBpublic.h"

#define TEST_FILE_BE "test_table_be.hdf5"
#define TEST_FILE_LE "test_table_le.hdf5"
#define TEST_FILE_CRAY "test_table_cray.hdf5"


/*-------------------------------------------------------------------------
* Table API test
*
* Functions tested:
*
* H5TBmake_table
* H5TBread_table
* H5TBwrite_records
* H5TBread_records
* H5TBappend_records
* H5TBinsert_record
* H5TBdelete_record
* H5TBcombine_tables
* H5TBwrite_fields_name
* H5TBread_fields_name
* H5TBwrite_fields_index
* H5TBinsert_field
* H5TBdelete_field
* H5TBget_table_info
* H5TBget_field_info
*
*-------------------------------------------------------------------------
*/

#define TITLE           "Title"
#define NFIELDS          5
#define NRECORDS         8
#define NRECORDS_ADD     3

#define TESTING2(WHAT) {printf("%-70s", "Testing     " WHAT); fflush(stdout);}

/*-------------------------------------------------------------------------
* structure used for all tests, a particle with properties
*-------------------------------------------------------------------------
*/
typedef struct particle_t
{
    char   name[16];
    long   longi;
    float  pressure;
    double temperature;
    int    lati;
} particle_t;

/*-------------------------------------------------------------------------
* a subset of particle_t, with latitude and longitude fields
*-------------------------------------------------------------------------
*/
typedef struct position_t
{
    long   longi;
    int    lati;
} position_t;

/*-------------------------------------------------------------------------
* a subset of particle_t, with name and pressure fields
*-------------------------------------------------------------------------
*/
typedef struct namepressure_t
{
    char   name[16];
    float  pressure;
} namepressure_t;

/*-------------------------------------------------------------------------
* an extended particle, used in the insert field test
*-------------------------------------------------------------------------
*/
typedef struct particle2_t
{
    char   name[16];
    long   longi;
    float  pressure;
    double temperature;
    int    lati;
    int    new_field;
} particle2_t;

/*-------------------------------------------------------------------------
* a particle with one field less, used in the delete field test
*-------------------------------------------------------------------------
*/
typedef struct particle3_t
{
    char   name[16];
    long   longi;
    double temperature;
    int    lati;
} particle3_t;



/*-------------------------------------------------------------------------
* function to open an HDF5 file and return its file identifier
*-------------------------------------------------------------------------
*/
static hid_t h5file_open(const char *fname, unsigned flags)
{

    hid_t fid;                        /* identifier for the file */
    char  *srcdir = getenv("srcdir"); /* the source directory */
    char  data_file[512]="";          /* buffer to hold name of existing file */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    if (srcdir)
    {
        strcpy(data_file,srcdir);
        strcat(data_file,"/");
    }
    strcat(data_file,fname);

    /* open */
    if ((fid = H5Fopen(data_file,flags,H5P_DEFAULT))<0)
    {
        fprintf(stderr,"Error: Cannot open file <%s>\n",data_file );
        exit(1);
    }

    return fid;
}

/*-------------------------------------------------------------------------
* function that compares one particle
*-------------------------------------------------------------------------
*/
static int cmp_par(hsize_t i, hsize_t j, particle_t *rbuf, particle_t *wbuf )
{
    if ( ( strcmp( rbuf[i].name, wbuf[j].name ) != 0 ) ||
        rbuf[i].lati != wbuf[j].lati ||
        rbuf[i].longi != wbuf[j].longi ||
        rbuf[i].pressure != wbuf[j].pressure ||
        rbuf[i].temperature != wbuf[j].temperature )
    {
        fprintf(stderr,"read and write buffers have differences\n");
        fprintf(stderr,"%s %ld %f %f %d\n",
            rbuf[i].name,rbuf[i].longi,rbuf[i].pressure,rbuf[i].temperature,rbuf[i].lati);
        fprintf(stderr,"%s %ld %f %f %d\n",
            wbuf[j].name,wbuf[j].longi,wbuf[j].pressure,wbuf[j].temperature,wbuf[j].lati);
        return -1;
    }
    return 0;
}

/*-------------------------------------------------------------------------
* function to compare deleted records
*-------------------------------------------------------------------------
*/
static int compare_deleted(hsize_t rrecords, hsize_t dstart, hsize_t drecords,
                           particle_t *rbuf, particle_t *wbuf)
{
    hsize_t i,j;
    for( i=0; i<rrecords; i++)
    {
        if (i<dstart)
        {
            if (cmp_par(i,i,rbuf,wbuf)<0)
                return -1;
        }
        else
        {
            j=i+drecords;
            if (cmp_par(i,j,rbuf,wbuf)<0)
                return -1;
        }
    }
    return 0;
}



/*-------------------------------------------------------------------------
* the test program
*-------------------------------------------------------------------------
*/

static int test_table(hid_t fid, int do_write)
{
    hid_t       fid1;
    hid_t       fid2;
    hsize_t     chunk_size=10;
    int         compress=0;
    int         *fill=NULL;
    particle_t  fill1[1] = { {"no data",-1, -99.0f, -99.0, -1} };
    int         fill1_new[1] = { -100 };
    hsize_t     position;
    char        tname[20];
    hsize_t     i, j;
    /* write, read, append, delete, insert some records and fields */
    hsize_t FIELDS   = NFIELDS;
    hsize_t RECORDS  = NRECORDS;
    hsize_t start;
    hsize_t wstart;
    hsize_t rstart;
    hsize_t istart;
    hsize_t dstart;
    hsize_t nrecords;
    hsize_t rrecords;
    hsize_t wrecords;
    hsize_t arecords;
    hsize_t irecords;
    hsize_t drecords;
    hsize_t nfields;
    hsize_t rfields;
    hsize_t start1;      /* record to start reading from 1st table */
    hsize_t start2;      /* record to start writing in 2nd table */
    /* read, write, insert, append buffers */
    particle_t  rbuf[NRECORDS+4];
    particle2_t rbuf2[NRECORDS];
    particle3_t rbuf3[NRECORDS];
    particle_t  rbufc[NRECORDS*2];
    particle_t  abuf[2]={{"eight",80,8.0f,80.0,80},{"nine",90,9.0f,90.0,90}};
    particle_t  ibuf[2]={{"zero", 0, 0.0f, 0.0, 0},{"zero", 0, 0.0f, 0.0, 0}};
    particle_t  wbufd[NRECORDS];
    particle_t  wbuf[NRECORDS] = {
        {"zero", 0, 0.0f, 0.0, 0,},
        {"one",  10, 1.0f, 10.0, 10},
        {"two",  20, 2.0f, 20.0, 20},
        {"three",30, 3.0f, 30.0, 30},
        {"four", 40, 4.0f, 40.0, 40},
        {"five", 50, 5.0f, 50.0, 50},
        {"six",  60, 6.0f, 60.0, 60},
        {"seven",70, 7.0f, 70.0, 70}
    };
    /* buffers for the field "Pressure" and "New_field" */
    float           pressure_in [NRECORDS] = { 0.0f,1.0f,2.0f,3.0f,4.0f,5.0f,6.0f,7.0f };
    float           pressure_out [NRECORDS];
    int             buf_new[NRECORDS] = { 0,1,2,3,4,5,6,7 };
    /* buffers for the fields "Latitude,Longitude"  */
    position_t      position_out[NRECORDS_ADD];
    position_t      position_in[NRECORDS_ADD] = { {0,0},
    {10,10},
    {20,20}};
    /* buffers for the fields "Name,Pressure"  */
    namepressure_t   namepre_out[NRECORDS];
    namepressure_t   namepre_in[NRECORDS] =
    { {"zero",0.0f},
    {"one",   1.0f},
    {"two",   2.0f},
    {"three", 3.0f},
    {"four",  4.0f},
    {"five",  5.0f},
    {"six",   6.0f},
    {"seven", 7.0f},
    };


    /*-------------------------------------------------------------------------
    * initialize table parameters
    * field offsets and sizes used in the fields functions
    *-------------------------------------------------------------------------
    */

    size_t field_offset_pos[2]=
    {
        HOFFSET( position_t, longi ),
        HOFFSET( position_t, lati )
    };
    size_t field_offset_namepre[2]=
    {
        HOFFSET( namepressure_t, name ),
        HOFFSET( namepressure_t, pressure )
    };
    size_t field_sizes_pos[2]=
    {
        sizeof(position_in[0].longi),
        sizeof(position_in[0].lati)
    };
    size_t field_sizes_namepre[2]=
    {
        sizeof(namepre_in[0].name),
        sizeof(namepre_in[0].pressure)
    };
    size_t field_sizes_pre[1]=
    {
        sizeof(namepre_in[0].pressure)
    };

    /*-------------------------------------------------------------------------
    * query table test
    *-------------------------------------------------------------------------
    */
    char    **names_out;
    size_t  sizes_out[NFIELDS];
    size_t  offset_out[NFIELDS];
    size_t  size_out;

    /*-------------------------------------------------------------------------
    * initialize table parameters
    * field indexes (zero based) used in the fields functions
    * "Name 0","Longitude 1","Pressure 2","Temperature 3","Latitude 4"
    *-------------------------------------------------------------------------
    */
    int    field_index_pre[1]     = { 2 };
    int    field_index_pos[2]     = { 1,4 };
    int    field_index_namepre[2] = { 0,2 };
    int    field_index[NFIELDS]   = { 0,1,2,3,4 };

    /*-------------------------------------------------------------------------
    * initialize table parameters
    * size and the offsets of struct members in memory
    * define the inserted field HDF5 type and buffers
    * these are used for the insert field test
    *-------------------------------------------------------------------------
    */
    hid_t   field_type_new = H5T_NATIVE_INT;
    size_t  dst_size2 =  sizeof( particle2_t );
    size_t  dst_offset2[NFIELDS+1] = { HOFFSET( particle2_t, name ),
        HOFFSET( particle2_t, longi ),
        HOFFSET( particle2_t, pressure ),
        HOFFSET( particle2_t, temperature ),
        HOFFSET( particle2_t, lati ),
        HOFFSET( particle2_t, new_field )};
    size_t dst_sizes2[NFIELDS+1] = { sizeof( rbuf2[0].name),
        sizeof( rbuf2[0].longi),
        sizeof( rbuf2[0].pressure),
        sizeof( rbuf2[0].temperature),
        sizeof( rbuf2[0].lati),
        sizeof( rbuf2[0].new_field)};
    /*-------------------------------------------------------------------------
    * initialize table parameters
    * size and the offsets of struct members in memory
    * these are used for the delete field test
    *-------------------------------------------------------------------------
    */
    size_t dst_size3 =  sizeof( particle3_t );
    size_t dst_offset3[NFIELDS-1] = { HOFFSET( particle3_t, name ),
        HOFFSET( particle3_t, longi ),
        HOFFSET( particle3_t, temperature ),
        HOFFSET( particle3_t, lati )};

    size_t dst_sizes3[NFIELDS-1] = { sizeof( rbuf3[0].name),
        sizeof( rbuf3[0].longi),
        sizeof( rbuf3[0].temperature),
        sizeof( rbuf3[0].lati)};


    /*-------------------------------------------------------------------------
    * initialize table parameters
    * 1) size and the offsets of struct members in memory
    * 2) field names
    * 3) define a HDF5 type for the fields
    *-------------------------------------------------------------------------
    */

    size_t type_size_mem =  sizeof( particle_t );
    size_t field_offset[NFIELDS]=
    {
        HOFFSET( particle_t, name ),
        HOFFSET( particle_t, longi ),
        HOFFSET( particle_t, pressure ),
        HOFFSET( particle_t, temperature ),
        HOFFSET( particle_t, lati )
    };
    size_t field_size[NFIELDS] =
    {
        sizeof( rbuf[0].name),
        sizeof( rbuf[0].longi),
        sizeof( rbuf[0].pressure),
        sizeof( rbuf[0].temperature),
        sizeof( rbuf[0].lati)
    };
    const char *field_names[NFIELDS]  =
    { "Name","Longitude","Pressure","Temperature","Latitude" };
    hid_t field_type[NFIELDS];
    hid_t string_type = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type, 16 );
    field_type[0] = string_type;
    field_type[1] = H5T_NATIVE_LONG;
    field_type[2] = H5T_NATIVE_FLOAT;
    field_type[3] = H5T_NATIVE_DOUBLE;
    field_type[4] = H5T_NATIVE_INT;

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBmake_table
    * H5TBread_table
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("making table");

        if (H5TBmake_table(TITLE,fid,"table1",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;
        PASSED();
    }

    TESTING2("reading table");

    /*-------------------------------------------------------------------------
    * read the table
    *-------------------------------------------------------------------------
    */

    if (H5TBread_table(fid,"table1",type_size_mem,field_offset,field_size,rbuf)<0)
        goto out;

    /* compare the extracted table with the original array */
    for( i = 0; i < NRECORDS; i++ )
    {
        if (cmp_par(i,i,rbuf,wbuf)<0)
            goto out;
    }

    PASSED();


    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBwrite_records
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("writing records");

        /* create an empty table */
        if (H5TBmake_table(TITLE,fid,"table2",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,0)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * write records, start at 0, write 8
        * pos = 0 1 2 3 4 5 6 7
        * data= 0 1 2 3 4 5 6 7
        *-------------------------------------------------------------------------
        */
        wstart=0;
        wrecords=8;
        if (H5TBwrite_records(fid,"table2",wstart,wrecords,type_size_mem,field_offset,
            field_size,wbuf)<0)
            goto out;

        /* read it back */
        if (H5TBread_table(fid,"table2",type_size_mem,field_offset,field_size,rbuf)<0)
            goto out;

        /* compare  */
        for( i = 0; i < NRECORDS; i++ )
        {
            if (cmp_par(i,i,rbuf,wbuf)<0)
                goto out;
        }

        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBread_records
    *
    *-------------------------------------------------------------------------
    */

    TESTING2("reading records");

    /*-------------------------------------------------------------------------
    * read records, start at 0, read 8
    * pos = 0 1 2 3 4 5 6 7
    * data= 0 1 2 3 4 5 6 7
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * for the read test we cannot use "table2" because it has been appended
    * we use the original "table1" instead
    *-------------------------------------------------------------------------
    */
    if(do_write)
        strcpy(tname,"table2");
    else
        strcpy(tname,"table1");

    rstart=0;
    rrecords=8;
    if (H5TBread_records(fid,tname,rstart,rrecords,type_size_mem,field_offset,
        field_size,rbuf)<0)
        goto out;

    /* compare */
    for( i = rstart; i < rrecords; i++)
    {
        if (cmp_par(i,i,rbuf,wbuf)<0)
            goto out;
    }
    PASSED();

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBappend_records
    * H5TBread_records
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("appending records");

        /*-------------------------------------------------------------------------
        * append 2 records
        * pos = 0 1 2 3 4 5 6 7 8 9
        * data= 0 1 2 3 4 5 6 7 8 9
        *-------------------------------------------------------------------------
        */
        arecords=2;
        if (H5TBappend_records(fid,"table2",arecords,type_size_mem,field_offset,field_size,abuf)<0)
            return 1;

        if (H5TBget_table_info(fid,"table2",&rfields,&rrecords)<0)
            return 1;

        rstart=0; rrecords=NRECORDS+arecords;
        if (H5TBread_records(fid,"table2",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            return 1;

        /* compare */
        wrecords=8;
        for( i = rstart; i< wrecords; i++)
        {
            if (cmp_par(i,i,rbuf,wbuf)<0)
                goto out;
        }
        for( i = wrecords, j = 0; i < rrecords; i++,j++)
        {
            if (cmp_par(i,j,rbuf,abuf)<0)
                goto out;
        }
        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBinsert_record
    * H5TBread_records
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("inserting records");

        /*-------------------------------------------------------------------------
        * insert 2 records
        * pos = 0 1 2 3 4 5 6 7 8 9 10 11
        * data= 0 0 0 1 2 3 4 5 6 7 8  9
        *-------------------------------------------------------------------------
        */
        istart=1; irecords=2;
        if (H5TBinsert_record(fid,"table2",istart,irecords,type_size_mem,field_offset,field_size,ibuf)<0)
            return 1;

        if (H5TBget_table_info(fid,"table2",&rfields,&rrecords)<0)
            return 1;

        if (H5TBread_records(fid,"table2",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            return 1;

        /* compare */
        for( i = 0; i < 12; i++)
        {
            if (i < istart)
            {
                if (cmp_par(i,i,rbuf,wbuf)<0)
                    goto out;
            }
            else if (i >= istart && i < istart + irecords)
            {
                j = i - istart;
                if (cmp_par(i,j,rbuf,ibuf)<0)
                    goto out;
            }
            else if ( i >= istart + irecords && i < 10 )
            {
                j = i - irecords;
                if (cmp_par(i,j,rbuf,wbuf)<0)
                    goto out;
            }
            else
            {
                j = i - 10;
                if (cmp_par(i,j,rbuf,abuf)<0)
                    goto out;
            }
        }
        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBdelete_record
    * H5TBread_records
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("deleting records");

        /*-------------------------------------------------------------------------
        * Create a table
        * pos = 0 1 2 3 4 5 6 7
        * data= 0 1 2 3 4 5 6 7
        *-------------------------------------------------------------------------
        */

        for( i=0; i<NRECORDS; i++)
        {
            wbufd[i].lati = wbuf[i].lati;
            wbufd[i].longi = wbuf[i].longi;
            wbufd[i].pressure = wbuf[i].pressure;
            wbufd[i].temperature = wbuf[i].temperature;
            strcpy(wbufd[i].name, wbuf[i].name );

        }


        if (H5TBmake_table(TITLE,fid,"table3",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbufd)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * Delete records, start at 2, delete 3
        * pos = 0 1 2 3 4
        * data= 0 1 5 6 7
        *-------------------------------------------------------------------------
        */
        dstart=2; drecords=3;
        if (H5TBdelete_record(fid,"table3",dstart,drecords)<0)
            goto out;

        if (H5TBget_table_info(fid,"table3",&rfields,&rrecords)<0)
            goto out;

        rstart=0;
        if (H5TBread_records(fid,"table3",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            goto out;

        /* compare */
        nrecords=NRECORDS;
        assert(rrecords == nrecords-drecords);
        if (compare_deleted(rrecords,dstart,drecords,rbuf,wbufd)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * reset compare buffer
        *-------------------------------------------------------------------------
        */
        nrecords=rrecords;
        for( i=0; i<nrecords; i++)
        {
            wbufd[i] = rbuf[i];
        }

        /*-------------------------------------------------------------------------
        * Delete records, start at 0, delete 2
        * pos = 0 1 2
        * data= 5 6 7
        *-------------------------------------------------------------------------
        */
        dstart=0; drecords=2;
        if (H5TBdelete_record(fid,"table3",dstart,drecords)<0)
            goto out;

        if (H5TBget_table_info(fid,"table3",&rfields,&rrecords)<0)
            goto out;

        rstart=0;
        if (H5TBread_records(fid,"table3",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            goto out;

        /* Compare */
        assert(rrecords == nrecords-drecords);
        if (compare_deleted(rrecords,dstart,drecords,rbuf,wbufd)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * reset compare buffer
        *-------------------------------------------------------------------------
        */
        nrecords=rrecords;
        for( i=0; i<nrecords; i++)
        {
            wbufd[i] = rbuf[i];
        }

        /*-------------------------------------------------------------------------
        * Delete records, start at 1, delete 1
        * pos = 0 1
        * data= 5 7
        *-------------------------------------------------------------------------
        */
        dstart=1; drecords=1;
        if (H5TBdelete_record(fid,"table3",dstart,drecords)<0)
            goto out;

        if (H5TBget_table_info(fid,"table3",&rfields,&rrecords)<0)
            goto out;

        rstart=0;
        if (H5TBread_records(fid,"table3",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            goto out;

        /* Compare */
        assert(rrecords == nrecords-drecords);
        if (compare_deleted(rrecords,dstart,drecords,rbuf,wbufd)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * reset compare buffer
        *-------------------------------------------------------------------------
        */
        nrecords=rrecords;
        for( i=0; i<nrecords; i++)
        {
            wbufd[i] = rbuf[i];
        }

        /*-------------------------------------------------------------------------
        * Delete records, start at 0, delete 1
        * pos = 0
        * data= 7
        *-------------------------------------------------------------------------
        */
        dstart=0; drecords=1;
        if (H5TBdelete_record(fid,"table3",dstart,drecords)<0)
            goto out;

        if (H5TBget_table_info(fid,"table3",&rfields,&rrecords)<0)
            goto out;

        rstart=0;
        if (H5TBread_records(fid,"table3",rstart,rrecords,type_size_mem,field_offset,
            field_size,rbuf)<0)
            goto out;

        /* Compare */
        assert(rrecords == nrecords-drecords);
        if (compare_deleted(rrecords,dstart,drecords,rbuf,wbufd)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * reset compare buffer
        *-------------------------------------------------------------------------
        */
        nrecords=rrecords;
        for( i=0; i<nrecords; i++)
        {
            wbufd[i] = rbuf[i];
        }

        /* Read complete table */
        if (H5TBread_table(fid,"table3",type_size_mem,field_offset,field_size,rbuf)<0)
            goto out;

        /* Compare */
        for( i=0; i<rrecords; i++)
        {
            if (cmp_par(i,i,rbuf,wbufd)<0)
            {
                goto out;
            }
        }

        /*-------------------------------------------------------------------------
        * Delete records, start at 0, delete 1
        * pos = 0
        * data= empty
        *-------------------------------------------------------------------------
        */
        dstart=0; drecords=1;
        if (H5TBdelete_record(fid,"table3",dstart,drecords)<0)
            goto out;

        if (H5TBget_table_info(fid,"table3",&rfields,&rrecords)<0)
            goto out;

        if (rrecords)
            goto out;

        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBadd_records_from
    * H5TBread_records
    *
    *-------------------------------------------------------------------------
    */

    if (do_write)
    {
        TESTING2("adding records");

        /* create 2 tables */
        if (H5TBmake_table(TITLE,fid,"table4",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;
        if (H5TBmake_table(TITLE,fid,"table5",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;

        /* add the records from "table4" to "table5"  */
        start1    = 3;
        nrecords  = 2;
        start2    = 6;
        if ( H5TBadd_records_from(fid,"table4",start1,nrecords,"table5",start2)<0)
            goto out;

        /* read final table */
        if (H5TBread_table(fid,"table5",type_size_mem,field_offset,field_size,rbuf)<0)
            goto out;

        /* compare */
        for( i = 0; i < NRECORDS+2; i++ )
        {
            if ( i < start2 )
            {
                if (cmp_par(i,i,rbuf,wbuf)<0)
                    goto out;
            }
            else if ( i < start2 + nrecords )
            {
                j = i - start1;
                if (cmp_par(i,j,rbuf,wbuf)<0)
                    goto out;
            }
            else
            {
                j = i - nrecords;
                if (cmp_par(i,j,rbuf,wbuf)<0)
                    goto out;
            }
        }

        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBcombine_tables
    * H5TBread_table
    *
    *-------------------------------------------------------------------------
    */

    if (do_write)
    {
        TESTING2("combining tables");

        /* create 2 tables */
        if (H5TBmake_table(TITLE,fid,"table6",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;
        if (H5TBmake_table(TITLE,fid,"table7",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;

        /* combine the two tables into a third  */
        if ( H5TBcombine_tables(fid,"table6",fid,"table7","table8")<0)
            goto out;

        /* read merged table */
        if (H5TBread_table(fid,"table8",type_size_mem,field_offset,field_size,rbufc)<0)
            goto out;

        /* compare  */
        for( i = 0; i < NRECORDS*2; i++ )
        {
            if ( i < NRECORDS )
            {
                if (cmp_par(i,i,rbufc,wbuf)<0)
                    goto out;
            }
            else
            {
                if (cmp_par(i,i-NRECORDS,rbufc,wbuf)<0)
                    goto out;
            }
        }

        /*-------------------------------------------------------------------------
        * multi file test
        *-------------------------------------------------------------------------
        */

        /* create 2 files using default properties. */
        fid1 = H5Fcreate("combine_tables1.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
        fid2 = H5Fcreate("combine_tables2.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

        /* create 2 tables, one in each file */
        if (H5TBmake_table(TITLE,fid1,"table1",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;
        if (H5TBmake_table(TITLE,fid2,"table2",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;

        /* combine the two tables into a third  */
        if ( H5TBcombine_tables(fid1,"table1",fid2,"table2","table3")<0)
            goto out;

        /* read merged table */
        if (H5TBread_table(fid1,"table3",type_size_mem,field_offset,field_size,rbufc)<0)
            goto out;

        /* compare  */
        for( i = 0; i < NRECORDS*2; i++ )
        {
            if ( i < NRECORDS )
            {
                if (cmp_par(i,i,rbufc,wbuf)<0)
                    goto out;
            }
            else
            {
                if (cmp_par(i,i-NRECORDS,rbufc,wbuf)<0)
                    goto out;
            }
        }

        /* close files */
        H5Fclose(fid1);
        H5Fclose(fid2);

        PASSED();
    }


    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBwrite_fields_name
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("writing fields by name");

        /* make an empty table with fill values */
        if (H5TBmake_table(TITLE,fid,"table9",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill1,compress,0)<0)
            goto out;

        /* write the pressure field starting at record 2 */
        start    = 2;
        nrecords = NRECORDS_ADD;
        if (H5TBwrite_fields_name(fid,"table9","Pressure",start,nrecords,sizeof(float),
            0,field_sizes_pre,pressure_in)<0)
            goto out;

        /* write the new longitude and latitude information starting at record 2 */
        start    = 2;
        nrecords = 3;
        if (H5TBwrite_fields_name(fid,"table9","Latitude,Longitude",start,nrecords,sizeof(position_t),
            field_offset_pos,field_sizes_pos,position_in)<0)
            goto out;

        /* read back the all table */
        start    = 0;
        nrecords = NRECORDS;
        if (H5TBread_table(fid,"table9",type_size_mem,field_offset,field_size,rbuf)<0)
            goto out;

        {


            /* compare the read values with the initial values */
            for( i = 0; i < NRECORDS; i++ )
            {
                if ( i >= 2 && i <= 4 )
                {
                    if ( rbuf[i].lati        != position_in[i-NRECORDS_ADD+1].lati ||
                        rbuf[i].longi       != position_in[i-NRECORDS_ADD+1].longi ||
                        rbuf[i].pressure    != pressure_in[i-NRECORDS_ADD+1] )
                    {
                        fprintf(stderr,"%ld %f %d\n",
                            rbuf[i].longi,rbuf[i].pressure,rbuf[i].lati);
                        fprintf(stderr,"%ld %f %d\n",
                            position_in[i].longi,pressure_in[i],position_in[i].lati);
                        goto out;
                    }
                }
            }
        }



        PASSED();
    } /*write*/

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBread_fields_name
    *
    *-------------------------------------------------------------------------
    */
    TESTING2("reading fields by name");

    /*-------------------------------------------------------------------------
    * write and read the "Pressure" field
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        if (H5TBmake_table(TITLE,fid,"table10",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill1,compress,0)<0)
            goto out;

        /* write the pressure field to all the records */
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBwrite_fields_name(fid,"table10","Pressure",start,nrecords,
            sizeof( float ),0,field_sizes_pre,pressure_in)<0)
            goto out;
    }

    /* read the "Pressure" field */
    start    = 0;
    nrecords = NRECORDS;
    if ( H5TBread_fields_name(fid,"table10","Pressure",start,nrecords,
        sizeof(float),0,field_sizes_pre,pressure_out)<0)
        goto out;


    /* Compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS; i++ )
    {
        if ( pressure_out[i] != pressure_in[i] ) {
            goto out;
        }
    }

    /*-------------------------------------------------------------------------
    * Write and read the "Latitude,Longitude" fields
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        /* Write the new longitude and latitude information to all the records */
        start    = 0;
        nrecords = NRECORDS_ADD;
        if ( H5TBwrite_fields_name(fid,"table10", "Latitude,Longitude", start, nrecords,
            sizeof( position_t ), field_offset_pos, field_sizes_pos, position_in  ) < 0 )
            goto out;
    }/*write*/

    /* Read the "Latitude,Longitude" fields */
    start    = 0;
    nrecords = NRECORDS_ADD;
    if ( H5TBread_fields_name( fid, "table10", "Latitude,Longitude",
        start, nrecords, sizeof(position_t), field_offset_pos, field_sizes_pos, position_out ) < 0 )
        goto out;


    /* Compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS_ADD; i++ )
    {
        if ( position_out[i].lati  != position_in[i].lati ||
            position_out[i].longi != position_in[i].longi )
            goto out;
    }


    /*-------------------------------------------------------------------------
    * Write and read the "Name,Pressure" fields
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        /* Write the new name and pressure information to all the records */
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBwrite_fields_name( fid, "table10", "Name,Pressure", start, nrecords,
            sizeof( namepressure_t ), field_offset_namepre, field_sizes_namepre, namepre_in  ) < 0 )
            goto out;
    }/*write*/

    /* Read the "Name,Pressure" fields */
    start    = 0;
    nrecords = NRECORDS;
    if ( H5TBread_fields_name( fid, "table10", "Name,Pressure",
        start, nrecords, sizeof(namepressure_t), field_offset_namepre, field_sizes_namepre,
        namepre_out ) < 0 )
        goto out;


    /* Compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS; i++ )
    {
        if ( ( strcmp( namepre_out[i].name,  namepre_in[i].name ) != 0 ) ||
            namepre_out[i].pressure != namepre_in[i].pressure ) {
                goto out;
        }
    }

    /* reset buffer */
    for( i = 0; i < NRECORDS; i++ )
    {
        strcpy( namepre_out[i].name,  "\0" );
        namepre_out[i].pressure = -1;
    }

    /*-------------------------------------------------------------------------
    * read only 3 records of the "Name,Pressure" fields, starting at record 2
    *-------------------------------------------------------------------------
    */
    start    = 2;
    nrecords = 3;
    if ( H5TBread_fields_name( fid, "table10", "Name,Pressure",
        start, nrecords, sizeof(namepressure_t), field_offset_namepre,
        field_sizes_namepre, namepre_out ) < 0 )
        goto out;


    /* Compare the extracted table with the initial values */
    for( i = 0; i < 3; i++ )
    {
        hsize_t iistart = start;
        if ( ( strcmp( namepre_out[i].name,  namepre_in[iistart+i].name ) != 0 ) ||
            namepre_out[i].pressure != namepre_in[iistart+i].pressure ) {
                goto out;
        }
    }



    PASSED();

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBwrite_fields_index
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("writing fields by index");

        /* make an empty table */
        if (H5TBmake_table(TITLE,fid,"table11",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,NULL)<0)
            goto out;

        /* write the pressure field starting at record 2 */
        nfields  = 1;
        start    = 2;
        nrecords = NRECORDS_ADD;
        if ( H5TBwrite_fields_index(fid, "table11", nfields, field_index_pre, start, nrecords,
            sizeof( float ), 0, field_sizes_pre, pressure_in  ) < 0 )
            goto out;


        /* write the new longitude and latitude information starting at record 2  */
        nfields  = 2;
        start    = 2;
        nrecords = NRECORDS_ADD;
        if ( H5TBwrite_fields_index(fid, "table11", nfields, field_index_pos, start, nrecords,
            sizeof( position_t ), field_offset_pos, field_sizes_pos, position_in  ) < 0 )
            goto out;

        /* read back the all table */
        nfields  = 5;
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBread_fields_index(fid, "table11", nfields, field_index,
            start, nrecords, type_size_mem, field_offset, field_size, rbuf ) < 0 )
            goto out;

        /* Compare the extracted table with the initial values */
        for( i = 0; i < NRECORDS; i++ )
        {
            if ( i >= 2 && i <= 4 )
            {
                if ( rbuf[i].lati        != position_in[i-NRECORDS_ADD+1].lati ||
                    rbuf[i].longi       != position_in[i-NRECORDS_ADD+1].longi ||
                    rbuf[i].pressure    != pressure_in[i-NRECORDS_ADD+1] )
                    goto out;
            }
        }

        PASSED();
    }


    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBread_fields_index
    *
    *-------------------------------------------------------------------------
    */

    TESTING2("reading fields by index");

    if (do_write)
    {
        /* make an empty table */
        if (H5TBmake_table(TITLE,fid,"table12",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,NULL)<0)
            goto out;

        /*-------------------------------------------------------------------------
        * write and read the "Pressure" field
        *-------------------------------------------------------------------------
        */

        /* write the pressure field to all the records */
        nfields  = 1;
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBwrite_fields_index(fid, "table12", nfields, field_index_pre, start, nrecords,
            sizeof( float ), 0, field_sizes_pre, pressure_in  ) < 0 )
            goto out;
    }

    /* read the "Pressure" field */
    nfields  = 1;
    start    = 0;
    nrecords = NRECORDS;
    if ( H5TBread_fields_index(fid, "table12", nfields, field_index_pre,
        start, nrecords, sizeof(float), 0, field_sizes_pre, pressure_out ) < 0 )
        goto out;

    /* compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS; i++ )
    {
        if ( pressure_out[i] != pressure_in[i] ) {
            goto out;
        }
    }

    /*-------------------------------------------------------------------------
    * write and read the "Latitude,Longitude" fields
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        /* write the new longitude and latitude information to all the records */
        nfields = 2;
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBwrite_fields_index(fid, "table12", nfields, field_index_pos, start, nrecords,
            sizeof( position_t ), field_offset_pos, field_sizes_pos, position_in  ) < 0 )
            goto out;
    } /*write*/

    /* read the "Latitude,Longitude" fields */
    nfields = 2;
    start    = 0;
    nrecords = NRECORDS_ADD;
    if ( H5TBread_fields_index(fid, "table12", nfields, field_index_pos,
        start, nrecords, sizeof(position_t), field_offset_pos, field_sizes_pos, position_out ) < 0 )
        goto out;

    /* compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS_ADD; i++ )
    {
        if ( position_out[i].lati  != position_in[i].lati ||
            position_out[i].longi != position_in[i].longi ) {
                goto out;
        }
    }

    /*-------------------------------------------------------------------------
    * write and read the "Name,Pressure" fields
    *-------------------------------------------------------------------------
    */

    if (do_write)
    {
        /* write the new name and pressure information to all the records */
        nfields = 2;
        start    = 0;
        nrecords = NRECORDS;
        if ( H5TBwrite_fields_index(fid, "table12", nfields, field_index_namepre, start, nrecords,
            sizeof( namepressure_t ), field_offset_namepre, field_sizes_namepre, namepre_in  ) < 0 )
            goto out;
    }

    /* read the "Name,Pressure" fields */
    nfields = 2;
    start    = 0;
    nrecords = NRECORDS;
    if ( H5TBread_fields_index(fid, "table12", nfields, field_index_namepre,
        start, nrecords, sizeof(namepressure_t), field_offset_namepre,
        field_sizes_namepre, namepre_out ) < 0 )
        goto out;

    /* compare the extracted table with the initial values */
    for( i = 0; i < NRECORDS; i++ )
    {
        if ( ( strcmp( namepre_out[i].name,  namepre_in[i].name ) != 0 ) ||
            namepre_out[i].pressure != namepre_in[i].pressure ) {
                goto out;
        }
    }

    /* reset buffer */
    for( i = 0; i < NRECORDS; i++ )
    {
        strcpy( namepre_out[i].name,  "\0" );
        namepre_out[i].pressure = -1;
    }

    /*-------------------------------------------------------------------------
    * read only 3 records of the "Name,Pressure" fields, starting at record 2
    *-------------------------------------------------------------------------
    */

    /* write the new name and pressure information to all the records */
    nfields  = 2;
    start    = 2;
    nrecords = 3;
    if ( H5TBread_fields_index(fid, "table12", nfields, field_index_namepre,
        start, nrecords, sizeof(namepressure_t), field_offset_namepre,
        field_sizes_namepre, namepre_out ) < 0 )
        goto out;

    /* compare the extracted table with the initial values */
    for( i = 0; i < 3; i++ )
    {
        int iistart = (int) start;
        if ( ( strcmp( namepre_out[i].name,  wbuf[iistart+i].name ) != 0 ) ||
            namepre_out[i].pressure != wbuf[iistart+i].pressure ) {
                goto out;
        }
    }

    PASSED();


    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBinsert_field
    *
    *-------------------------------------------------------------------------
    */

    if (do_write)
    {
        TESTING2("inserting fields");

        /* make a table */
        if (H5TBmake_table(TITLE,fid,"table13",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill1,compress,wbuf)<0)
            goto out;

        /* insert the new field at the end of the field list */
        position = NFIELDS;
        if ( H5TBinsert_field( fid, "table13", "New Field", field_type_new, position,
            fill1_new, buf_new ) < 0 )
            goto out;

        /* read the table */
        if ( H5TBread_table( fid, "table13", dst_size2, dst_offset2, dst_sizes2, rbuf2 ) < 0 )
            goto out;

        /* compare the extracted table with the original array */
        for( i = 0; i < NRECORDS; i++ )
        {
            if ( ( strcmp( rbuf2[i].name,  wbuf[i].name ) != 0 ) ||
                rbuf2[i].lati          != wbuf[i].lati ||
                rbuf2[i].longi         != wbuf[i].longi ||
                rbuf2[i].pressure      != wbuf[i].pressure ||
                rbuf2[i].temperature   != wbuf[i].temperature ||
                rbuf2[i].new_field     != buf_new[i] ) {
                    goto out;
            }
        }
        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBdelete_field
    *
    *-------------------------------------------------------------------------
    */
    if (do_write)
    {
        TESTING2("deleting fields");

        /* make a table */
        if (H5TBmake_table(TITLE,fid,"table14",FIELDS,RECORDS,type_size_mem,
            field_names,field_offset,field_type,
            chunk_size,fill,compress,wbuf)<0)
            goto out;

        /* delete the field */
        if ( H5TBdelete_field(fid, "table14", "Pressure" ) < 0 )
            goto out;

        /* read the table */
        if ( H5TBread_table(fid, "table14", dst_size3, dst_offset3, dst_sizes3, rbuf3 ) < 0 )
            goto out;

        /* compare the extracted table with the original array */
        for( i = 0; i < NRECORDS; i++ )
        {
            if ( ( strcmp( rbuf3[i].name, wbuf[i].name ) != 0 ) ||
                rbuf3[i].lati != wbuf[i].lati ||
                rbuf3[i].longi != wbuf[i].longi ||
                rbuf3[i].temperature != wbuf[i].temperature ) {
                    goto out;
            }
        }

        PASSED();
    }

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBget_table_info
    * H5TBget_field_info
    *
    *-------------------------------------------------------------------------
    */

    TESTING2("getting table info");

    /* get table info  */
    if ( H5TBget_table_info (fid, "table1", &rfields, &rrecords ) < 0 )
        goto out;

    if ( NFIELDS != rfields || rrecords != NRECORDS  ) {
        goto out;
    }

    PASSED();

    /*-------------------------------------------------------------------------
    *
    * Functions tested:
    *
    * H5TBget_field_info
    *
    *-------------------------------------------------------------------------
    */

    TESTING2("getting field info");

    /* alocate */
    names_out = (char**) malloc( sizeof(char*) * (size_t)NFIELDS );
    for ( i = 0; i < NFIELDS; i++)
    {
        names_out[i] = (char*) malloc( sizeof(char) * 255 );
    }

    /* Get field info */
    if ( H5TBget_field_info(fid, "table1", names_out, sizes_out, offset_out, &size_out ) < 0 )
        goto out;

    for ( i = 0; i < NFIELDS; i++)
    {
        if ( (strcmp( field_names[i], names_out[i] ) != 0)) {
            goto out;
        }
    }

    /* release */
    for ( i = 0; i < NFIELDS; i++)
    {
        free ( names_out[i] );
    }
    free ( names_out );

    PASSED();

    /*-------------------------------------------------------------------------
    * end
    *-------------------------------------------------------------------------
    */
    return 0;
out:
    H5_FAILED();
    return -1;
}


/*-------------------------------------------------------------------------
* the main program
*-------------------------------------------------------------------------
*/

int main(void)
{
    hid_t    fid;  /* identifier for the file */
    unsigned flags=H5F_ACC_RDONLY;

    /*-------------------------------------------------------------------------
    * test1: create a file for the write/read test
    *-------------------------------------------------------------------------
    */

    /* create a file using default properties */
    fid=H5Fcreate("test_table.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

    puts("Testing table with file creation mode (read/write in native architecture):");

    /* test, do write */
    if (test_table(fid,1)<0)
        goto out;

    /* close */
    H5Fclose(fid);

    /*-------------------------------------------------------------------------
    * test2: open a file written in test1 on a big-endian machine
    *-------------------------------------------------------------------------
    */
    puts("Testing table with file open mode (read big-endian data):");

    fid=h5file_open(TEST_FILE_BE,flags);

    /* test, do not write */
    if (test_table(fid,0)<0)
        goto out;

    /* close */
    H5Fclose(fid);

    /*-------------------------------------------------------------------------
    * test3: open a file written in test1 on a little-endian machine
    *-------------------------------------------------------------------------
    */
    puts("Testing table with file open mode (read little-endian data):");

    fid=h5file_open(TEST_FILE_LE,flags);

    /* test, do not write */
    if (test_table(fid,0)<0)
        goto out;

    /* close */
    H5Fclose(fid);

    /*-------------------------------------------------------------------------
    * test4: open a file written in test1 on the Cray T3 machine
    *-------------------------------------------------------------------------
    */
    puts("Testing table with file open mode (read Cray data):");

    fid=h5file_open(TEST_FILE_CRAY,flags);

    /* test, do not write */
    if (test_table(fid,0)<0)
        goto out;

    /* close */
    H5Fclose(fid);

    return 0;
out:
    H5Fclose(fid);
    return 1;
}


