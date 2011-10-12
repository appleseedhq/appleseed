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
#include "hdf5_hl.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Table API example
 *
 * H5TBwrite_fields_index
 *
 *-------------------------------------------------------------------------
 */

#define NFIELDS       (hsize_t)   5
#define NRECORDS      (hsize_t)   8
#define NRECORDS_ADD  (hsize_t)   3
#define TABLE_NAME               "table"



int main( void )
{
 typedef struct Particle
 {
  char   name[16];
  int    lati;
  int    longi;
  float  pressure;
  double temperature;
 } Particle;

 /* Define a subset of Particle, with latitude and longitude fields */
 typedef struct Position
 {
  int    lati;
  int    longi;
 } Position;

 /* Define a subset of Particle, with name and pressure fields */
 typedef struct NamePressure
 {
  char   name[16];
  float  pressure;
 } NamePressure;

 /* Calculate the type_size and the offsets of our struct members */
 Particle  dst_buf[NRECORDS];
 size_t dst_size =  sizeof( Particle );
 size_t dst_offset[NFIELDS] = { HOFFSET( Particle, name ),
                                HOFFSET( Particle, lati ),
                                HOFFSET( Particle, longi ),
                                HOFFSET( Particle, pressure ),
                                HOFFSET( Particle, temperature )};
 size_t dst_sizes[NFIELDS] = { sizeof( dst_buf[0].name),
                               sizeof( dst_buf[0].lati),
                               sizeof( dst_buf[0].longi),
                               sizeof( dst_buf[0].pressure),
                               sizeof( dst_buf[0].temperature)};

 size_t field_offset_pos[2] = { HOFFSET( Position, lati ),
                                HOFFSET( Position, longi )};

 /* Initially no data */
 Particle  *p_data = NULL;

 /* Define field information */
 const char *field_names[NFIELDS]  =
 { "Name","Latitude", "Longitude", "Pressure", "Temperature" };
 hid_t      field_type[NFIELDS];
 hid_t      string_type;
 hid_t      file_id;
 hsize_t    chunk_size = 10;
  Particle   fill_data[1] =
 { {"no data",-1,-1, -99.0f, -99.0} };   /* Fill value particle */
 int        compress  = 0;
 hsize_t    nfields;
 hsize_t    start;                       /* Record to start reading/writing */
 hsize_t    nrecords;                    /* Number of records to read/write */
 herr_t     status;
 int        i;

 /* Define new values for the field "Pressure"  */
 float      pressure_in  [NRECORDS_ADD] =
 { 0.0f,1.0f,2.0f};
 int        field_index_pre[1]     = { 3 };
 int        field_index_pos[2]     = { 1,2 };

 /* Define new values for the fields "Latitude,Longitude"  */
 Position   position_in[NRECORDS_ADD] = { {0,0},
 {10,10},
 {20,20} };

 size_t field_sizes_pos[2]=
 {
  sizeof(position_in[0].longi),
  sizeof(position_in[0].lati)
 };

 size_t field_sizes_pre[1]=
 {
  sizeof(float)
 };

 /* Initialize the field field_type */
 string_type = H5Tcopy( H5T_C_S1 );
 H5Tset_size( string_type, 16 );
 field_type[0] = string_type;
 field_type[1] = H5T_NATIVE_INT;
 field_type[2] = H5T_NATIVE_INT;
 field_type[3] = H5T_NATIVE_FLOAT;
 field_type[4] = H5T_NATIVE_DOUBLE;

 /* Create a new file using default properties. */
 file_id = H5Fcreate( "ex_table_05.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

 /* Make the table */
 status=H5TBmake_table( "Table Title", file_id, TABLE_NAME,NFIELDS,NRECORDS,
                         dst_size,field_names, dst_offset, field_type,
                         chunk_size, fill_data, compress, p_data  );

 /* Write the pressure field starting at record 2 */
 nfields  = 1;
 start    = 2;
 nrecords = NRECORDS_ADD;
 status=H5TBwrite_fields_index( file_id, TABLE_NAME, nfields, field_index_pre, start, nrecords,
   sizeof( float ), 0, field_sizes_pre, pressure_in  );


 /* Write the new longitude and latitude information starting at record 2  */
 nfields  = 2;
 start    = 2;
 nrecords = NRECORDS_ADD;
 status=H5TBwrite_fields_index( file_id, TABLE_NAME, nfields, field_index_pos, start, nrecords,
   sizeof( Position ), field_offset_pos, field_sizes_pos, position_in  );


 /* read the table */
 status=H5TBread_table( file_id, TABLE_NAME, dst_size, dst_offset, dst_sizes, dst_buf );

 /* print it by rows */
 for (i=0; i<NRECORDS; i++) {
  printf ("%-5s %-5d %-5d %-5f %-5f",
   dst_buf[i].name,
   dst_buf[i].lati,
   dst_buf[i].longi,
   dst_buf[i].pressure,
   dst_buf[i].temperature);
  printf ("\n");
 }


  /* close type */
 H5Tclose( string_type );

 /* close the file */
 H5Fclose( file_id );

 return 0;

}

