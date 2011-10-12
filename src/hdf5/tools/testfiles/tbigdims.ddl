#############################
Expected output for 'h5dump -d dset4gb -s 4294967284 -c 22 tbigdims.h5'
#############################
HDF5 "tbigdims.h5" {
DATASET "dset4gb" {
   DATATYPE  H5T_STD_I8LE
   DATASPACE  SIMPLE { ( 4294967306 ) / ( 4294967306 ) }
   SUBSET {
      START ( 4294967284 );
      STRIDE ( 1 );
      COUNT ( 22 );
      BLOCK ( 1 );
      DATA {
      (4294967284): 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
      (4294967301): 15, 16, 17, 18, 19
      }
   }
}
}
