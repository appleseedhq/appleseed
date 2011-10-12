#############################
Expected output for 'h5dump -d /g1/g1.1/dset1.1.2[0;2;10;] tall.h5'
#############################
HDF5 "tall.h5" {
DATASET "/g1/g1.1/dset1.1.2" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 20 ) / ( 20 ) }
   SUBSET {
      START ( 0 );
      STRIDE ( 2 );
      COUNT ( 10 );
      BLOCK ( 1 );
      DATA {
      (0): 0, 2, 4, 6, 8, 10, 12, 14, 16, 18
      }
   }
}
}
