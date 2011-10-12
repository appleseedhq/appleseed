#############################
Expected output for 'h5dump -d /dset1[1,1;;;] tdset.h5'
#############################
HDF5 "tdset.h5" {
DATASET "/dset1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 20 ) / ( 10, 20 ) }
   SUBSET {
      START ( 1, 1 );
      STRIDE ( 1, 1 );
      COUNT ( 1, 1 );
      BLOCK ( 1, 1 );
      DATA {
      (1,1): 2
      }
   }
}
}
