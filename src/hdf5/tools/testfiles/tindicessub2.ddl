#############################
Expected output for 'h5dump -d 2d -s 1,2 -S 3,3 -c 3,2 -k 2,2 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "2d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   SUBSET {
      START ( 1, 2 );
      STRIDE ( 3, 3 );
      COUNT ( 3, 2 );
      BLOCK ( 2, 2 );
      DATA {
      (1,2): 12, 13, 15, 16,
      (2,2): 22, 23, 25, 26,
      (4,2): 42, 43, 45, 46,
      (5,2): 52, 53, 55, 56,
      (7,2): 72, 73, 75, 76,
      (8,2): 82, 83, 85, 86
      }
   }
}
}
