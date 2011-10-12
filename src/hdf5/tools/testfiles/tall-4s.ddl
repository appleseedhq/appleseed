#############################
Expected output for 'h5dump --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5'
#############################
HDF5 "tall.h5" {
DATASET "/g1/g1.1/dset1.1.1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   SUBSET {
      START ( 1, 1 );
      STRIDE ( 2, 3 );
      COUNT ( 3, 2 );
      BLOCK ( 1, 1 );
      DATA {
      (1,1): 1, 4,
      (3,1): 3, 12,
      (5,1): 5, 20
      }
   }
   ATTRIBUTE "attr1" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
      DATA {
      (0): 49, 115, 116, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101, 32,
      (14): 111, 102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
      }
   }
   ATTRIBUTE "attr2" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
      DATA {
      (0): 50, 110, 100, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101, 32,
      (14): 111, 102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
      }
   }
}
}
