HDF5 "tdatareg.h5" {
DATASET "/Dataset1" {
   DATATYPE  H5T_REFERENCE { H5T_STD_REF_DSETREG }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
   SUBSET {
      START ( 0 );
      STRIDE ( 1 );
      COUNT ( 1 );
      BLOCK ( 1 );
      DATA {
         DATASET /Dataset2 {
            REGION_TYPE BLOCK  (2,2)-(7,7)
            DATATYPE  H5T_STD_U8BE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            DATA { 
             } 
         }
      }
   }
}
}
