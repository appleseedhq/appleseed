HDF5 "tall.h5" {
DATASET "/g1/g1.1/dset1.1.1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   DATA {
   }
   ATTRIBUTE "attr1" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
      DATA {
         49, 115, 116, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101, 32, 111,
         102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
      }
   }
   ATTRIBUTE "attr2" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
      DATA {
         50, 110, 100, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101, 32, 111,
         102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
      }
   }
}
}
