#############################
Expected output for 'h5dump -H -p -d external tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "external" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 100 ) / ( 100 ) }
   STORAGE_LAYOUT {
      CONTIGUOUS
      EXTERNAL {
         FILENAME ext1.bin SIZE 200 OFFSET 0
         FILENAME ext2.bin SIZE 200 OFFSET 0
      }
   }
   FILTERS {
      NONE
   }
   FILLVALUE {
      FILL_TIME H5D_FILL_TIME_IFSET
      VALUE  0   
   }
   ALLOCATION_TIME {
      H5D_ALLOC_TIME_LATE
   }
}
}
