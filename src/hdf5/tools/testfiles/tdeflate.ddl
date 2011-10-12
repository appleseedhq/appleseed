#############################
Expected output for 'h5dump -H -p -d deflate tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "deflate" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 385 (2.078:1 COMPRESSION)
    }
   FILTERS {
      COMPRESSION DEFLATE { LEVEL 9 }
   }
   FILLVALUE {
      FILL_TIME H5D_FILL_TIME_IFSET
      VALUE  0   
   }
   ALLOCATION_TIME {
      H5D_ALLOC_TIME_INCR
   }
}
}
