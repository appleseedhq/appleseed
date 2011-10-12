#############################
Expected output for 'h5dump -H -p -d chunked tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "chunked" {
COMMENT "This is a dataset with chunked storage"
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800
    }
   FILTERS {
      NONE
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
