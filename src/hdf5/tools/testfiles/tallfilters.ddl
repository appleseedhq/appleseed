#############################
Expected output for 'h5dump -H -p -d all tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "all" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 458 (1.747:1 COMPRESSION)
    }
   FILTERS {
      PREPROCESSING SHUFFLE
      COMPRESSION SZIP {
         PIXELS_PER_BLOCK 4
         MODE K13
         CODING ENTROPY
         BYTE_ORDER LSB
         HEADER RAW
      }
      COMPRESSION DEFLATE { LEVEL 5 }
      CHECKSUM FLETCHER32
      COMPRESSION NBIT
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
