#############################
Expected output for 'h5dump -H -p -d szip tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "szip" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 372 (2.151:1 COMPRESSION)
    }
   FILTERS {
      COMPRESSION SZIP {
         PIXELS_PER_BLOCK 4
         MODE K13
         CODING NEAREST NEIGHBOUR
         BYTE_ORDER LSB
         HEADER RAW
      }
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
