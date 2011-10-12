#############################
Expected output for 'h5dump -p tfvalues.h5'
#############################
HDF5 "tfvalues.h5" {
GROUP "/" {
   DATASET "fill_array" {
      DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 24
         OFFSET 1920
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  [ 0, 0, 0 ]      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): [ 1, 2, 3 ], [ 4, 5, 6 ]
      }
   }
   DATASET "fill_compound" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I8LE "a";
         H5T_IEEE_F64LE "b";
      }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 24
         OFFSET 1864
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  {
         1,
         2
      }      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): {
            1,
            2
         },
      (1): {
            3,
            4
         }
      }
   }
   DATASET "fill_time_alloc" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 8
         OFFSET 1848
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_ALLOC
         VALUE  -99      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): 1, 2
      }
   }
   DATASET "fill_time_ifset" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 8
         OFFSET 2048
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  -99      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): 1, 2
      }
   }
   DATASET "fill_time_never" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 8
         OFFSET 2056
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_NEVER
         VALUE  -99      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): 1, 2
      }
   }
   DATASET "fill_vlen" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 32
         OFFSET 1888
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_ALLOC
         VALUE  ()      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
      DATA {
      (0): (1), (2, 3)
      }
   }
   DATASET "no_fill" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 8
         OFFSET 1856
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
      DATA {
      (0): 1, 2
      }
   }
}
}
