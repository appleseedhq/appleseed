#############################
Expected output for 'h5dump tempty.h5'
#############################
HDF5 "tempty.h5" {
GROUP "/" {
   DATASET "Dataset1.0" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): (), (), (), ()
      }
   }
   DATASET "Dataset2.0" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 0, 0, 0, 0
      }
   }
   DATASET "Dataset3.0" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 0, 0, 0, 0
      }
   }
   DATASET "Dataset4.0" {
      DATATYPE  H5T_ARRAY { [4] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ]
      }
   }
   DATASET "Dataset5.0" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a";
         H5T_IEEE_F32LE "b";
         H5T_STD_I8LE "c";
      }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): {
            0,
            0,
            0
         },
      (1): {
            0,
            0,
            0
         },
      (2): {
            0,
            0,
            0
         },
      (3): {
            0,
            0,
            0
         }
      }
   }
}
}
