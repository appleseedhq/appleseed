#############################
Expected output for 'h5dump tvldtypes3.h5'
#############################
HDF5 "tvldtypes3.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "i";
         H5T_IEEE_F32LE "f";
         H5T_VLEN { H5T_STD_U32LE} "v";
      }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): {
            0,
            0,
            (0)
         },
      (1): {
            10,
            6.66667,
            (10, 11)
         },
      (2): {
            20,
            13.3333,
            (20, 21, 22)
         },
      (3): {
            30,
            20,
            (30, 31, 32, 33)
         }
      }
   }
}
}
