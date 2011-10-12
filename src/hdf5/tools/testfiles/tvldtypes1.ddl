#############################
Expected output for 'h5dump tvldtypes1.h5'
#############################
HDF5 "tvldtypes1.h5" {
GROUP "/" {
   DATASET "Dataset1.0" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): (0), (10, 11), (20, 21, 22), (30, 31, 32, 33)
      }
   }
   DATASET "Dataset2.0" {
      DATATYPE  H5T_VLEN { H5T_IEEE_F32LE}
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): (0), (10, 10.1), (20, 20.1, 20.2), (30, 30.1, 30.2, 30.3)
      }
   }
   DATASET "Dataset3.0" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SCALAR
      DATA {
      (0): (0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72)
      }
   }
}
}
