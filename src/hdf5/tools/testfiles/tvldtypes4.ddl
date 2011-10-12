#############################
Expected output for 'h5dump tvldtypes4.h5'
#############################
HDF5 "tvldtypes4.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_VLEN { H5T_COMPOUND {
         H5T_STD_I32LE "i";
         H5T_IEEE_F32LE "f";
      }}
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): ({
               0,
               0
            }),
      (1): ({
               10,
               6.66667
            }, {
               11,
               7
            }),
      (2): ({
               20,
               13.3333
            }, {
               21,
               13.6667
            }, {
               22,
               14
            }),
      (3): ({
               30,
               20
            }, {
               31,
               20.3333
            }, {
               32,
               20.6667
            }, {
               33,
               21
            })
      }
   }
}
}
