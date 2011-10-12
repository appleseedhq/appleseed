#############################
Expected output for 'h5dump tarray4.h5'
#############################
HDF5 "tarray4.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_ARRAY { [4] H5T_COMPOUND {
         H5T_STD_I32LE "i";
         H5T_IEEE_F32LE "f";
      } }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): [ {
               0,
               0
            }, {
               1,
               1
            }, {
               2,
               2
            }, {
               3,
               3
            } ],
      (1): [ {
               10,
               2.5
            }, {
               11,
               3.5
            }, {
               12,
               4.5
            }, {
               13,
               5.5
            } ],
      (2): [ {
               20,
               5
            }, {
               21,
               6
            }, {
               22,
               7
            }, {
               23,
               8
            } ],
      (3): [ {
               30,
               7.5
            }, {
               31,
               8.5
            }, {
               32,
               9.5
            }, {
               33,
               10.5
            } ]
      }
   }
}
}
