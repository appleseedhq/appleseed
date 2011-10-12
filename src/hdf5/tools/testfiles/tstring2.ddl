#############################
Expected output for 'h5dump -r -d str4 tstr3.h5'
#############################
HDF5 "tstr3.h5" {
DATASET "str4" {
   DATATYPE  H5T_STD_I8LE
   DATASPACE  SIMPLE { ( 93 ) / ( 93 ) }
   DATA {
         "Four score and seven
            years ago our forefathers brought fo"
         "rth on this continent a new nation\000"
   }
}
}
